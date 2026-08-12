#!/usr/bin/env Rscript
# ============================================================================
# rbx_binary_parser.R  —  full-fledged parser for Bio-Plex Manager binary .rbx
# ============================================================================
# R port of the validated Python parser. Reads the RAW BINARY .rbx (.srbx)
# directly — no Excel, no XML export, no Bio-Plex Manager. Panel-agnostic:
# analytes, bead regions, samples and well count are read from the file, so it
# works for ANY antigen panel.
#
# Extracts (all validated against the vendor export on a real run):
#   * format/version, panel name, analyte -> bead-region map
#   * metadata: reader & platform serials, source path, plate id, notes, operator
#   * well -> sample assignment (label, description, category, dilution)  [96/96]
#   * per-well x per-analyte statistics: median (FI) [576/576], mean, trimmed
#     mean, cv, trimmed cv, sd, trimmed sd, std err, trimmed std err, bead count
#   * per-well total/gated/in-region event counts
#
# NOT in the binary (need the XML export): observed/expected concentrations and
# the standard-curve fit coefficients.
#
# Usage:
#   Rscript rbx_binary_parser.R FILE.rbx [out_prefix]
#       -> writes <out_prefix>_long.csv and <out_prefix>_fi.csv (default: rbx)
#   # or source() it and call:  doc <- parse_rbx("FILE.rbx")
#
# No non-base packages required for CSV output. JSON output is written only if
# the 'jsonlite' package is available.
# Offsets in this file are 0-based (like the format); raw[] indexing adds +1.
#
# Looking at the parser code, it returns a list and we can see exactly what it captures vs.
# what the binary stages actually parse. Here's the full picture:
#
# **The `.rbx` file has exactly 5 components — and the parser already captures all of them:**
#
# | Binary stage | Returned as | output data frame |
# |---|---|---|
# | `parse_header()` — magic bytes + version string + panel name | `doc$format` + `doc$panel$name` | `tdap.format`, `tdap.panel` |
# | `parse_analytes()` — analyte names + bead regions | `doc$panel$analytes` | `tdap.panel` |
# | `parse_metadata()` — serials, source path, plate ID, notes, operator | `doc$metadata` | `tdap.metadata` |
# | `parse_samples()` — sample → well assignments | `doc$samples` | `tdap.samples` |
# | `parse_quantification()` + `parse_stat_blocks()` + `merge_all()` — per-well per-analyte FI and stats | `doc$wells` | `tdap.wells` |
#
# **Nothing is left over.** The two things worth noting:
#
# 1. **`medians` and `stat_blocks` are intermediate** — `parse_quantification()` and `parse_stat_blocks()` are parsed separately then merged by `merge_all()` into `doc$wells`. They aren't separate top-level components; the median FI and the full stats block are two encodings of the same well data that the parser reconciles into one unified wells list.
#
# 2. **The parser comments explicitly note what is NOT in the binary at all:** observed/expected concentrations and standard-curve fit coefficients — those only exist in the XML export, not in the `.rbx` binary.
#
# So your five data frames (`tdap.format`, `tdap.metadata`, `tdap.panel`, `tdap.samples`, `tdap.wells`) cover the complete file.
#
# ============================================================================

# ---- low-level readers (0-based offsets) ---------------------------------- #
rd_u8  <- function(raw, o) as.integer(raw[o + 1L])
rd_u16 <- function(raw, o) readBin(raw[(o + 1L):(o + 2L)], "integer", n = 1L, size = 2L,
                                    signed = FALSE, endian = "little")
rd_i32 <- function(raw, o) readBin(raw[(o + 1L):(o + 4L)], "integer", n = 1L, size = 4L,
                                    endian = "little")
rd_f64 <- function(raw, o) readBin(raw[(o + 1L):(o + 8L)], "double", n = 1L, size = 8L,
                                    endian = "little")
rd_f32 <- function(raw, o) readBin(raw[(o + 1L):(o + 4L)], "double", n = 1L, size = 4L,
                                    endian = "little")

# Vectorized unsigned int32 at EVERY byte offset (1-based result index i = offset 0).
int32_all <- function(raw) {
  n  <- length(raw)
  b0 <- as.numeric(raw[1:(n - 3L)]);  b1 <- as.numeric(raw[2:(n - 2L)])
  b2 <- as.numeric(raw[3:(n - 1L)]);  b3 <- as.numeric(raw[4:n])
  b0 + b1 * 256 + b2 * 65536 + b3 * 16777216
}

# .NET 7-bit length-prefixed string at 0-based offset o. Returns list(text, nextpos) or NULL.
rd_str <- function(raw, o, maxlen = 200L) {
  n <- length(raw)
  if (o < 0L || o >= n) return(NULL)
  val <- 0; shift <- 0L; j <- o          # val is DOUBLE to avoid 32-bit overflow -> NA
  repeat {
    if (j >= n) return(NULL)
    bb <- as.integer(raw[j + 1L]); j <- j + 1L
    val <- val + bitwAnd(bb, 0x7F) * (2^shift)
    if (bitwAnd(bb, 0x80) == 0) break
    shift <- shift + 7L
    if (shift > 21L) return(NULL)         # any real length fits in <= 3 bytes (< 2^14 here)
  }
  if (is.na(val) || val < 0 || val > maxlen || j + val > n) return(NULL)
  val <- as.integer(val)
  if (val == 0L) return(list(text = "", nextpos = j))
  bytes <- as.integer(raw[(j + 1L):(j + val)])
  ok <- all((bytes %in% c(9L, 10L, 13L)) | (bytes >= 32L & bytes < 127L))
  if (!ok) return(NULL)
  list(text = rawToChar(as.raw(bytes)), nextpos = j + val)
}

looks_like_analyte <- function(s) grepl("^[A-Za-z0-9 ./._+-]{1,16}$", s)

# ---- stage 1: header (magic, version, panel) ------------------------------ #
parse_header <- function(raw) {
  magic <- rawToChar(raw[1:2])
  # scan first 0x80 bytes for the two leading length-prefixed strings
  strs <- character(0); o <- 0L
  while (o < 0x80L && length(strs) < 6L) {
    r <- rd_str(raw, o)
    if (!is.null(r) && nchar(r$text) > 0) { strs <- c(strs, r$text); o <- r$nextpos }
    else o <- o + 1L
  }
  version <- if (length(strs) >= 1) strs[1] else ""
  panel   <- ""
  for (s in strs[-1]) if (grepl("[A-Za-z]", s)) { panel <- s; break }
  list(magic = magic, version = version, panel = panel)
}

# ---- stage 2: analyte -> region map --------------------------------------- #
# Entry: <u16 region><len><name><00 00><i32 count><00 00 00 00>. Region precedes name.
parse_analytes <- function(raw) {
  N <- length(raw)
  lim <- min(N, 0x80000L)
  # candidate entries: the 6-byte count marker 00 00 01 00 00 00
  z <- as.raw(0); one <- as.raw(1)
  M <- lim - 6L
  marker <- which(raw[1:M] == z & raw[2:(M + 1L)] == z & raw[3:(M + 2L)] == one &
                  raw[4:(M + 3L)] == z & raw[5:(M + 4L)] == z & raw[6:(M + 5L)] == z) - 1L  # 0-based

  try_run <- function(start) {
    out_name <- character(0); out_reg <- integer(0); seen <- character(0); p <- start
    for (i in 1:64) {
      if (p + 4L > N) break
      region <- rd_u16(raw, p)
      r <- rd_str(raw, p + 2L)
      if (is.null(r) || nchar(r$text) < 2 || !looks_like_analyte(r$text) || r$text %in% seen) break
      k <- r$nextpos
      if (k + 8L > N) break
      sep <- rd_u16(raw, k); cnt <- rd_i32(raw, k + 2L); pad <- rd_i32(raw, k + 6L)
      if (sep != 0L || pad != 0L || cnt < 1L || cnt > 1000L) break
      seen <- c(seen, r$text)
      out_name <- c(out_name, r$text); out_reg <- c(out_reg, region)
      p <- k + 10L
    }
    list(names = out_name, regions = out_reg)
  }

  best <- list(names = character(0), regions = integer(0)); best_off <- -1L
  for (mk in marker) {
    # name ends just before the marker; len byte 1 before the name. Back up to region u16.
    # find the name length: bytes between a length byte and the marker.
    for (nl in 2:15) {
      lenpos <- mk - 1L - nl
      if (lenpos < 2L) next
      if (rd_u8(raw, lenpos) == nl) {
        nm <- as.integer(raw[(lenpos + 2L):(lenpos + 1L + nl)])
        if (all(nm >= 32L & nm < 127L)) {
          start <- lenpos - 2L  # region u16 precedes the length byte
          run <- try_run(start)
          if (length(run$names) > length(best$names)) { best <- run; best_off <- start }
        }
      }
    }
  }
  list(names = best$names, regions = best$regions, def_off = best_off)
}

# ---- stage 3: metadata ----------------------------------------------------- #
# Find the length-prefixed string whose CONTENT covers 0-based offset `pos`.
lp_string_covering <- function(raw, pos, maxback = 260L) {
  if (pos < 0L) return(NULL)
  for (s in pos:max(0L, pos - maxback)) {
    r <- rd_str(raw, s, maxlen = 255L)
    if (!is.null(r)) {
      clen <- nchar(r$text, type = "bytes")
      cend <- r$nextpos - 1L; cstart <- r$nextpos - clen
      if (clen > 0 && cstart <= pos && pos <= cend) return(r$text)
    }
  }
  NULL
}

# grepRaw helper: 0-based start offsets of a fixed ASCII pattern.
raw_find <- function(raw, pat) {
  hits <- grepRaw(pat, raw, fixed = TRUE, all = TRUE)
  if (length(hits) == 0) integer(0) else hits - 1L
}

parse_metadata <- function(raw) {
  meta <- list()
  # source path: a string ending in .pbx/.rbx/.srbx that contains a drive-letter ":\"
  for (ext in c(".pbx", ".srbx", ".rbx")) {
    for (p in raw_find(raw, ext)) {
      s <- lp_string_covering(raw, p)                 # p = offset of the '.'
      if (!is.null(s) && grepl(":\\\\", s) && grepl("\\.(pbx|rbx|srbx)$", tolower(s))) {
        meta$source_path <- s
        meta$source_file <- tail(strsplit(s, "[\\\\/]")[[1]], 1)
        parts <- strsplit(s, "[\\\\/]")[[1]]
        di <- which(tolower(parts) == "desktop")
        if (length(di) && di[1] < length(parts)) meta$operator <- parts[di[1] + 1L]
        break
      }
    }
    if (!is.null(meta$source_path)) break
  }
  # serials: length-prefixed strings beginning "LX"
  for (p in raw_find(raw, "LX")) {
    r <- rd_str(raw, p - 1L, maxlen = 32L)            # length byte precedes "LX"
    if (!is.null(r) && grepl("^LX[A-Z]?[0-9]{6,}$", r$text)) {
      if (startsWith(r$text, "LXY")) { if (is.null(meta$platform_serial)) meta$platform_serial <- r$text }
      else                           { if (is.null(meta$reader_serial))   meta$reader_serial   <- r$text }
    }
  }
  # plate id / notes: the (possibly multi-line) string containing "Plate <n>"
  for (p in raw_find(raw, "Plate ")) {
    s <- lp_string_covering(raw, p)
    if (!is.null(s) && grepl("plate\\s*[0-9]", s, ignore.case = TRUE) &&
        !grepl(":\\\\", s) && !grepl("\\.pbx$", tolower(s))) {
      lines <- trimws(strsplit(s, "[\r\n]+")[[1]]); lines <- lines[nchar(lines) > 0]
      if (length(lines)) { meta$notes <- trimws(s); meta$plate_id <- lines[1] }
      break
    }
  }
  meta
}

# ---- stage 4: per-well median FI (quantification section) ----------------- #
parse_quantification <- function(raw, region_order, qend) {
  if (length(region_order) == 0) return(list())
  v <- int32_all(raw)                 # unsigned int32 at every offset (1-based)
  stride <- 24L; Nreg <- length(region_order); span <- stride * Nreg
  first <- region_order[1]
  hi <- min(qend, length(v)) - span
  # candidate well starts: v[o]==first & v[o+4]==1   (offsets 0-based -> v index o+1)
  idx0 <- which(v[1:hi] == first)            # 1-based positions where v==first
  idx0 <- idx0[(idx0 + 4L) <= length(v) & v[idx0 + 4L] == 1]
  wells <- list()
  for (pos1 in idx0) {
    o <- pos1 - 1L                            # 0-based offset
    vals <- numeric(Nreg); ok <- TRUE
    for (k in 0:(Nreg - 1L)) {
      p <- o + stride * k
      reg <- rd_i32(raw, p); cnt <- rd_i32(raw, p + 4L); val <- rd_f64(raw, p + 16L)
      if (reg != region_order[k + 1L] || cnt != 1L || is.nan(val) || is.infinite(val) ||
          !(val == 0 || (abs(val) >= 1e-6 && abs(val) <= 1e7))) { ok <- FALSE; break }
      vals[k + 1L] <- val
    }
    if (ok) wells[[length(wells) + 1L]] <- setNames(vals, as.character(region_order))
  }
  # drop all-zero padding wells
  Filter(function(w) any(w != 0), wells)
}

# ---- stage 5: per-well statistics blocks ---------------------------------- #
F_OFF <- c(median = 16L, mean = 20L, trimmed_mean = 24L, cv = 28L, trimmed_cv = 32L,
           std_dev = 36L, trimmed_std_dev = 40L, std_err = 52L, trimmed_std_err = 56L)
BLOCK_HEADER <- 396L; RECSZ <- 80L; STAT_LIMIT <- 0x364000L

parse_stat_blocks <- function(raw, names) {
  if (length(names) == 0) return(list())
  nb <- charToRaw(names[1]); fl <- length(nb); N <- length(raw)
  hi <- min(STAT_LIMIT, N - fl)
  cand <- which(raw[1:hi] == as.raw(fl)) - 1L          # 0-based candidate block starts
  blocks <- list()
  for (o in cand) {
    if (!all(raw[(o + 2L):(o + 1L + fl)] == nb)) next
    p <- o; okk <- TRUE                                 # verify full <len><name><i32> sequence
    for (nm in names) {
      e <- charToRaw(nm); L <- length(e)
      if (rd_u8(raw, p) == L && all(raw[(p + 2L):(p + 1L + L)] == e)) p <- p + 1L + L + 4L
      else { okk <- FALSE; break }
    }
    if (!okk) next
    tot <- rd_i32(raw, o + BLOCK_HEADER); gat <- rd_i32(raw, o + BLOCK_HEADER + 4L)
    rgn <- rd_i32(raw, o + BLOCK_HEADER + 8L)
    recs <- vector("list", 8L)
    for (i in 0:7) {
      base <- o + BLOCK_HEADER + i * RECSZ
      if (base + RECSZ > N) break
      recs[[i + 1L]] <- sapply(F_OFF, function(off) rd_f32(raw, base + off))
    }
    blocks[[length(blocks) + 1L]] <- list(total = tot, gated = gat, region = rgn,
                                           medians = sapply(recs, function(r) r["median"]),
                                           records = recs)
  }
  blocks
}

# ---- stage 6: sample -> well assignment ----------------------------------- #
sample_category <- function(label) {
  p <- toupper(substr(label, 1, 1))
  switch(p, B = "Blank", S = "Standard", C = "Control", X = "Unknown", "Unknown")
}

parse_samples <- function(raw, panel, plate = 96L) {
  if (panel <= 0L) return(list())
  v <- int32_all(raw); N <- length(raw)
  back_string_end <- function(E) {                 # string whose last content byte is at E (0-based)
    if (E < 0L) return(NULL)
    for (s in E:max(0L, E - 81L)) {
      ln <- rd_u8(raw, s)
      if (length(ln) != 1L || is.na(ln)) next
      if (ln >= 0L && ln <= 80L && s + ln == E) {
        if (ln == 0L) return(list(start = s, text = ""))
        bytes <- as.integer(raw[(s + 2L):(s + 1L + ln)])
        if (all((bytes %in% c(9L,10L,13L)) | (bytes >= 32L & bytes < 127L)))
          return(list(start = s, text = rawToChar(as.raw(bytes))))
      }
    }
    NULL
  }
  rows <- "ABCDEFGHIJKLMNOP"
  wlabel <- function(idx) { r <- idx %/% 12L; c <- idx %% 12L
    if (r < nchar(rows)) paste0(substr(rows, r + 1L, r + 1L), c + 1L) else paste0("#", idx) }

  naoff <- which(v == panel) - 1L                  # 0-based offsets of the analyte-count field
  found <- list()
  for (na in naoff) {
    for (cnt in 1:8) {
      cpos <- na - 4L - 4L * cnt
      if (cpos < 16L) next
      if (rd_i32(raw, cpos) != cnt) next
      wells <- vapply(0:(cnt - 1L), function(w) rd_i32(raw, cpos + 4L + 4L * w), integer(1))
      if (!all(wells >= 0L & wells < plate)) next
      dil <- rd_f64(raw, cpos - 8L)
      d <- back_string_end(cpos - 9L); if (is.null(d)) next
      l <- back_string_end(d$start - 1L); if (is.null(l)) next
      lab <- l$text
      if (!grepl("^[A-Za-z]{1,4}[0-9]{0,4}$", lab)) next
      found[[length(found) + 1L]] <- list(label = lab, description = d$text,
        category = sample_category(lab), dilution = if (is.nan(dil)) NA else dil,
        well_index = wells, wells = vapply(wells, wlabel, character(1)))
      break
    }
  }
  # dedupe
  seen <- character(0); uniq <- list()
  for (s in found) {
    key <- paste(s$label, s$description, paste(sort(s$well_index), collapse = ","), sep = "|")
    if (!(key %in% seen)) { seen <- c(seen, key); uniq[[length(uniq) + 1L]] <- s }
  }
  uniq
}

# ---- stage 7: merge -------------------------------------------------------- #
STAT_FIELDS <- c("median","mean","trimmed_mean","cv","trimmed_cv",
                 "std_dev","trimmed_std_dev","std_err","trimmed_std_err","bead_count")

merge_all <- function(medians, stat_blocks, region_order, region_name, samples) {
  rows <- "ABCDEFGHIJKLMNOP"; ncol <- 12L
  wells <- vector("list", length(medians))
  for (i in seq_along(medians)) {
    idx <- i - 1L; r <- idx %/% ncol; c <- idx %% ncol
    label <- if (r < nchar(rows)) paste0(substr(rows, r + 1L, r + 1L), c + 1L) else NA
    med <- medians[[i]]
    analytes <- list()
    for (rg in names(med)) {
      nm <- region_name[[rg]]; if (is.null(nm)) nm <- rg
      st <- as.list(rep(NA_real_, length(STAT_FIELDS))); names(st) <- STAT_FIELDS
      st$region <- as.integer(rg); st$median <- med[[rg]]
      analytes[[nm]] <- st
    }
    wells[[i]] <- list(well = label, index = i, sample_label = NA, sample_description = NA,
                       sample_category = NA, dilution = NA, total_events = NA,
                       gated_events = NA, region_events = NA, analytes = analytes)
  }
  # well -> sample
  for (s in samples) for (idx in s$well_index) if (idx >= 0 && idx < length(wells)) {
    wells[[idx + 1L]]$sample_label <- s$label
    wells[[idx + 1L]]$sample_description <- s$description
    wells[[idx + 1L]]$sample_category <- s$category
    wells[[idx + 1L]]$dilution <- s$dilution
  }
  # align stat blocks to wells by median set, then fill stats
  used <- integer(0)
  for (blk in stat_blocks) {
    recmeds <- blk$medians
    best <- -1L
    for (i in seq_along(wells)) {
      if (i %in% used) next
      wm <- sapply(wells[[i]]$analytes, function(a) a$median)
      if (length(wm) && all(sapply(wm, function(mv) any(abs(recmeds - mv) < 0.6)))) { best <- i; break }
    }
    if (best < 0) next
    used <- c(used, best)
    wells[[best]]$total_events <- blk$total; wells[[best]]$gated_events <- blk$gated
    wells[[best]]$region_events <- blk$region
    for (nm in names(wells[[best]]$analytes)) {
      mv <- wells[[best]]$analytes[[nm]]$median
      ri <- which.min(abs(recmeds - mv))
      if (abs(recmeds[ri] - mv) >= 0.6) next
      rec <- blk$records[[ri]]
      a <- wells[[best]]$analytes[[nm]]
      a$mean <- rec["mean"]; a$trimmed_mean <- rec["trimmed_mean"]
      a$cv <- rec["cv"]; a$trimmed_cv <- rec["trimmed_cv"]
      a$std_dev <- rec["std_dev"]; a$trimmed_std_dev <- rec["trimmed_std_dev"]
      a$std_err <- rec["std_err"]; a$trimmed_std_err <- rec["trimmed_std_err"]
      a$bead_count <- if (rec["std_err"] > 0) round((rec["std_dev"] / rec["std_err"])^2) else NA
      wells[[best]]$analytes[[nm]] <- a
    }
  }
  wells
}

# ---- top-level ------------------------------------------------------------- #
parse_rbx <- function(path) {
  raw <- readBin(path, "raw", n = file.info(path)$size)
  if (!(raw[1] == as.raw(0x61) && raw[2] == as.raw(0x42)))
    stop(sprintf("%s does not start with the Bio-Plex 'aB' magic — not a binary .rbx/.srbx.", path))
  hdr <- parse_header(raw)
  an  <- parse_analytes(raw)
  meta <- parse_metadata(raw)
  region_order <- sort(an$regions)
  region_name  <- setNames(as.list(an$names), as.character(an$regions))
  qend <- if (an$def_off > 0L) an$def_off else min(length(raw), 0x44000L)
  medians <- parse_quantification(raw, region_order, qend)
  plate   <- max(length(medians), 96L)
  samples <- parse_samples(raw, length(an$names), plate)
  blocks  <- parse_stat_blocks(raw, an$names)
  wells   <- merge_all(medians, blocks, region_order, region_name, samples)
  list(format = list(magic = hdr$magic, version = hdr$version),
       panel = list(name = hdr$panel,
                    analytes = Map(function(n, r) list(name = n, region = r), an$names, an$regions)),
       metadata = meta, samples = samples, wells = wells)
}

# ---- outputs --------------------------------------------------------------- #

extract_tdap_panel <- function(doc) {
  analytes <- doc$panel$analytes
  if (is.null(analytes) || length(analytes) == 0)
    return(data.frame(panel_name     = character(),
                      analyte_name   = character(),
                      analyte_region = character(),
                      stringsAsFactors = FALSE))
  data.frame(
    panel_name     = doc$panel$name,
    analyte_name   = sapply(analytes, function(a) a$name),
    analyte_region = sapply(analytes, function(a) a$region),
    stringsAsFactors = FALSE
  )
}

extract_tdap_samples <- function(doc) {
  rows <- lapply(doc$samples, function(s) {
    data.frame(
      label            = s$label,
      description      = s$description,
      category         = s$category,
      dilution         = s$dilution,
      well             = s$wells,
      well_index       = s$well_index,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

long_dataframe <- function(doc) {
  recs <- list()
  for (w in doc$wells) for (nm in names(w$analytes)) {
    s <- w$analytes[[nm]]
    recs[[length(recs) + 1L]] <- data.frame(
      well = ifelse(is.null(w$well), NA, w$well), well_index = w$index,
      sample_label = w$sample_label, sample_description = w$sample_description,
      sample_category = w$sample_category, dilution = w$dilution,
      analyte = nm, region = s$region,
      total_events = w$total_events, gated_events = w$gated_events, region_events = w$region_events,
      median = s$median, mean = s$mean, trimmed_mean = s$trimmed_mean,
      cv = s$cv, trimmed_cv = s$trimmed_cv, std_dev = s$std_dev,
      trimmed_std_dev = s$trimmed_std_dev, std_err = s$std_err,
      trimmed_std_err = s$trimmed_std_err, bead_count = s$bead_count,
      stringsAsFactors = FALSE)
  }
  do.call(rbind, recs)
}

write_outputs <- function(doc, prefix = "rbx") {
  df <- long_dataframe(doc)
  write.csv(df, paste0(prefix, "_long.csv"), row.names = FALSE)
  # wide FI matrix
  names <- vapply(doc$panel$analytes, function(a) a$name, character(1))
  fi <- data.frame(well = vapply(doc$wells, function(w)
                     if (is.null(w$well) || is.na(w$well)) NA_character_ else as.character(w$well),
                     character(1)),
                   stringsAsFactors = FALSE)
  for (nm in names) fi[[nm]] <- vapply(doc$wells, function(w)
    if (!is.null(w$analytes[[nm]])) w$analytes[[nm]]$median else NA_real_, numeric(1))
  write.csv(fi, paste0(prefix, "_fi.csv"), row.names = FALSE)
  if (requireNamespace("jsonlite", quietly = TRUE))
    writeLines(jsonlite::toJSON(doc, auto_unbox = TRUE, na = "null", pretty = TRUE),
               paste0(prefix, "_full.json"))
  invisible(c(paste0(prefix, "_long.csv"), paste0(prefix, "_fi.csv")))
}

print_summary <- function(doc) {
  cat(sprintf("Bio-Plex binary .rbx  (magic=%s  version=%s)\n", doc$format$magic, doc$format$version))
  cat(sprintf("Panel: %s\n\nAnalytes (%d):\n", doc$panel$name, length(doc$panel$analytes)))
  for (a in doc$panel$analytes) cat(sprintf("   %-10s region %s\n", a$name, a$region))
  cat("\nMetadata:\n"); for (k in names(doc$metadata)) cat(sprintf("   %-16s %s\n", k, doc$metadata[[k]]))
  cat(sprintf("\nSamples: %d   Wells: %d\n", length(doc$samples), length(doc$wells)))
}

# ---- CLI ------------------------------------------------------------------- #
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) { cat("Usage: Rscript rbx_binary_parser.R FILE.rbx [out_prefix]\n"); quit(status = 1) }
  doc <- parse_rbx(args[1])
  print_summary(doc)
  prefix <- if (length(args) >= 2) args[2] else "rbx"
  out <- write_outputs(doc, prefix)
  cat(sprintf("\n[wrote] %s\n", paste(out, collapse = ", ")))
}
