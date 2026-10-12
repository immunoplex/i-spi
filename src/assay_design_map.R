# =============================================================================
# assay_design_map.R  --  study-design map: assign identity from a spreadsheet
# -----------------------------------------------------------------------------
# An alternative to reading identity out of the Description text. The user
# downloads a design template (generated here, never stored), fills the
# `current_plates_map` tab -- one row per plate well:
#
#   plate  well  sample_type  subject_id  group1  group2  timepoint  source  dilution
#
# -- and uploads it. The rows are read, validated against the loaded plates with
# the same contract the description parser enforces (AI_TYPE_REQUIRED), and, on
# request, applied: sample_type is written into the well inventory and the rest
# becomes the RESOLVED identity for those wells, in exactly the shape
# ai_resolve_inventory() produces, so the layout template is built the same way
# whichever route supplied it. No description parsing is involved.
#
# Partial maps are fine: a well that is absent keeps whatever the description
# rules say. A blank sample_type means "keep the type the plate already has".
#
# Pure base R (+ openxlsx for the workbook). No Shiny, no DB.
# Source AFTER assay_description_parse.R, assay_well_inventory.R and
# assay_shape_rules.R.
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

AI_DESIGN_SHEET   <- "current_plates_map"
AI_DESIGN_EXAMPLE <- "example_template"
AI_DESIGN_COLS    <- c("plate", "well", "sample_type", "subject_id", "group1",
                       "group2", "timepoint", "source", "dilution")
# header spellings accepted on upload (the template text says "timeperiod")
AI_DESIGN_ALIASES <- c(timeperiod = "timepoint", time_period = "timepoint",
                       sampletype = "sample_type", type = "sample_type",
                       patientid = "subject_id", patient_id = "subject_id",
                       groupa = "group1", groupb = "group2",
                       dilution_factor = "dilution", specimen_source = "source")
AI_DESIGN_SUBJECT_MAX <- 15L   # patientid width used elsewhere in the importer

.ai_design_issue_frame <- function(sev = character(), col = character(),
                                   msg = character(), row = integer())
  data.frame(sheet = rep(AI_DESIGN_SHEET, length(sev)), severity = sev,
             column = col, message = msg, row = row, stringsAsFactors = FALSE)

# A plate name reduced to what identifies it: upper case, no file extension, no
# leading "xPONENT" tag, the plate-list "3. " ordinal of the grid's labels dropped.
.ai_design_plate_norm <- function(x) {
  s <- toupper(.ai_design_trim(x))
  s <- sub("^[0-9]+\\.[[:space:]]+", "", s)
  s <- sub("[[:space:]]+\\(.*\\)$", "", s)
  s <- sub("\\.(CSV|XLSX|XLS|TXT|RBX|SRBX)$", "", s)
  s <- sub("^XPONENT[_ -]+", "", s)
  s[is.na(x)] <- NA_character_
  s
}

# a cell as trimmed text; blank / NA -> NA. Whole numbers lose any ".0" or
# scientific form (Excel hands back 10000 as a double, 1e5 would print "1e+05").
.ai_design_chr <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x)) {
    out <- ifelse(is.na(x), NA_character_,
                  format(x, scientific = FALSE, trim = TRUE, digits = 15))
    out[!is.na(x) & x == round(x)] <- format(x[!is.na(x) & x == round(x)],
                                             scientific = FALSE, trim = TRUE)
    x <- out
  }
  x <- .ai_design_trim(x)
  x[!nzchar(x) | toupper(x) == "NA"] <- NA_character_
  x
}

# trimws() leaves non-breaking spaces (U+00A0), which spreadsheets pick up from
# copy-paste (the reference template has one before every plate name).
.ai_design_trim <- function(x)
  trimws(gsub("[   ﻿]", " ", as.character(x)))


# ---- the template -----------------------------------------------------------

.ai_design_instructions <- function() c(
  "Fill in the table below (one row per plate well), then upload this file back in I-SPI. Delete nothing from the header row. Leave a value out by leaving its cell completely empty.",
  "Test samples (sample_type = X) need a subject_id and a timepoint. A sample dilution is strongly advised (a missing one is settled later). group1 and group2 are optional grouping variables that help measure error later in the pipeline.",
  "Standard curve samples (sample_type = S) need a source and a dilution. Add a number to the type for the standard point, e.g. S1, S2 ... (optional if the plate already has them).",
  "Blank samples (sample_type = B) need a source and a dilution (usually 1). Control samples (sample_type = C) need a dilution; a source is optional.",
  "dilution is the denominator of the dilution: write 100 or 1:100 for a 1-in-100 dilution. plate must match the plate names already in the table. Wells you leave out keep what the description field says. Leaving sample_type empty keeps the plate's current type.")

# An illustrative 96-well plate (not the user's data): standards down column 1,
# blanks and a control in column 12, samples elsewhere.
.ai_design_example <- function() {
  rows <- LETTERS[1:8]; wells <- unlist(lapply(1:12, function(cn) paste0(rows, cn)))
  df <- data.frame(plate = "Example_Plate_1", well = wells, sample_type = "X",
                   subject_id = NA_character_, group1 = NA_character_,
                   group2 = NA_character_, timepoint = NA_character_,
                   source = NA_character_, dilution = NA_character_,
                   stringsAsFactors = FALSE)
  is_std <- grepl("^[A-H]1$", df$well)
  is_blk <- df$well %in% c("A12", "B12", "C12", "D12")
  is_ctl <- df$well %in% c("E12", "F12")
  df$sample_type[is_std] <- paste0("S", match(df$well[is_std], paste0(rows, 1)))
  df$source[is_std] <- "NIBSC"
  df$dilution[is_std] <- as.character(round(50 * 1.8 ^ (match(df$well[is_std], paste0(rows, 1)) - 1)))
  df$sample_type[is_blk] <- "B"; df$source[is_blk] <- "PBS"; df$dilution[is_blk] <- "1"
  df$sample_type[is_ctl] <- "C"; df$source[is_ctl] <- "HighControl"; df$dilution[is_ctl] <- "5000"
  is_x <- df$sample_type == "X" & !df$well %in% c("G12", "H12")
  df$sample_type[df$well %in% c("G12", "H12")] <- "B"
  df$source[df$well %in% c("G12", "H12")] <- "PBS"; df$dilution[df$well %in% c("G12", "H12")] <- "1"
  n <- sum(is_x)
  df$subject_id[is_x] <- as.character(100 + seq_len(n))
  df$group1[is_x]     <- rep(c("Vaccine", "Placebo"), length.out = n)
  df$timepoint[is_x]  <- rep(c("Baseline", "Day28"), each = 2, length.out = n)
  df$dilution[is_x]   <- "10000"
  df
}

#' The prefilled `current_plates_map`: every occupied well of the loaded plates,
#' with the type, source and dilution the plates already carry.
ai_design_skeleton <- function(inv) {
  if (is.null(inv) || !nrow(inv))
    return(.ai_design_example()[0L, , drop = FALSE])
  d <- inv[!is.na(inv$specimen_type), , drop = FALSE]
  d <- d[order(d$plate_index, d$col_number, match(d$row_letter, AI_ROW_LETTERS)), , drop = FALSE]
  n <- nrow(d)
  chr <- function(col) if (col %in% names(d)) .ai_design_chr(d[[col]]) else rep(NA_character_, n)
  dil <- if ("instrument_dilution" %in% names(d)) d$instrument_dilution else rep(NA_real_, n)
  dil[!is.finite(dil) | dil <= 0] <- NA_real_
  data.frame(plate = d$plate_key, well = d$well, sample_type = d$type_code,
             subject_id = NA_character_, group1 = NA_character_, group2 = NA_character_,
             timepoint = NA_character_, source = chr("instrument_source"),
             dilution = ifelse(is.na(dil), NA_character_, format(dil, scientific = FALSE, trim = TRUE)),
             stringsAsFactors = FALSE)
}

#' Write the study design template workbook for the loaded plates.
#'
#' Two tabs, as in the reference template: `example_template` (a worked example,
#' not read back) and `current_plates_map` (the tab the user fills in, prefilled
#' with the loaded plates' wells). Built from code on every download.
ai_design_write_template <- function(inv, path) {
  skeleton <- ai_design_skeleton(inv)
  wb <- openxlsx::createWorkbook()
  head_st <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1",
                                   border = "bottom")
  note_st <- openxlsx::createStyle(textDecoration = "italic", fontColour = "#444444")
  fill_tab <- function(sheet, df) {
    openxlsx::addWorksheet(wb, sheet)
    ins <- .ai_design_instructions()
    for (i in seq_along(ins)) openxlsx::writeData(wb, sheet, ins[i], startRow = i, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, note_st, rows = seq_along(ins), cols = 1L, gridExpand = TRUE)
    hdr <- length(ins) + 1L
    openxlsx::writeData(wb, sheet, df, startRow = hdr, headerStyle = head_st)
    # dilution is written as a number where it is one, so Excel doesn't flag it
    num <- suppressWarnings(as.numeric(df$dilution))
    ok  <- !is.na(num) & grepl("^[0-9]+$", df$dilution)
    if (any(ok))
      openxlsx::writeData(wb, sheet, num[ok], startRow = hdr + which(ok)[1],
                          startCol = match("dilution", names(df)), colNames = FALSE)
    openxlsx::setColWidths(wb, sheet, cols = seq_along(df), widths = c(34, 8, 13, 14, 10, 10, 12, 14, 10))
    openxlsx::freezePane(wb, sheet, firstActiveRow = hdr + 1L)
    if (nrow(df))
      openxlsx::dataValidation(wb, sheet, cols = 3L, rows = (hdr + 1L):(hdr + nrow(df)),
                               type = "list", value = '"X,S,C,B"', allowBlank = TRUE)
  }
  fill_tab(AI_DESIGN_EXAMPLE, .ai_design_example())
  fill_tab(AI_DESIGN_SHEET, skeleton)
  openxlsx::activeSheet(wb) <- 2L
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}


# ---- reading ----------------------------------------------------------------

#' Read the `current_plates_map` tab. Stops with a readable message when the tab
#' or the required columns are missing. The header row is found by content, so
#' the instruction lines above it may be edited or removed.
ai_design_read <- function(path) {
  sheets <- tryCatch(openxlsx::getSheetNames(path),
                     error = function(e) stop("That file could not be read as an Excel workbook.", call. = FALSE))
  if (!AI_DESIGN_SHEET %in% sheets)
    stop(sprintf("The workbook has no tab named '%s' (it has: %s). Use the downloaded design template.",
                 AI_DESIGN_SHEET, paste(sheets, collapse = ", ")), call. = FALSE)
  raw <- openxlsx::read.xlsx(path, sheet = AI_DESIGN_SHEET, colNames = FALSE,
                             skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  if (!nrow(raw)) stop("The current_plates_map tab is empty.", call. = FALSE)
  norm <- function(x) { x <- tolower(.ai_design_trim(x)); x[is.na(x)] <- ""; x }
  hit <- which(vapply(seq_len(nrow(raw)), function(i) {
    v <- norm(unlist(raw[i, ], use.names = FALSE)); "plate" %in% v && "well" %in% v
  }, logical(1)))
  if (!length(hit))
    stop("Could not find the header row (a row with 'plate' and 'well'). Keep the header row of the template.",
         call. = FALSE)
  h <- hit[1]
  nm <- norm(unlist(raw[h, ], use.names = FALSE))
  nm <- ifelse(nm %in% names(AI_DESIGN_ALIASES), AI_DESIGN_ALIASES[nm], nm)
  body <- raw[-seq_len(h), , drop = FALSE]
  names(body) <- ifelse(nzchar(nm), nm, paste0(".blank", seq_along(nm)))
  need <- c("plate", "well")
  if (length(miss <- setdiff(need, names(body))))
    stop(sprintf("The header row has no '%s' column.", paste(miss, collapse = "', '")), call. = FALSE)
  for (cl in setdiff(AI_DESIGN_COLS, names(body))) body[[cl]] <- NA_character_
  out <- as.data.frame(lapply(body[, AI_DESIGN_COLS, drop = FALSE], .ai_design_chr),
                       stringsAsFactors = FALSE)
  out$.row <- h + seq_len(nrow(body))
  # drop rows that name no plate and no well (stray formatting below the table)
  out[!(is.na(out$plate) & is.na(out$well)), , drop = FALSE]
}


# ---- validation -------------------------------------------------------------

#' Check a design against the loaded plates, with the description parser's
#' contract: AI_TYPE_REQUIRED says what each sample type must carry.
#'
#' @param design ai_design_read() output.
#' @param inv    the well inventory (the loaded plates).
#' @return list(design = normalised rows keyed by plate_key/well (only rows that
#'   name a real occupied well), issues = data.frame(sheet, severity, column,
#'   message, row), n_rows). `issues` has a `row` column (spreadsheet row).
ai_design_check <- function(design, inv) {
  iss <- list()
  add <- function(sev, col, msg, row = NA_integer_)
    iss[[length(iss) + 1L]] <<- .ai_design_issue_frame(sev, col, msg, as.integer(row))
  finish <- function(d)
    list(design = d, n_rows = nrow(design),
         issues = if (length(iss)) do.call(rbind, iss) else .ai_design_issue_frame())

  empty_design <- data.frame(plate_key = character(), well = character(),
    sample_type = character(), type_suffix = character(), subject_id = character(),
    group1 = character(), group2 = character(), timepoint = character(),
    source = character(), dilution = integer(), .row = integer(), stringsAsFactors = FALSE)

  if (is.null(inv) || !nrow(inv)) {
    add("error", "plate", "Parse the instrument file(s) first - there are no plates to apply a design to.")
    return(finish(empty_design))
  }
  if (is.null(design) || !nrow(design)) {
    add("error", "plate", "The current_plates_map tab has no rows. Fill in the table and upload it again.")
    return(finish(empty_design))
  }

  # plate: match the plate key, its label or its file name; case, a file
  # extension and a leading "xPONENT_" are ignored, so "20221003_AGA_Plate_1"
  # finds plate XPONENT_20221003_AGA_PLATE_1. A name that fits more than one
  # plate (e.g. a file holding several) is never guessed at.
  plates <- unique(inv[, intersect(c("plate_key", "plate_label", "source_file"), names(inv))])
  cand <- do.call(rbind, lapply(intersect(c("plate_key", "plate_label", "source_file"), names(plates)),
    function(cn) data.frame(name = .ai_design_plate_norm(plates[[cn]]), key = plates$plate_key,
                            stringsAsFactors = FALSE)))
  cand <- unique(cand[!is.na(cand$name) & nzchar(cand$name), ])
  cand <- cand[!(duplicated(cand$name) | duplicated(cand$name, fromLast = TRUE)), , drop = FALSE]
  pk <- cand$key[match(.ai_design_plate_norm(design$plate), cand$name)]
  bad_plate <- is.na(pk)
  if (any(is.na(design$plate)))
    add("error", "plate", sprintf("Row(s) %s have a well but no plate.",
        paste(utils::head(design$.row[is.na(design$plate)], 8), collapse = ", ")),
        design$.row[is.na(design$plate)][1])
  for (p in unique(design$plate[bad_plate & !is.na(design$plate)]))
    add("error", "plate",
        sprintf("Plate '%s' is not one of the loaded plates (%s).", p,
                paste(utils::head(plates$plate_key, 6), collapse = ", ")),
        design$.row[which(design$plate == p)[1]])

  w <- ai_normalize_well(design$well)
  if (any(is.na(w)))
    add("error", "well", sprintf("Row(s) %s have a plate but no well.",
        paste(utils::head(design$.row[is.na(w) & !is.na(design$plate)], 8), collapse = ", ")),
        design$.row[is.na(w) & !is.na(design$plate)][1])

  ii <- match(paste(pk, w, sep = "\r"), paste(inv$plate_key, inv$well, sep = "\r"))
  no_well <- !bad_plate & !is.na(w) & is.na(ii)
  for (i in which(no_well))
    add("error", "well", sprintf("Well %s is not on plate '%s'.", w[i], pk[i]), design$.row[i])

  empty_well <- !is.na(ii) & is.na(inv$specimen_type[ii])
  for (i in which(empty_well))
    add("warning", "well",
        sprintf("Well %s on plate '%s' has no readings in the instrument file; the row is ignored.", w[i], pk[i]),
        design$.row[i])

  keep <- !bad_plate & !is.na(w) & !is.na(ii) & !empty_well
  d <- design[keep, , drop = FALSE]
  ii <- ii[keep]; pk <- pk[keep]; w <- w[keep]
  if (!nrow(d)) return(finish(empty_design))

  # duplicates
  key <- paste(pk, w, sep = "\r")
  dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
  if (any(dup))
    add("error", "well", sprintf("%d row(s) repeat a plate/well already listed (e.g. %s on '%s', rows %s).",
        sum(dup), w[dup][1], pk[dup][1], paste(d$.row[key == key[dup][1]], collapse = " and ")),
        d$.row[dup][1])

  # sample type: given letter(+index), else the plate's current type
  given   <- toupper(.ai_design_trim(d$sample_type))
  letter  <- ifelse(is.na(given), NA_character_, substr(given, 1L, 1L))
  suffix  <- ifelse(is.na(given), NA_character_, substring(given, 2L))
  bad_t   <- !is.na(given) & (!letter %in% AI_SPECIMEN_TYPES | !grepl("^[0-9]*$", suffix))
  for (i in which(bad_t))
    add("error", "sample_type",
        sprintf("sample_type '%s' is not X, S, C or B (optionally followed by a number).", d$sample_type[i]),
        d$.row[i])
  cur_letter <- inv$specimen_type[ii]
  eff <- ifelse(is.na(letter) | bad_t, cur_letter, letter)

  # the description parser's contract, per type
  comp_col <- c(PatientID = "subject_id", TimePeriod = "timepoint",
                DilutionFactor = "dilution", Source = "source")
  dil <- vapply(d$dilution, function(x) {
    r <- ai_parse_dilution(x); if (isTRUE(r$ok) && r$value > 0) r$value else NA_integer_
  }, integer(1))
  bad_dil <- !is.na(d$dilution) & is.na(dil)
  for (i in which(bad_dil))
    add("error", "dilution", sprintf("Dilution '%s' is not a positive integer or a 1:N ratio.", d$dilution[i]),
        d$.row[i])

  for (i in seq_len(nrow(d))) {
    if (is.na(eff[i])) next
    for (comp in AI_TYPE_REQUIRED[[eff[i]]]) {
      col <- comp_col[[comp]]
      miss <- if (col == "dilution") is.na(dil[i]) && !bad_dil[i] else is.na(d[[col]][i])
      if (miss)
        add("error", col, sprintf("Row %d (%s, %s): type %s needs a %s.",
            d$.row[i], pk[i], w[i], eff[i], col), d$.row[i])
    }
    if (eff[i] == "X" && is.na(dil[i]) && !bad_dil[i])
      add("warning", "dilution",
          sprintf("Row %d (%s, %s): no sample dilution; it will have to be settled in the dilution step.",
                  d$.row[i], pk[i], w[i]), d$.row[i])
    if (!is.na(d$subject_id[i]) && nchar(d$subject_id[i]) > AI_DESIGN_SUBJECT_MAX)
      add("warning", "subject_id",
          sprintf("Row %d: subject_id '%s' is longer than %d characters and will be truncated.",
                  d$.row[i], d$subject_id[i], AI_DESIGN_SUBJECT_MAX), d$.row[i])
  }

  # a well that keeps its old type but whose index would clash is left to the
  # grid gate; nothing more to say here.
  covered <- sum(!is.na(ii))
  occ <- sum(!is.na(inv$specimen_type))
  if (covered < occ && !any(unlist(lapply(iss, function(x) x$severity == "error"))))
    add("warning", "plate",
        sprintf("The table covers %d of %d occupied wells; the rest keep what the description field says.",
                covered, occ))

  out <- data.frame(plate_key = pk, well = w, sample_type = eff,
                    type_suffix = ifelse(is.na(suffix), NA_character_, suffix),
                    subject_id = d$subject_id, group1 = d$group1, group2 = d$group2,
                    timepoint = d$timepoint, source = d$source, dilution = dil,
                    .row = d$.row, stringsAsFactors = FALSE)
  finish(out)
}

ai_design_has_errors <- function(chk)
  !is.null(chk) && nrow(chk$issues) > 0 && any(chk$issues$severity == "error")


# ---- applying ---------------------------------------------------------------

#' Write the design's sample types into the inventory. A well keeps its index
#' (S3 stays S3) when its letter doesn't change and none is given. Wells whose
#' type was changed, or was only a suggestion, become user-confirmed.
ai_design_apply_inventory <- function(inv, dn) {
  if (is.null(dn) || !nrow(dn)) return(inv)
  i <- match(paste(dn$plate_key, dn$well, sep = "\r"),
             paste(inv$plate_key, inv$well, sep = "\r"))
  ok <- !is.na(i); dn <- dn[ok, , drop = FALSE]; i <- i[ok]
  old_letter <- inv$specimen_type[i]
  old_suffix <- ifelse(is.na(inv$type_code[i]), "", ai_type_suffix(inv$type_code[i]))
  sfx <- ifelse(!is.na(dn$type_suffix) & nzchar(dn$type_suffix), dn$type_suffix,
                ifelse(!is.na(old_letter) & old_letter == dn$sample_type, old_suffix, ""))
  new_code <- paste0(dn$sample_type, sfx)
  changed  <- is.na(inv$type_code[i]) | inv$type_code[i] != new_code
  was_prop <- !is.na(inv$type_origin[i]) & inv$type_origin[i] == "proposed"
  inv$type_code[i]     <- new_code
  inv$specimen_type[i] <- ai_type_letter(new_code)
  inv$type_origin[i[changed | was_prop]] <- "user"
  inv
}

#' Which rows of the inventory the design currently governs: the well is in the
#' design AND the inventory still has the type the design gave it. A well retyped
#' on the grid after applying is released back to the description rules.
ai_design_coverage <- function(inv, dn) {
  if (is.null(inv) || !nrow(inv) || is.null(dn) || !nrow(dn))
    return(rep(FALSE, if (is.null(inv)) 0L else nrow(inv)))
  i <- match(paste(inv$plate_key, inv$well, sep = "\r"),
             paste(dn$plate_key, dn$well, sep = "\r"))
  !is.na(i) & !is.na(inv$specimen_type) & inv$specimen_type == dn$sample_type[i]
}

#' The design as resolved identity, in ai_resolve_inventory()'s output shape.
ai_design_resolved <- function(inv, dn) {
  cov <- ai_design_coverage(inv, dn)
  d <- inv[cov, , drop = FALSE]
  n <- nrow(d)
  if (!n) return(data.frame())
  j <- match(paste(d$plate_key, d$well, sep = "\r"), paste(dn$plate_key, dn$well, sep = "\r"))
  g <- dn[j, , drop = FALSE]
  blank <- function(x) ifelse(is.na(x), "", as.character(x))
  sfx <- ifelse(is.na(d$type_code), "", ai_type_suffix(d$type_code))
  sid <- blank(g$subject_id)
  sid[d$specimen_type != "X" & !nzchar(sid)] <- ifelse(nzchar(sfx), sfx, "1")[d$specimen_type != "X" & !nzchar(sid)]
  too_long <- nchar(sid) > AI_DESIGN_SUBJECT_MAX
  sid[too_long] <- substr(sid[too_long], 1L, AI_DESIGN_SUBJECT_MAX)
  data.frame(plateid = d$plate_key, well = d$well, specimen_type = d$specimen_type,
             type_code = d$type_code, description = d$description,
             shape_key = rep("design_map", n),
             subject_id = sid,
             timepoint_tissue_abbreviation = blank(g$timepoint),
             specimen_dilution_factor = as.integer(g$dilution),
             specimen_source = blank(g$source),
             groupa = blank(g$group1), groupb = blank(g$group2),
             biosample_id_barcode = sfx, stringsAsFactors = FALSE)
}

#' Lay the design's rows over a rules-resolved table (design wins per well).
ai_design_overlay_resolved <- function(resolved, design_resolved) {
  if (is.null(design_resolved) || !nrow(design_resolved)) return(resolved)
  if (is.null(resolved) || !nrow(resolved)) return(design_resolved)
  key_r <- paste(resolved$plateid, resolved$well, sep = "\r")
  key_d <- paste(design_resolved$plateid, design_resolved$well, sep = "\r")
  rbind(resolved[!key_r %in% key_d, names(design_resolved), drop = FALSE], design_resolved)
}

#' Short counts for the status line: wells per sample type under the design.
ai_design_summary <- function(dn) {
  if (is.null(dn) || !nrow(dn)) return("no wells")
  tb <- table(factor(dn$sample_type, levels = AI_SPECIMEN_TYPES))
  sprintf("%d well(s) on %d plate(s): %s", nrow(dn), length(unique(dn$plate_key)),
          paste(sprintf("%d %s", tb[tb > 0], names(tb)[tb > 0]), collapse = ", "))
}


# ---- integrity of an applied design ------------------------------------------

#' The checks that stand in for description parsing when a design supplies the
#' identity. They run on what the design actually governs in the CURRENT
#' inventory (a well retyped on the grid after applying is not governed), and on
#' cross-references the upload-time check cannot make: against the instrument
#' file, between wells of a subject, and along a standard curve.
#'
#' @return list(issues = data.frame(sheet, severity, column, message),
#'              summary = data.frame(plate_key, X, S, C, B, rules),
#'              n_covered, n_open)
#'   `rules` counts occupied wells the design does NOT govern (they still need
#'   description rules). Errors block the step; warnings are carried into the
#'   validation table.
ai_design_integrity <- function(inv, dn) {
  iss <- list()
  add <- function(sev, col, msg)
    iss[[length(iss) + 1L]] <<- data.frame(sheet = "design_integrity", severity = sev,
      column = col, message = msg, stringsAsFactors = FALSE)
  ex <- function(x, n = 4L) {
    x <- unique(x); paste0(paste(utils::head(x, n), collapse = ", "),
                           if (length(x) > n) sprintf(" (+%d more)", length(x) - n) else "")
  }
  none <- list(issues = data.frame(sheet = character(), severity = character(),
                 column = character(), message = character(), stringsAsFactors = FALSE),
               summary = data.frame(), n_covered = 0L, n_open = 0L)
  if (is.null(inv) || !nrow(inv) || is.null(dn) || !nrow(dn)) return(none)

  occ <- !is.na(inv$specimen_type)
  cov <- ai_design_coverage(inv, dn) & occ
  d   <- inv[cov, , drop = FALSE]
  r   <- ai_design_resolved(inv, dn)
  n_open <- sum(occ & !cov)
  if (!nrow(r)) return(modifyList(none, list(n_open = n_open)))
  wl <- paste0(r$plateid, " ", r$well)

  # 1. what each type needs (the parser's contract), read off the resolved table
  comp <- list(PatientID = c("subject_id", r$subject_id),
               TimePeriod = c("timepoint", r$timepoint_tissue_abbreviation),
               DilutionFactor = c("dilution", r$specimen_dilution_factor),
               Source = c("source", r$specimen_source))
  for (t in intersect(AI_SPECIMEN_TYPES, unique(r$specimen_type))) {
    sel <- r$specimen_type == t
    for (cn in AI_TYPE_REQUIRED[[t]]) {
      v <- comp[[cn]]; col <- v[1L]; v <- v[-1L]
      miss <- sel & (is.na(v) | !nzchar(as.character(v)))
      if (any(miss))
        add("error", col, sprintf("%d %s well(s) have no %s (e.g. %s).", sum(miss),
            tolower(AI_SPECIMEN_LABELS[[t]]), col, ex(wl[miss])))
    }
  }

  # 2. a subject should have one group / one source
  x <- r$specimen_type == "X" & nzchar(r$subject_id)
  for (pair in list(c("groupa", "group1"), c("groupb", "group2"))) {
    v <- r[[pair[1]]]
    g <- tapply(v[x & nzchar(v)], r$subject_id[x & nzchar(v)],
                function(z) length(unique(z)))
    bad <- names(g)[g > 1L]
    if (length(bad))
      add("warning", pair[2], sprintf("%d subject(s) are assigned more than one %s (e.g. %s).",
          length(bad), pair[2], ex(bad)))
  }

  # 3. standard curve: along the index, the dilution should not decrease
  s <- which(r$specimen_type == "S" & is.finite(r$specimen_dilution_factor))
  if (length(s)) {
    idx <- suppressWarnings(as.integer(ai_type_suffix(d$type_code[s])))
    for (p in unique(r$plateid[s])) {
      k <- s[r$plateid[s] == p]; ik <- idx[r$plateid[s] == p]
      ok <- !is.na(ik)
      if (sum(ok) < 2L) next
      dl <- tapply(r$specimen_dilution_factor[k][ok], ik[ok], function(z) length(unique(z)))
      if (any(dl > 1L))
        add("warning", "dilution", sprintf(
            "Plate %s: standard point(s) %s have replicates with different dilutions.",
            p, ex(paste0("S", names(dl)[dl > 1L]))))
      m <- tapply(r$specimen_dilution_factor[k][ok], ik[ok], min)
      m <- m[order(as.integer(names(m)))]
      if (length(m) > 1L && any(diff(as.numeric(m)) < 0))
        add("warning", "dilution", sprintf(
            "Plate %s: standard dilutions are not increasing with the point number (%s).",
            p, paste(sprintf("S%s=%s", names(m), m), collapse = ", ")))
    }
  }

  # 4. against the instrument file, where it reported a value
  if ("instrument_dilution" %in% names(d)) {
    idl <- suppressWarnings(as.numeric(d$instrument_dilution))
    diff_d <- is.finite(idl) & idl > 0 & is.finite(r$specimen_dilution_factor) &
      abs(idl - r$specimen_dilution_factor) > 1e-9
    if (any(diff_d))
      add("warning", "dilution", sprintf(
          "%d well(s) have a dilution that differs from the one in the instrument file (e.g. %s: design %s, file %s). The design is used.",
          sum(diff_d), wl[diff_d][1], r$specimen_dilution_factor[diff_d][1], idl[diff_d][1]))
  }
  if ("instrument_source" %in% names(d)) {
    isr <- trimws(as.character(d$instrument_source))
    diff_s <- !is.na(isr) & nzchar(isr) & nzchar(r$specimen_source) &
      toupper(isr) != toupper(r$specimen_source)
    if (any(diff_s))
      add("warning", "source", sprintf(
          "%d well(s) have a source that differs from the one in the instrument file (e.g. %s: design '%s', file '%s'). The design is used.",
          sum(diff_s), wl[diff_s][1], r$specimen_source[diff_s][1], isr[diff_s][1]))
  }

  # per-plate counts under the design, and what is still left to the rules
  pl <- unique(inv$plate_key)
  smry <- do.call(rbind, lapply(pl, function(p) {
    rr <- r$specimen_type[r$plateid == p]
    row <- data.frame(plate_key = p, stringsAsFactors = FALSE)
    for (t in AI_SPECIMEN_TYPES) row[[t]] <- sum(rr == t)
    row$rules <- sum(occ & !cov & inv$plate_key == p)
    row
  }))
  list(issues = if (length(iss)) do.call(rbind, iss) else none$issues,
       summary = smry, n_covered = nrow(r), n_open = n_open)
}
