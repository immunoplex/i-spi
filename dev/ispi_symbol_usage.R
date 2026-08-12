#!/usr/bin/env Rscript
# =============================================================================
# ispi_symbol_usage.R  --  removal-safety call graph for the std-curve refactor
# -----------------------------------------------------------------------------
# PURPOSE
#   Answer "what old code can I remove NOW, and what must I migrate FIRST?" for
#   the standard-curve subsystem. For every function + output defined in the
#   legacy fitting/UI files, it finds all references across the whole repo and
#   classifies each symbol:
#     SAFE           -> referenced only within the legacy set  => remove now
#     MIGRATE-FIRST  -> referenced by a kept/consumer file      => a cross-tab
#                       dependency (dilution, study overview, summary, ...) that
#                       must move to the calib_* boundary before removal
#   It also lists every stanassay and live-fit reference to excise, and every
#   legacy output id that is consumed cross-tab.
#
# WHY: removal is only safe where a symbol has no consumers outside the code we
#   are deleting. This produces that consumer map from the real repo (the app
#   files this assistant can see are only a fraction of the 64).
#
#   Base R only, read-only. Same conventions as ispi_diagnostic.R.
#
# USAGE
#   Rscript ispi_symbol_usage.R [repo_root] [out_dir]
#
# OUTPUTS (in out_dir)
#   removal_symbols.csv   symbol | kind | def_file | refs_total | refs_external |
#                         external_files | verdict
#   cross_tab_outputs.csv legacy output ids referenced outside their def file
#   special_refs.csv      every stanassay / live-fit reference (file, line, text)
#   file_readiness.csv    per legacy file: n symbols, n safe, n blocked, verdict
#   REPORT_removal.md     the actionable read-out
# =============================================================================

args      <- commandArgs(trailingOnly = TRUE)
repo_root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else getwd()
out_dir   <- if (length(args) >= 2 && nzchar(args[2])) args[2] else file.path(repo_root, "ispi_symbol_usage_out")
repo_root <- normalizePath(repo_root, mustWork = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- 1. Configuration -------------------------------------------------------
# The "old interactive refitting" set we intend to remove. Symbols DEFINED in
# these files are the removal candidates; every OTHER .R file is a "consumer"
# whose reference to such a symbol blocks removal until migrated.
# Tune this list as you decide a file is legacy vs shared (e.g. plot_functions.R
# has LOQ overlays used broadly -- left OUT of legacy by default = treated as a
# consumer, so you can SEE what still leans on it).
legacy_files <- c(
  "std_curver_ui.R",
  "std_curve_functions.R",
  "batch_fit_functions.R",
  "model_functions.R",
  "bayes_concentration_functions.R",
  "propagate_functions.R"
)

# Files that are themselves being retired for other reasons (never count as
# "external consumers" -- their references don't block removal).
also_dead <- c("std_curve_bayes_concentration.R", "revised_batch_upload_code.R",
               "dilution_analysis_ui-original.R", "delete_plate.R",
               "import_lumifile_flowjo_patch.R",
               # unsourced/dead in app.R (commented out) -> not real consumers:
               "se_x_robust_fix.R", "segment_reader.R")

# This tool's own scripts: never app code, so exclude from the scan entirely
# (otherwise their regex/comment text looks like it "consumes" legacy symbols).
tooling_files <- c("ispi_diagnostic.R", "ispi_db_diagnostic.R", "ispi_symbol_usage.R")

# Names that are almost always call-arguments / anonymous callbacks, not real
# top-level defs. Requiring `<-` below removes most; this catches stragglers.
ARG_BLOCKLIST <- c("error", "warning", "interrupt", "finally",
                   "onFulfilled", "onRejected", "isTRUE", "FUN",
                   "filename", "fmt", "tr")

# Symbols to always locate regardless of where defined (things to excise).
STANASSAY_RE <- "StanAssay|stanassay|cdan_profile"
LIVEFIT_RE   <- "\\bnlsLM\\b|nls_multstart|nls\\.multstart|\\bdrda\\s*\\(|\\bdrm\\s*\\(|\\bsampling\\s*\\(|jags\\.model|\\brjags\\b|StanAssay|fit_experiment_plate_batch|create_batch_fit_outputs|process_batch_outputs"

# Require `<-`/`<<-` (not bare `=`), so `name = function(...)` call-arguments
# (tryCatch error=, apply FUN=, promise onFulfilled=) are NOT treated as defs.
RE_FUNDEF  <- "^\\s*([A-Za-z._][A-Za-z0-9._]*)\\s*<<?-\\s*function\\b"
RE_OUTPUT  <- "output\\$([A-Za-z0-9_.]+)"

# ---- 2. Load files ----------------------------------------------------------
all_paths <- list.files(repo_root, pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
all_paths <- all_paths[!grepl("/(renv|packrat|dead)/", all_paths)]  # ignore retired code in dead/
all_paths <- all_paths[!(basename(all_paths) %in% tooling_files)]  # drop our own tools
rel <- function(p) sub(paste0("^", repo_root, "/?"), "", p)
base <- function(p) basename(p)

files <- lapply(all_paths, function(p)
  tryCatch(readLines(p, warn = FALSE, encoding = "UTF-8"),
           error = function(e) readLines(p, warn = FALSE)))
names(files) <- all_paths

# code-only view: drop full-line comments (trailing comments left; minor noise)
code_lines <- lapply(files, function(ls) ls[!grepl("^\\s*#", ls)])

is_legacy   <- base(all_paths) %in% legacy_files
is_dead     <- base(all_paths) %in% also_dead
legacy_set  <- all_paths[is_legacy]

# R identifiers can only contain letters, digits, '_' and '.', so the only
# regex metacharacter a symbol can carry is '.' -- escape just that. (The older
# full-metachar class tripped TRE with "Invalid contents of {}".)
esc <- function(x) gsub("([.])", "\\\\\\1", x)

# reference test: does a symbol appear on a code line of a file?
refs_in <- function(sym, path) {
  pat <- paste0("\\b", esc(sym), "\\b")
  any(grepl(pat, code_lines[[path]], perl = TRUE))
}
ref_line_hits <- function(pattern, path) {
  ls <- files[[path]]; which(grepl(pattern, ls, perl = TRUE))
}
first_cap <- function(line, pat) {
  m <- regmatches(line, regexec(pat, line, perl = TRUE))[[1]]
  if (length(m) >= 2) m[2] else NA_character_
}

# Every function name DEFINED in each file (across the whole repo). Used to skip
# "consumers" that define their own same-named function -- a lexical name clash,
# not a real dependency on the legacy definition.
defs_by_file <- lapply(all_paths, function(p) {
  ls <- files[[p]]; hits <- ls[grepl(RE_FUNDEF, ls, perl = TRUE)]
  unique(na.omit(vapply(hits, function(l) first_cap(l, RE_FUNDEF), character(1))))
})
names(defs_by_file) <- all_paths

# ---- 3. Build symbol table from legacy files --------------------------------
sym_rows <- list()
for (p in legacy_set) {
  ls <- files[[p]]
  # functions (top-level and nested both count as "defined here")
  for (i in which(grepl(RE_FUNDEF, ls, perl = TRUE))) {
    nm <- first_cap(ls[i], RE_FUNDEF)
    if (!is.na(nm) && !(nm %in% ARG_BLOCKLIST)) sym_rows[[length(sym_rows)+1]] <-
      data.frame(symbol = nm, kind = "function", def_file = rel(p), stringsAsFactors = FALSE)
  }
}
# extract output ids from legacy files
for (p in legacy_set) {
  ls <- files[[p]]
  hits <- unlist(regmatches(ls, gregexpr(RE_OUTPUT, ls, perl = TRUE)))
  ids <- unique(sub("output\\$", "", hits))
  for (id in ids) sym_rows[[length(sym_rows)+1]] <-
    data.frame(symbol = id, kind = "output", def_file = rel(p), stringsAsFactors = FALSE)
}
symbols <- if (length(sym_rows)) unique(do.call(rbind, sym_rows)) else
  data.frame(symbol=character(), kind=character(), def_file=character())

# ---- 4. Classify each symbol by external usage ------------------------------
verdict <- character(nrow(symbols))
ext_files_col <- character(nrow(symbols))
refs_total_col <- integer(nrow(symbols))
refs_ext_col   <- integer(nrow(symbols))

for (r in seq_len(nrow(symbols))) {
  sym  <- symbols$symbol[r]
  kind <- symbols$kind[r]
  def  <- symbols$def_file[r]
  ext <- character(0); n_total <- 0L
  for (p in all_paths) {
    bn <- base(p)
    hit <- if (kind == "output") {
      # output id referenced as a quoted string outside its def file
      any(grepl(paste0("[\"']", esc(sym), "[\"']"), code_lines[[p]], perl = TRUE))
    } else refs_in(sym, p)
    if (!hit) next
    n_total <- n_total + 1L
    # A file that defines its OWN function of this name isn't consuming ours.
    shadows <- kind == "function" && (sym %in% defs_by_file[[p]]) && rel(p) != def
    if (!(bn %in% legacy_files) && !(bn %in% also_dead) && !shadows) ext <- c(ext, rel(p))
  }
  refs_total_col[r] <- n_total
  refs_ext_col[r]   <- length(ext)
  ext_files_col[r]  <- paste(ext, collapse = "; ")
  verdict[r] <- if (length(ext) == 0) "SAFE" else "MIGRATE-FIRST"
}
symbols$refs_total    <- refs_total_col
symbols$refs_external <- refs_ext_col
symbols$external_files<- ext_files_col
symbols$verdict       <- verdict

# ---- 5. Special reference sweep (stanassay + live-fit) ----------------------
special <- list()
for (p in all_paths) {
  for (nm in c("stanassay","live_fit")) {
    pat <- if (nm=="stanassay") STANASSAY_RE else LIVEFIT_RE
    for (ln in ref_line_hits(pat, p)) {
      txt <- trimws(gsub("\\s+"," ", files[[p]][ln]))
      special[[length(special)+1]] <- data.frame(
        kind = nm, file = rel(p), line = ln,
        text = substr(txt, 1, 160), stringsAsFactors = FALSE)
    }
  }
}
special_df <- if (length(special)) do.call(rbind, special) else NULL

# ---- 6. Cross-tab outputs ---------------------------------------------------
cross_out <- symbols[symbols$kind=="output" & symbols$refs_external>0,
                     c("symbol","def_file","external_files")]

# ---- 7. Per-file readiness --------------------------------------------------
fr <- list()
for (p in legacy_set) {
  sub <- symbols[symbols$def_file==rel(p), ]
  n <- nrow(sub); safe <- sum(sub$verdict=="SAFE"); blk <- n - safe
  fr[[length(fr)+1]] <- data.frame(
    file = rel(p), symbols = n, safe = safe, blocked = blk,
    verdict = if (blk==0) "DELETE OK" else "EXTRACT/MIGRATE FIRST",
    stringsAsFactors = FALSE)
}
readiness <- if (length(fr)) do.call(rbind, fr) else NULL

# ---- 8. Write CSVs ----------------------------------------------------------
wr <- function(df,n) if (!is.null(df) && nrow(df)) write.csv(df, file.path(out_dir,n), row.names = FALSE)
wr(symbols[order(symbols$def_file, -symbols$refs_external), ], "removal_symbols.csv")
wr(cross_out,   "cross_tab_outputs.csv")
wr(special_df,  "special_refs.csv")
wr(readiness,   "file_readiness.csv")

# ---- 9. REPORT_removal.md ---------------------------------------------------
md <- c(); add <- function(...) md <<- c(md, paste0(...))
add("# Standard-curve removal-safety report")
add("")
add(sprintf("- Repo: `%s`  |  generated %s", repo_root, format(Sys.time(), "%Y-%m-%d %H:%M")))
add(sprintf("- Legacy files analyzed: %s", paste(legacy_files, collapse=", ")))
add(sprintf("- Symbols: **%d**  (SAFE to remove: **%d**  |  migrate-first: **%d**)",
            nrow(symbols), sum(symbols$verdict=="SAFE"), sum(symbols$verdict=="MIGRATE-FIRST")))
add("")

add("## Per-file readiness")
add("")
if (!is.null(readiness)) {
  add("| file | symbols | safe | blocked | verdict |")
  add("|---|--:|--:|--:|---|")
  for (i in seq_len(nrow(readiness))) { r <- readiness[i,]
    add(sprintf("| `%s` | %d | %d | %d | **%s** |", r$file, r$symbols, r$safe, r$blocked, r$verdict)) }
}
add("")

add("## Migrate-first (cross-tab dependencies to resolve BEFORE removal)")
add("*Each symbol below is still called by a file outside the legacy set.*")
add("")
blk <- symbols[symbols$verdict=="MIGRATE-FIRST", ]
blk <- blk[order(blk$def_file, blk$symbol), ]
if (nrow(blk)) {
  add("| symbol | kind | defined in | external consumers |")
  add("|---|---|---|---|")
  for (i in seq_len(nrow(blk))) { r <- blk[i,]
    add(sprintf("| `%s` | %s | `%s` | %s |", r$symbol, r$kind, r$def_file, r$external_files)) }
} else add("_None -- every legacy symbol is unused externally; the files can be removed._")
add("")

add("## stanassay references to excise")
add("")
if (!is.null(special_df)) {
  sa <- special_df[special_df$kind=="stanassay", ]
  if (nrow(sa)) for (i in seq_len(nrow(sa))) add(sprintf("- `%s:%d`  %s", sa$file[i], sa$line[i], sa$text[i]))
  else add("_No stanassay references found._")
}
add("")

add("## Cross-tab legacy outputs (UI ids used outside their def file)")
add("")
if (nrow(cross_out)) {
  add("| output id | defined in | consumers |")
  add("|---|---|---|")
  for (i in seq_len(nrow(cross_out))) { r <- cross_out[i,]
    add(sprintf("| `%s` | `%s` | %s |", r$symbol, r$def_file, r$external_files)) }
} else add("_No legacy output ids are referenced cross-tab._")
add("")

add("## How to use this")
add("")
add("1. Files marked **DELETE OK** have no external consumers -- move to dead/ now.")
add("2. For **MIGRATE-FIRST** symbols, repoint each listed consumer to the calib_*")
add("   boundary (fetch_calib_*), then re-run this diagnostic; the symbol flips to")
add("   SAFE once its external refs hit zero.")
add("3. Excise every stanassay reference listed (incl. the library load in global.R).")
add("")
add("> Heuristic caveats: references are matched lexically on code lines (full-line")
add("> comments excluded; trailing comments/strings may still match). Dynamic")
add("> `output[[paste0(...)]]` ids and `get()/do.call()` string calls can be missed.")
writeLines(md, file.path(out_dir, "REPORT_removal.md"))

message("symbols: ", nrow(symbols),
        " | SAFE: ", sum(symbols$verdict=="SAFE"),
        " | MIGRATE-FIRST: ", sum(symbols$verdict=="MIGRATE-FIRST"))
message("wrote report + CSVs to ", normalizePath(out_dir))
