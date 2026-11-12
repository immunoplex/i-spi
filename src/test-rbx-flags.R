# Tests for the two per-well quality fields read from Bio-Plex .rbx / .srbx files:
#   * the outlier (mask) flag of every well x analyte  -- byte 12 of the 24-byte
#     quantification record
#   * % aggregate beads, derived from each well's total and gated event counts
# The real-file checks use dev/PIH_plate_data (not in the repository's tracked
# files) and are skipped when it is absent.
library(testthat)
.rf_find <- function(...) for (p in c(file.path(...), file.path("..", ...))) if (file.exists(p)) return(p)
.rf_src <- function(f) for (p in c(file.path("src", f), f)) if (file.exists(p)) return(invisible(source(p)))
if (!exists("parse_rbx")) .rf_src("rbx_binary_parser.R")
.pih <- function(name) .rf_find("dev", "PIH_plate_data", name)

test_that("rbx_pct_agg is the share of events outside the gate", {
  expect_equal(rbx_pct_agg(1084, 943), 100 * (1084 - 943) / 1084)
  expect_equal(rbx_pct_agg(1000, 1000), 0)
  for (bad in list(c(NA, 5), c(100, NA), c(0, 0), c(100, 150), c(-5, 1), c(NA_real_, NA_real_)))
    expect_true(is.na(rbx_pct_agg(bad[1], bad[2])))
  expect_true(is.na(rbx_pct_agg(NULL, 1))); expect_true(is.na(rbx_pct_agg(c(1, 2), 1)))
})

test_that("parse_quantification keeps the flag of each analyte record, and only 0 or 1", {
  # one synthetic well: 2 regions, 24-byte records <region><count=1><i32><flag><f64 median>
  mk <- function(region, flag, med) c(writeBin(as.integer(c(region, 1L, 0L, flag)), raw(), size = 4L), writeBin(med, raw(), size = 8L))
  rec <- c(mk(1L, 0L, 50), mk(3L, 1L, 120))
  raw <- c(as.raw(rep(0, 8)), rec, as.raw(rep(0, 64)))
  w <- parse_quantification(raw, c(1L, 3L), length(raw))
  expect_length(w, 1L)
  expect_equal(as.numeric(w[[1]]), c(50, 120)); expect_equal(attr(w[[1]], "outlier"), c(0L, 1L))
  rec2 <- c(mk(1L, 7L, 50), mk(3L, 1L, 120))                         # an unexpected value reads as unknown, not 7
  w2 <- parse_quantification(c(as.raw(rep(0, 8)), rec2, as.raw(rep(0, 64))), c(1L, 3L), 8L + 48L + 64L)
  expect_equal(attr(w2[[1]], "outlier"), c(NA, 1L))
})

test_that("a real plate: every well has a flag per analyte, and % aggregate is derived from the event counts", {
  f <- .pih("PIH 08JAN18 Plate 8.rbx"); skip_if(is.null(f))
  doc <- suppressMessages(parse_rbx(f)); lg <- long_dataframe(doc)
  expect_true(all(c("outlier", "pct_agg_beads") %in% names(lg)))
  expect_true(all(lg$outlier %in% c(0L, 1L)))
  a2 <- lg[lg$well == "A2", ][1, ]
  expect_equal(a2$pct_agg_beads, 100 * (a2$total_events - a2$gated_events) / a2$total_events)
  has <- !is.na(lg$pct_agg_beads)
  expect_true(mean(has) > 0.98); expect_true(all(lg$pct_agg_beads[has] > 0 & lg$pct_agg_beads[has] < 40))
  expect_true(all(is.na(lg$pct_agg_beads[lg$well == "A1"])))       # the first-read well stores no event counts
})

test_that("the outlier flag equals the vendor Outlier sheet cell for cell (plates exported from the same .rbx)", {
  zipf <- .pih("PIH_excel_sheets_(MASKED).zip"); skip_if(is.null(zipf))
  skip_if_not(requireNamespace("openxlsx", quietly = TRUE))
  td <- tempfile("pihx"); dir.create(td); on.exit(unlink(td, recursive = TRUE), add = TRUE)
  utils::unzip(zipf, exdir = td)
  truth <- function(xlsx) {
    d <- openxlsx::read.xlsx(xlsx, "Outlier", colNames = FALSE, skipEmptyRows = FALSE)
    hdr <- which(d[[1]] == "Type")[1]
    an <- sub(" \\(.*$", "", unlist(d[hdr - 1, 4:ncol(d)]))
    rows <- d[(hdr + 1):nrow(d), ]; rows <- rows[!is.na(rows[[2]]) & grepl("^[A-P][0-9]", rows[[2]]), ]
    out <- list()
    for (i in seq_len(nrow(rows))) for (w in trimws(strsplit(rows[i, 2], ",")[[1]]))
      out[[length(out) + 1L]] <- data.frame(well = w, analyte = an, flag = suppressWarnings(as.integer(unlist(rows[i, 4:ncol(d)]))), stringsAsFactors = FALSE)
    unique(do.call(rbind, out))
  }
  for (nm in c("PIH 08Mar18 Plate 17 Reanalysed 31Jan2019", "PIH 12Dec17 Plate 6 30mins Reanalysed 30Jan2019", "PIH IgG 07Nov17 Plate 3 Reanalysed 30Jan2019")) {
    f <- .pih(paste0(nm, ".rbx")); x <- list.files(td, pattern = paste0("^", gsub("([.()])", "\\\\\\1", nm), " MASKED.*xlsx$"), full.names = TRUE)
    skip_if(is.null(f) || !length(x))
    lg <- long_dataframe(suppressMessages(parse_rbx(f)))
    m <- merge(unique(lg[, c("well", "analyte", "outlier")]), truth(x[1]), by = c("well", "analyte"))
    expect_true(nrow(m) >= 500, info = nm)
    expect_true(sum(m$flag == 1) > 20, info = nm)                    # the plate really has flagged points
    expect_equal(m$outlier, m$flag, info = nm)
  }
})

test_that("the .rbx reader exposes pctaggbeads and the mask flag on the long response table", {
  f <- .pih("PIH 08Mar18 Plate 17 Reanalysed 31Jan2019.rbx"); skip_if(is.null(f))
  skip_if_not(requireNamespace("dplyr", quietly = TRUE) && requireNamespace("tidyr", quietly = TRUE))
  # load the reader's functions without its registration block (that needs the whole app)
  rp <- if (file.exists("reader_bead_rbx.R")) "reader_bead_rbx.R" else file.path("src", "reader_bead_rbx.R")
  for (e in parse(rp)) if (!(is.call(e) && identical(e[[1]], as.name("local")))) eval(e, globalenv())
  stubs <- list(clean_antigen_label = function(x) gsub("[^A-Za-z0-9._]", "", gsub(" ", ".", gsub("/", "_", x))),
                clean_plate_id = function(p) gsub("[^A-Za-z0-9]+", ".", sub("[.]rbx$", "", p)),
                assign_plate_numbers = function(h) h)
  added <- character(0)
  for (n in names(stubs)) if (!exists(n, envir = globalenv(), inherits = FALSE)) { assign(n, stubs[[n]], envir = globalenv()); added <- c(added, n) }
  on.exit(for (n in added) rm(list = n, envir = globalenv()), add = TRUE)
  p <- suppressMessages(process_rbx_files(data.frame(name = basename(f), datapath = f, stringsAsFactors = FALSE)))
  a <- p$assay_response_long
  expect_true(all(c("pctaggbeads", "masked", "mask_reason") %in% names(a)))
  expect_true(mean(!is.na(a$pctaggbeads)) > 0.98)
  expect_equal(length(unique(round(a$pctaggbeads[a$well == "C1" & !is.na(a$pctaggbeads)], 4))), 1L)  # one value per well, repeated per analyte
  expect_true(sum(a$masked) > 20); expect_true(all(is.na(a$mask_reason[!a$masked])))
  expect_match(unique(a$mask_reason[a$masked]), "outlier flag", fixed = TRUE)
  expect_true(all(a$masked[a$well == "C1"]))                          # S1 is flagged for every analyte on this plate
})

# ---- the masks and % aggregate beads reach the frames that are inserted -----------------------
test_that("the assembler writes masked / mask_reason / pctaggbeads to the specimen frames, standards masked from the start", {
  .rf_src("assay_import_contract.R")
  stubs <- list(clean_antigen_label = function(x) gsub("[^A-Za-z0-9._]", "", gsub(" ", ".", gsub("/", "_", x))),
                create_column_mapping = function() NULL, apply_column_mapping = function(df, m) df)
  added <- character(0)
  for (n in names(stubs)) if (!exists(n, envir = globalenv(), inherits = FALSE)) { assign(n, stubs[[n]], envir = globalenv()); added <- c(added, n) }
  on.exit(for (n in added) rm(list = n, envir = globalenv()), add = TRUE)

  pm <- data.frame(study_name = "S", experiment_name = "E", plateid = "P1", well = c("A1", "B1", "C1", "D1"),
                   specimen_type = c("B", "S", "S", "X"), stringsAsFactors = FALSE)
  ar <- data.frame(study_name = "S", experiment_name = "E", plateid = "P1", well = rep(pm$well, each = 2),
                   antigen = rep(c("PTx", "PRN"), 4), feature = "Net_MFI", assay_response = seq(10, 80, by = 10), assay_bead_count = 50L,
                   pctaggbeads = rep(c(NA, 12.5, 14, 9), each = 2),
                   masked = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE),
                   mask_reason = c(NA, NA, "Bio-Plex Manager outlier flag (.rbx)", NA, NA, "", NA, NA), stringsAsFactors = FALSE)
  out <- assemble_upload_frames(list(assay_response_long = ar, plates_map = pm), list(project_id = 1, study = "S", experiment = "E"))
  std <- out$standards; blk <- out$blanks; smp <- out$samples
  expect_true(all(c("masked", "mask_reason", "pctaggbeads") %in% names(std)))
  expect_equal(nrow(std), 4L)                                              # 2 standard wells x 2 analytes
  b1 <- std[std$well == "B1", ]; c1 <- std[std$well == "C1", ]
  expect_equal(b1$masked[b1$antigen == "PTx"], TRUE)                       # masked per analyte, so curves for other antigens are untouched
  expect_equal(b1$masked[b1$antigen == "PRN"], FALSE)
  expect_equal(b1$mask_reason[b1$antigen == "PTx"], "Bio-Plex Manager outlier flag (.rbx)") 
  expect_equal(b1$mask_reason[b1$antigen == "PRN"], NA_character_)                          # unmasked: no reason
  expect_equal(c1$mask_reason[c1$antigen == "PTx"], "masked at import")                   # masked, the reader gave none
  expect_equal(c1$mask_reason[c1$antigen == "PRN"], "masked at import")                   # an empty reason counts as none
  expect_equal(std$pctaggbeads[std$well == "B1"][1], 12.5)

  expect_equal(blk$mask_reason, c(NA_character_, NA_character_)); expect_false(any(blk$masked))
  expect_type(std$masked, "logical"); expect_false(anyNA(std$masked)); expect_false(anyNA(smp$masked))
  # a row carrying the reader's reason keeps it
  ar2 <- ar; ar2$masked[1:2] <- TRUE; ar2$mask_reason[1:2] <- "Bio-Plex Manager outlier flag (.rbx)"
  blk2 <- assemble_upload_frames(list(assay_response_long = ar2, plates_map = pm), list(project_id = 1, study = "S", experiment = "E"))$blanks
  expect_equal(unique(blk2$mask_reason), "Bio-Plex Manager outlier flag (.rbx)")
  # a reader with no flags at all: the columns are not invented (the DB default applies)
  ar3 <- ar[, setdiff(names(ar), c("masked", "mask_reason"))]
  expect_false(any(c("masked", "mask_reason") %in% names(assemble_upload_frames(list(assay_response_long = ar3, plates_map = pm), list(project_id = 1, study = "S", experiment = "E"))$standards)))
  # an unknown flag (NA) is not a mask
  ar4 <- ar; ar4$masked[3] <- NA
  s4 <- assemble_upload_frames(list(assay_response_long = ar4, plates_map = pm), list(project_id = 1, study = "S", experiment = "E"))$standards
  expect_false(anyNA(s4$masked))
})

test_that("the layout template keeps pctaggbeads, masked and mask_reason on the long response sheet", {
  src <- paste(readLines(.rf_find("src", "generate_layout_template_ref.R") %||% "generate_layout_template_ref.R", warn = FALSE), collapse = "\n")
  expect_match(src, '"pctaggbeads", "masked", "mask_reason"', fixed = TRUE)
})
