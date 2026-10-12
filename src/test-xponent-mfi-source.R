# =============================================================================
# test-xponent-mfi-source.R
# -----------------------------------------------------------------------------
# The xPONENT reader lets the user choose which response block becomes
# assay_response (Net MFI | Median | Average Net MFI). Which blocks a file has,
# and in what order, depends on the lab's export setup, so these tests build
# variants of the real dev/ export with its blocks moved, dropped and edited.
#
# Run from src/:  Rscript -e "testthat::test_file('test-xponent-mfi-source.R')"
# =============================================================================

library(testthat)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(janitor) })

if (!exists("xponent_mfi_long")) source("reader_bead_xponent_parsers.R")

SRC_CSV <- "../dev/xPONENT_20221003_AGA_1_2000_IgG1_Plate_1.csv"
skip_if_no_fixture <- function() {
  skip_if_not(file.exists(SRC_CSV), "dev/ xPONENT sample not found (run from src/)")
  skip_if_not_installed("moach")
}

# Split the "Results" part of an export into its blocks (separated by lines that
# are nothing but delimiters). Returns list(head, blocks) with block names taken
# from the "DataType:,<name>" line. The last block also carries the CRC line, so
# tests only ever move/drop the first four (Median, Net MFI, Count, Avg Net MFI).
.split_blocks <- function(lines) {
  r <- grep("^Results", lines)[1]
  head <- lines[seq_len(r)]
  body <- lines[-seq_len(r)]
  sep  <- grepl("^[,;]*$", body)
  grp  <- cumsum(sep)
  blocks <- split(body[!sep], grp[!sep])
  names(blocks) <- vapply(blocks, function(b) sub("^DataType:,([^,]*).*$", "\\1", b[1]), "")
  list(head = head, blocks = blocks)
}
.write_blocks <- function(parts, order, drop = character(), path = tempfile(fileext = ".csv")) {
  b <- parts$blocks[setdiff(order, drop)]
  out <- parts$head
  for (nm in names(b)) out <- c(out, ",,,,,,,,,,,,,,,,", b[[nm]])
  writeLines(out, path)
  path
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# The dev/ export's protocol defines no background, so its Net MFI block equals
# its Median block everywhere -- which would let a reader that picked the wrong
# block (or the wrong POSITION) pass unnoticed. Give the copy a "background" of
# BG: Net MFI = Median - BG, Avg Net MFI likewise (as xPONENT derives them).
BG <- 10
.subtract_bg <- function(lines, section, keyed_by_location) {
  s <- grep(paste0("^DataType:,", section), lines)[1]
  e <- s + which(grepl("^[,;]*$", lines[-seq_len(s)]))[1] - 1L
  for (i in seq(s + 2L, e)) {
    m <- if (keyed_by_location) regmatches(lines[i], regexec("^(\"[^\"]*\",[^,]*),(.*)$", lines[i]))[[1]]
         else                   regmatches(lines[i], regexec("^([^,]*),(.*)$", lines[i]))[[1]]
    f <- strsplit(m[3], ",", fixed = TRUE)[[1]]
    v <- suppressWarnings(as.numeric(f))
    f[!is.na(v)] <- v[!is.na(v)] - BG            # leave empty / non-numeric cells untouched
    lines[i] <- paste0(m[2], ",", paste(f, collapse = ","))
  }
  lines
}
SRC_BG <- local({
  path <- NULL
  function() {
    if (is.null(path)) {
      l <- readLines(SRC_CSV, warn = FALSE)
      l <- .subtract_bg(l, "Net MFI", TRUE)
      l <- .subtract_bg(l, "Avg Net MFI", FALSE)
      path <<- tempfile(fileext = ".csv"); writeLines(l, path)
    }
    path
  }
})

fixture <- function(order = NULL, drop = character(), from = SRC_BG()) {
  parts <- .split_blocks(readLines(from, warn = FALSE))
  all <- names(parts$blocks)
  .write_blocks(parts, order %||% all, drop)
}

read_x <- function(path) suppressWarnings(moach::read_Xponent_csv(path))

test_that("sections are reported in FILE order, whatever that order is", {
  skip_if_no_fixture()
  expect_equal(head(xponent_file_sections(SRC_CSV), 4),
               c("Median", "Net MFI", "Count", "Avg Net MFI"))
  p <- fixture(order = c("Net MFI", "Avg Net MFI", "Median", "Count", "Units", "Per Bead Count",
                         "Dilution Factor", "Analysis Types", "Audit Logs", "Warnings/Errors"))
  expect_equal(head(xponent_file_sections(p), 4), c("Net MFI", "Avg Net MFI", "Median", "Count"))
  expect_setequal(xponent_mfi_sources_in(xponent_file_sections(p)),
                  c("net_mfi", "median", "avg_net_mfi"))
})

test_that("each source reads its own block; Net MFI and Median genuinely differ", {
  skip_if_no_fixture()
  x <- read_x(SRC_BG())
  net <- xponent_mfi_long(x, "net_mfi"); med <- xponent_mfi_long(x, "median")
  avg <- xponent_mfi_long(x, "avg_net_mfi")
  expect_equal(net$mfi, x$AssayData$Exprs$Net_MFI)
  expect_equal(med$mfi, x$AssayData$Exprs$Median)
  expect_equal(as.numeric(net$mfi), as.numeric(med$mfi) - BG)   # the choice is not cosmetic
  expect_equal(as.numeric(avg$mfi), as.numeric(net$mfi))         # unique sample names: avg == net
  expect_equal(names(net), c("Location", "Sample", "Analyte", "Count", "mfi"))
})

test_that("the default source is Net MFI, exactly as the reader always behaved", {
  skip_if_no_fixture()
  x <- read_x(SRC_BG())
  expect_identical(xponent_mfi_long(x)$mfi, x$AssayData$Exprs$Net_MFI)
  expect_false(isTRUE(all.equal(as.numeric(xponent_mfi_long(x)$mfi),
                                as.numeric(x$AssayData$Exprs$Median))))  # not Median
  long <- extract_xponent_assay_response_long(x, "f.csv")
  expect_equal(long$assay_response, as.numeric(x$AssayData$Exprs$Net_MFI))
  expect_equal(unique(long$feature), "Net_MFI")
  expect_equal(.xponent_mfi_key(NULL), "net_mfi")
  expect_equal(.xponent_mfi_key(""), "net_mfi")
})

test_that("wide and long outputs follow the chosen source, in the same well order", {
  skip_if_no_fixture()
  x <- read_x(SRC_BG())
  an <- "H1_cal09_Gen6"
  got <- list()
  for (k in c("net_mfi", "median", "avg_net_mfi")) {
    wide <- xponent_to_wide_plate_data(x, "f.csv", k)
    long <- extract_xponent_assay_response_long(x, "f.csv", k)
    ex   <- x$AssayData$Exprs
    col  <- if (k == "avg_net_mfi") "Net_MFI" else XPONENT_MFI_SOURCES[[k]]$column
    want <- as.numeric(ex[[col]][ex$Location == "P24" & ex$Analyte == an])
    expect_equal(as.numeric(wide[[an]][wide$Well == "P24"]), want, info = k)
    expect_equal(long$assay_response[long$well == "P24" & long$antigen == an], want, info = k)
    expect_equal(unique(long$feature), XPONENT_MFI_SOURCES[[k]]$column)
    got[[k]] <- wide
  }
  expect_equal(got$net_mfi$Well, got$median$Well)
  expect_equal(got$net_mfi$Well, got$avg_net_mfi$Well)
  expect_false(isTRUE(all.equal(got$net_mfi[[an]], got$median[[an]])))
})

test_that("block ORDER does not change what any source returns", {
  skip_if_no_fixture()
  base <- read_x(SRC_BG())
  rest <- c("Units", "Per Bead Count", "Dilution Factor", "Analysis Types", "Audit Logs", "Warnings/Errors")
  for (ord in list(c("Net MFI", "Median", "Count", "Avg Net MFI"),
                   c("Avg Net MFI", "Count", "Net MFI", "Median"))) {
    moved <- read_x(fixture(order = c(ord, rest)))
    for (k in names(XPONENT_MFI_SOURCES))
      expect_identical(xponent_mfi_long(moved, k), xponent_mfi_long(base, k),
                       info = paste(k, "/", paste(ord, collapse = ">")))
  }
})

test_that("Average Net MFI is keyed by sample name: replicate wells share one value", {
  skip_if_no_fixture()
  # Make wells A1 and B1 replicates ("Unknown1"): rename in every well-keyed
  # block, drop Unknown2's Average row and give Unknown1 their mean (analyte 1).
  lines <- readLines(SRC_CSV, warn = FALSE)
  lines <- sub("^(\"2\\(1,B1\\)\"),Unknown2,", "\\1,Unknown1,", lines)
  avg_start <- grep("^DataType:,Avg Net MFI", lines)
  avg_end   <- avg_start + which(grepl("^[,;]*$", lines[-seq_len(avg_start)]))[1] - 1L
  i1 <- avg_start + 1L + which(grepl("^Unknown1,", lines[(avg_start + 2L):avg_end]))[1]
  i2 <- avg_start + 1L + which(grepl("^Unknown2,", lines[(avg_start + 2L):avg_end]))[1]
  lines[i1] <- sub("^Unknown1,[0-9.]+,", "Unknown1,130.25,", lines[i1])
  lines <- lines[-i2]
  p <- tempfile(fileext = ".csv"); writeLines(lines, p)

  x   <- read_x(p)
  avg <- xponent_mfi_long(x, "avg_net_mfi")
  an  <- "SARSCOV2_HEXAPRO"
  rep_vals <- avg$mfi[avg$Analyte == an & avg$Location %in% c("A1", "B1")]
  expect_equal(rep_vals, c(130.25, 130.25))
  # ... while Net MFI keeps the wells distinct, and row count is untouched
  net <- xponent_mfi_long(x, "net_mfi")
  expect_equal(sort(net$mfi[net$Analyte == an & net$Location %in% c("A1", "B1")]), c(105.5, 155))
  expect_equal(nrow(avg), nrow(net))
  expect_identical(avg$Location, net$Location)
})

test_that("a missing block is reported by name, per file, before anything is parsed", {
  skip_if_no_fixture()
  no_net <- fixture(drop = "Net MFI")
  expect_error(xponent_mfi_long(read_x(no_net), "net_mfi"), "no 'Net MFI' section.*Median")
  expect_equal(xponent_mfi_long(read_x(no_net), "median")$mfi,
               read_x(SRC_CSV)$AssayData$Exprs$Median)

  up <- data.frame(name = c("full.csv", "no_net.csv"), datapath = c(SRC_CSV, no_net))
  sc <- xponent_scan_upload(up)
  expect_equal(sc$lacking$net_mfi, "no_net.csv")
  expect_false("net_mfi" %in% sc$common)
  expect_setequal(sc$common, c("median", "avg_net_mfi"))
  # process_xponent_files refuses the batch up front, naming the file
  expect_error(process_xponent_files(up, "net_mfi"), "no_net\\.csv")
})

test_that("unknown source keys fail loudly instead of falling back silently", {
  expect_error(.xponent_mfi_key("net-mfi"), "Unknown MFI source")
})
