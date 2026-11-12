# =============================================================================
# test-assay-where.R
# -----------------------------------------------------------------------------
# The importer's "4. Result" table says WHERE each description is (plate and
# wells), and the plate grid's "Label characters" control defaults to 10 and
# offers up to 25.
#
# Run: testthat::test_file("src/test-assay-where.R")
# =============================================================================

library(testthat)
library(shiny)

if (!exists("ai_shape_server") || !exists("ai_where_text")) {
  root <- c(".", "src", "..", "../src", "../..", "../../src")
  hit  <- root[file.exists(file.path(root, "assay_shape_ui.R"))][1]
  if (is.na(hit)) stop("cannot find src/ -- run from the repo root")
  for (f in c("assay_description_parse.R", "assay_well_inventory.R",
              "assay_shape_rules.R", "assay_design_map.R", "assay_shape_ui.R"))
    source(file.path(hit, f))
}
.aw_src <- function(f) { for (p in c(f, file.path("src", f))) if (file.exists(p)) return(paste(readLines(p, warn = FALSE), collapse = "\n")) }

# ---- the helpers ------------------------------------------------------------
test_that("wells are listed in plate order, not text order", {
  w <- c("A10", "B1", "A2", "A1", "C12", "A9")
  expect_equal(w[ai_well_order(w)], c("A1", "A2", "A9", "A10", "B1", "C12"))
  expect_equal(c("AA1", "B1", "H12", "A1")[ai_well_order(c("AA1", "B1", "H12", "A1"))], c("A1", "B1", "H12", "AA1"))   # 1536-well rows
})

test_that("ai_where_text names each plate with its wells, and truncates", {
  expect_equal(ai_where_text(c("1. p1", "1. p1"), c("B1", "A1")), "1. p1: A1, B1")
  expect_equal(ai_where_text(c("p1", "p2", "p1"), c("C3", "A1", "A2")), "p1: A2, C3; p2: A1")     # plates in first-seen order
  many <- ai_where_text(rep("p1", 12), paste0("A", 1:12), max_wells = 5L)
  expect_equal(many, "p1: A1, A2, A3, A4, A5 (+7 more)")
  pl <- ai_where_text(paste0("p", 1:9), rep("A1", 9), max_plates = 3L)
  expect_equal(pl, "p1: A1; p2: A1; p3: A1; +6 more plates")
  expect_equal(ai_where_text("p1", "A1"), "p1: A1")
  expect_equal(ai_where_text(c("p1", "p1"), c("A1", "A1")), "p1: A1")                             # a well is listed once
  expect_equal(ai_where_text(character(), character()), "")
  expect_equal(ai_where_text(c("p1", NA), c(NA, "B2")), "?: B2")                                  # a missing plate is not dropped silently
})

test_that("ai_locate_strings counts wells and plates per string", {
  desc  <- c("a", "b", "a", "c", "a")
  plate <- c("p1", "p1", "p2", "p2", "p2")
  well  <- c("A1", "A2", "B1", "B2", "B3")
  r <- ai_locate_strings(c("a", "c", "zzz"), desc, plate, well)
  expect_equal(r$n_wells, c(3L, 1L, 0L)); expect_equal(r$n_plates, c(2L, 1L, 0L))
  expect_equal(r$where, c("p1: A1; p2: B1, B3", "p2: B2", ""))
  expect_equal(nrow(ai_locate_strings(character(), desc, plate, well)), 0L)
})

# ---- the Result table, through the module ----------------------------------------
mk_plate <- function(plateid, src, wells, types, desc)
  data.frame(Well = wells, Type = types, Description = desc, plateid = plateid, source_file = src, stringsAsFactors = FALSE)
two_plates <- function() {
  pv <- rbind(
    mk_plate("p1", "PIH Plate 1.xlsx", c("A1", "A2", "A10", "B1", "C1"), "X",
             c("1001_V1_100", "1002_V1_100", "1003_V1_100", "1004_V2_100", "ODD LABEL")),
    mk_plate("p2", "PIH Plate 2.xlsx", c("A1", "B2", "C3"), "X", c("1005_V1_100", "ODD LABEL", "1006_V3_100")))
  ai_well_inventory(list(preview = pv), "bead", n_wells = 96)
}

test_that("the Result table lists where each description is found", {
  testServer(ai_shape_server, args = list(inventory_rv = reactiveVal(two_plates()), enabled = reactive(TRUE)), {
    session$flushReact()
    ss <- state_of("X")
    odd <- ss$shapes$shape_key[ss$shapes$longest == "ODD LABEL"]
    expect_length(odd, 1L)
    sel_shape[["X"]] <- odd; session$flushReact()
    df <- preview_frames$X()
    expect_equal(names(df)[1:3], c("Description", "Wells", "Where (plate: wells)"))
    expect_equal(df$Description, "ODD LABEL")
    expect_equal(df$Wells, 2L)
    expect_equal(df$`Where (plate: wells)`, "1. p1: C1; 2. p2: B2")      # compact plate labels, plate order, well order
    expect_equal(df$OK, "no")
    # the incomplete-string message points at the same wells
    v <- gsub("<[^>]+>", "", as.character(output$verdict_X$html))
    expect_match(v, "ODD LABEL [1. p1: C1; 2. p2: B2]", fixed = TRUE)

    # the regular group: five wells on two plates, wells in natural order (A2 before A10)
    sel_shape[["X"]] <- ss$shapes$shape_key[ss$shapes$longest != "ODD LABEL"][1]; session$flushReact()
    d2 <- preview_frames$X()
    expect_true(all(d2$Wells >= 1L))
    row <- d2[d2$Description == "1001_V1_100", ]
    expect_equal(row$`Where (plate: wells)`, "1. p1: A1")
    expect_match(paste(d2$`Where (plate: wells)`, collapse = " | "), "A2", fixed = TRUE)
    expect_equal(sum(d2$Wells), 6L)
  })
})

test_that("many distinct strings are all listed (up to a generous cap), not just twelve", {
  pv <- mk_plate("p1", "PIH Plate 1.xlsx", paste0(rep(LETTERS[1:8], each = 12), 1:12), "X", sprintf("%04d_V1_100", 1:96))
  inv <- ai_well_inventory(list(preview = pv), "bead", n_wells = 96)
  testServer(ai_shape_server, args = list(inventory_rv = reactiveVal(inv), enabled = reactive(TRUE)), {
    session$flushReact()
    df <- preview_frames$X()
    expect_equal(nrow(df), 96L)
    expect_true(all(nzchar(df$`Where (plate: wells)`)))
  })
})

# ---- Label characters -----------------------------------------------------------------
test_that("Label characters defaults to 10 and offers 2 to 25", {
  skip_if_not(exists("AI_GRID_LABEL_CHOICES") || { for (p in c("assay_plate_grid.R", "src/assay_plate_grid.R")) if (file.exists(p)) { suppressMessages(source(p)); break }; exists("AI_GRID_LABEL_CHOICES") })
  if (!exists("help_icon")) { assign("help_icon", function(...) NULL, envir = globalenv()); on.exit(rm("help_icon", envir = globalenv()), add = TRUE) }
  skip_if_not_installed("shinyBS"); suppressMessages(library(shinyBS))
  expect_equal(AI_GRID_LABEL_DEFAULT, 10L)
  expect_equal(min(AI_GRID_LABEL_CHOICES), 2L); expect_equal(max(AI_GRID_LABEL_CHOICES), 25L)
  expect_true(AI_GRID_LABEL_DEFAULT %in% AI_GRID_LABEL_CHOICES); expect_false(is.unsorted(AI_GRID_LABEL_CHOICES))
  html <- as.character(ai_plate_grid_ui("pg"))
  expect_match(html, '<option value="10" selected>10</option>', fixed = TRUE)         # the default
  expect_match(html, '<option value="25">25</option>', fixed = TRUE)                  # the top of the list
  expect_false(grepl('<option value="26"', html, fixed = TRUE))
  expect_match(html, '<option value="auto">Auto', fixed = TRUE)                       # auto is still available
})

test_that("a wider label gets a wider well, so it is not clipped", {
  skip_if_not(exists("ai_grid_min_cell_px"))
  expect_true(ai_grid_min_cell_px(25, 9) > ai_grid_min_cell_px(10, 9))
  expect_true(ai_grid_min_cell_px(10, 10) > ai_grid_min_cell_px(10, 8))
  expect_gte(ai_grid_min_cell_px(10, 9), 10 * 9 * 0.5)                               # room for ten glyphs
  src <- .aw_src("assay_plate_grid.R")
  expect_match(src, "minmax(%dpx, 1fr)", fixed = TRUE)
  expect_match(src, "input$label_chars %||% as.character(AI_GRID_LABEL_DEFAULT)", fixed = TRUE)   # the default applies before the control exists
})
