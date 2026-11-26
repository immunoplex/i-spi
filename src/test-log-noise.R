# =============================================================================
# test-log-noise.R
# Harmless warnings that filled the preprod server log (2026-11-23..26):
#  - plotly "plotly_click ... is not registered" at every session start
#    (mask_click_data silences only that one warning);
#  - ggplot2 4.0 "geom_errorbarh() was deprecated" (Compare fits forest plot).
# Run from src/: testthat::test_file("test-log-noise.R")
# =============================================================================
library(testthat)
source("mask_ui_helpers.R", local = TRUE)

test_that("mask_click_data silences only the 'not registered' warning", {
  local_mocked_bindings(event_data = function(...) {
    warning("The 'plotly_click' event tied a source ID of 'x' is not registered.")
    NULL }, .package = "plotly")
  expect_no_warning(expect_null(mask_click_data("x")))

  local_mocked_bindings(event_data = function(...) { warning("something else"); "ev" },
                        .package = "plotly")
  expect_warning(v <- mask_click_data("x"), "something else")
  expect_equal(v, "ev")
})

test_that("horizontal error bars build without the geom_errorbarh deprecation", {
  df <- data.frame(est = 1:2, lo = 0:1, hi = 2:3, item = c("a", "b"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = est, y = item)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi), orientation = "y",
                           width = 0.25, na.rm = TRUE, colour = "grey40")
  expect_no_warning(b <- ggplot2::ggplot_build(p))
  d <- b$data[[1]]
  expect_equal(d$xmin, c(0, 1)); expect_equal(d$xmax, c(2, 3))
  expect_equal(as.numeric(d$ymax - d$ymin), c(0.25, 0.25))   # bar height on the y axis
})

test_that("click observers wait for their plot: no 'not registered' warning at sign-in, clicks still arrive", {
  srv <- function(input, output, session) {
    ready <- shiny::reactiveVal(FALSE)
    got   <- shiny::reactiveVal(NULL)
    shiny::observeEvent(mask_click_data("src", ready), got(mask_click_data("src", ready)))
    session$userData$t <- list(ready = ready, got = got)
  }
  shiny::testServer(srv, {
    # plotly warns from session$onFlushed when a read is not registered
    expect_no_warning(session$flushReact())
    # the plot renders: plotly records the registered event, then ready is set
    session$userData$plotlyShinyEventIDs <- "plotly_click-src"
    session$userData$t$ready(TRUE)
    expect_no_warning(session$flushReact())
    session$setInputs(`plotly_click-src` = '[{"customdata":"std|A1|100"}]')
    expect_equal(session$userData$t$got()$customdata, "std|A1|100")
  })
})
