# =============================================================================
# test-compute-cluster-registry.R
# -----------------------------------------------------------------------------
# Tests for compute_cluster_registry()'s env-var scan (pure, no DB/Shiny).
# resolve_compute_cluster() needs a DB pool and is covered by the manual
# checklist in dev/HANDOFF_configurable_compute_backend.md instead.
#
# Run: testthat::test_file("test-compute-cluster-registry.R")
# =============================================================================

if (!exists("compute_cluster_registry")) {
  cand <- c("compute_cluster_registry.R", "src/compute_cluster_registry.R",
            "../../compute_cluster_registry.R", "../../../src/compute_cluster_registry.R")
  hit <- cand[file.exists(cand)][1]
  if (is.na(hit)) stop("cannot find compute_cluster_registry.R; edit the path in this test")
  source(hit)
}

library(testthat)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Isolate every test from whatever ISPI_COMPUTE_* vars happen to be set in the
# running environment (e.g. a real .Renviron on a dev machine).
.with_clean_env <- function(vars, code) {
  all_vars <- unique(c(vars, grep("^ISPI_COMPUTE_", names(Sys.getenv()), value = TRUE)))
  old <- Sys.getenv(all_vars, unset = NA, names = TRUE)
  on.exit({
    unset <- names(old)[is.na(old)]
    if (length(unset)) Sys.unsetenv(unset)
    keep <- old[!is.na(old)]
    if (length(keep)) do.call(Sys.setenv, as.list(keep))
  }, add = TRUE)
  Sys.unsetenv(grep("^ISPI_COMPUTE_", names(Sys.getenv()), value = TRUE))
  force(code)
}

test_that("bare pair only registers as 'default'", {
  .with_clean_env(character(0), {
    Sys.setenv(ISPI_COMPUTE_URL = "https://host/i-spi-compute",
               ISPI_COMPUTE_API_KEY = "k1")
    reg <- compute_cluster_registry()
    expect_equal(names(reg), "default")
    expect_equal(reg$default$base_url, "https://host/i-spi-compute")
    expect_equal(reg$default$api_key, "k1")
  })
})

test_that("a suffixed pair registers under its normalized label alongside default", {
  .with_clean_env(character(0), {
    Sys.setenv(ISPI_COMPUTE_URL = "https://host/i-spi-compute",
               ISPI_COMPUTE_API_KEY = "k1",
               ISPI_COMPUTE_URL__CLONE_B = "https://host-b/i-spi-compute",
               ISPI_COMPUTE_API_KEY__CLONE_B = "k2")
    reg <- compute_cluster_registry()
    expect_setequal(names(reg), c("default", "clone_b"))
    expect_equal(reg$clone_b$base_url, "https://host-b/i-spi-compute")
    expect_equal(reg$clone_b$api_key, "k2")
  })
})

test_that("a URL var with no matching key var yields an empty-string key, not an error", {
  .with_clean_env(character(0), {
    Sys.setenv(ISPI_COMPUTE_URL__ORPHAN = "https://host-orphan/i-spi-compute")
    reg <- compute_cluster_registry()
    expect_equal(reg$orphan$base_url, "https://host-orphan/i-spi-compute")
    expect_equal(reg$orphan$api_key, "")
  })
})

test_that("duplicate normalized labels warn and keep exactly one, deterministically", {
  .with_clean_env(character(0), {
    Sys.setenv(ISPI_COMPUTE_URL__CLONE_B = "https://host-b1/i-spi-compute",
               ISPI_COMPUTE_API_KEY__CLONE_B = "k1",
               `ISPI_COMPUTE_URL__CLONE-B` = "https://host-b2/i-spi-compute",
               `ISPI_COMPUTE_API_KEY__CLONE-B` = "k2")
    expect_warning(reg <- compute_cluster_registry(), "duplicate cluster label")
    # "winner" is whichever env var name sorts first (Sys.getenv() enumerates
    # alphabetically) -- "CLONE-B" < "CLONE_B" since '-' (0x2D) < '_' (0x5F).
    expect_equal(reg$clone_b$base_url, "https://host-b2/i-spi-compute")
  })
})

test_that("no ISPI_COMPUTE_URL at all yields an empty registry", {
  .with_clean_env(character(0), {
    expect_equal(compute_cluster_registry(), list())
  })
})
