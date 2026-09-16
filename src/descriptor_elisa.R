# =============================================================================
# descriptor_elisa.R  —  11.10 Assay Import Refactor, Phase 3 (+ problem 2)
# -----------------------------------------------------------------------------
# ELISA descriptor for the generic assay import module. Same constrained/ordered
# description controls as bead, minus the feature box (ELISA derives feature from
# the data). Source AFTER assay_import_contract.R and reader_elisa.R.
# =============================================================================

descriptor_elisa <- list(
  assay          = "elisa",
  label          = "ELISA",
  default_format = "xlsx",
  description_elements = list(
    base     = c("PatientID", "DilutionFactor", "TimePeriod"),
    optional = c("SampleGroupA", "SampleGroupB"),
    bcs      = c("Source", "DilutionFactor")
  ),
  assay_controls = function(ns) {
    # Description parse-rule controls are now the per-type rule panel, mounted by
    # the module from description_elements (assay_description_rule_ui.R).
    tagList(
      numericInput(ns("n_wells"), "Number of wells per plate",
                   value = 96, min = 96, max = 384, step = 288)
    )
  }
)
