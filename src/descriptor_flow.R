# =============================================================================
# descriptor_flow.R  —  11.10 Assay Import Refactor, Phase 3
# -----------------------------------------------------------------------------
# Post-gating flow-cytometry descriptor for the generic assay import module.
# One format (flow/flowjo, from reader_flow.R). The feature control is the
# isotype label written into the template; the antigen (whole-virus/bacterium
# target) is filled into the antigen_list sheet and is required at validation.
# Source AFTER assay_import_contract.R and reader_flow.R.
# =============================================================================

descriptor_flow <- list(
  assay          = "flow",
  label          = "Post-gating Flow Cytometry",
  default_format = "flowjo",
  assay_controls = function(ns) {
    tagList(
      numericInput(ns("n_wells"), "Wells per plate", value = 96, min = 1),
      textInput(ns("feature_value"), "Feature (isotype), e.g. MFI",
                value = "MFI", placeholder = "\u226415 chars")
    )
  }
)
