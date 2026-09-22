# =============================================================================
# descriptor_elisa.R  --  ELISA descriptor for the generic assay import module
# -----------------------------------------------------------------------------
# Same reduction as descriptor_bead.R: the delimiter and element-order controls
# are replaced by the description pre-processor, and "Number of wells per plate"
# is derived by ai_infer_plate_size() from the wells present rather than typed
# in. ELISA has no feature box either (the feature is read from the plate_map
# sheet), so this descriptor carries no controls at all.
#
# assay_controls is omitted rather than set to a function returning NULL:
# assay_import_ui() already tests for its presence.
#
# Source AFTER assay_import_contract.R and reader_elisa.R.
# =============================================================================

descriptor_elisa <- list(
  assay          = "elisa",
  label          = "ELISA",
  default_format = "xlsx",
  preprocess     = TRUE    # mount the description pre-processor
)
