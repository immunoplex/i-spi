# =============================================================================
# descriptor_bead.R  --  bead-array descriptor for the generic assay import module
# -----------------------------------------------------------------------------
# The flat description controls are GONE (delimiter, optional-element toggles,
# drag-to-order sample elements, drag-to-order B/S/C elements). They asked the
# user to commit to an element order before seeing a single parsed string, and
# the consequence only surfaced three steps later in a downloaded workbook --
# so every new submitter cost a round of trial and error.
#
# "Number of wells per plate" is gone too. It was not a setting so much as a
# guess that the pre-processor then corrected: .rbx reports its own geometry
# (doc$geometry$n_wells), and for every other format ai_infer_plate_size()
# derives it from the wells actually present. A control whose value is always
# overwritten is worse than no control -- it implies a choice that does not
# exist, and a wrong entry looked like a setting the user had made.
#
# `description_elements` is also removed: the component vocabulary and the
# per-type contract now come from AI_COMPONENTS / AI_TYPE_REQUIRED in
# assay_shape_rules.R, so there is one definition instead of one per descriptor.
#
# What remains is the one thing no file carries: the isotype label.
#
# Source AFTER assay_import_contract.R and reader_bead.R.
# =============================================================================

descriptor_bead <- list(
  assay          = "bead",
  label          = "Bead Array",
  default_format = "raw",
  preprocess     = TRUE,   # mount the description pre-processor
  assay_controls = function(ns) {
    textInput(ns("feature_value"), "Feature (isotype), e.g. IgG",
              value = "", placeholder = "\u226415 chars")
  }
)
