# =============================================================================
# import_helpers.R  —  11.10 Assay Import Refactor, Phase 4
# -----------------------------------------------------------------------------
# Small pure helpers LIFTED from the retired import_lumifile.R that are still
# used by kept libraries (batch_layout_functions.R, generate_layout_template_ref.R).
# extract_plate_number() must remain defined once import_lumifile.R is retired.
# =============================================================================

extract_plate_number <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)

  # Try multiple patterns to extract plate number
  # Pattern 1: "plate" followed by separator and number (plate_3, plate 3, plate.3, plate-3)
  match1 <- regmatches(text, regexpr("[Pp]late[_\\s\\.-]+(\\d+)", text, perl = TRUE))
  if (length(match1) > 0 && nchar(match1) > 0) {
    num <- gsub("[^0-9]", "", match1)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  # Pattern 2: Just "plate" followed immediately by number (plate3, plate1IgGtot...)
  match2 <- regmatches(text, regexpr("[Pp]late(\\d+)", text, perl = TRUE))
  if (length(match2) > 0 && nchar(match2) > 0) {
    num <- gsub("[^0-9]", "", match2)
    if (nchar(num) > 0) return(paste0("plate_", num))
  }

  return(NA_character_)
}

