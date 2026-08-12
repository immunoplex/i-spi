# =============================================================================
# assay_response.R  --  the one place that understands the assay response value.
# -----------------------------------------------------------------------------
# The response value is generic. It is physically stored in the column
# `antibody_mfi` (a historical bead-array name), but its MEANING is given per
# experiment by xmap_header.assay_response_variable:
#     'mfi'         -> bead arrays (Luminex/MSD)
#     'absorbance'  -> ELISA
#     ... and whatever future assay types define.
# Likewise antibody_n is a generic replicate/observation count, NOT a bead count.
#
# Downstream code should stop hardcoding "mfi". Instead:
#   * canonicalize_response(df) renames the physical columns to the assay-
#     agnostic names `assay_response` / `response_n`;
#   * response_label(var) turns the header code into a display string.
#
# PHYSICAL RENAME LATER: if antibody_mfi/antibody_n are ever renamed in the DB,
# change ONLY the two *_SRC constants below -- nothing else. That is the whole
# point of routing every consumer through this module first.
# =============================================================================

# Physical source columns (the single point of truth for the DB names).
ASSAY_RESPONSE_SRC <- "antibody_mfi"
ASSAY_N_SRC        <- "antibody_n"

# Canonical internal names every consumer should migrate to.
ASSAY_RESPONSE_COL <- "assay_response"
ASSAY_N_COL        <- "response_n"

# Human labels for known response-variable codes. Extend as assay types are added.
RESPONSE_LABELS <- c(
  mfi          = "MFI",
  absorbance   = "Absorbance",
  luminescence = "Luminescence"
)

#' Human label for a response-variable code ("absorbance" -> "Absorbance").
#' Unknown codes are title-cased; empty/NA -> "Response".
response_label <- function(response_var) {
  if (is.null(response_var) || !length(response_var) ||
      is.na(response_var[1]) || !nzchar(response_var[1])) return("Response")
  v <- tolower(response_var[1])
  lbl <- RESPONSE_LABELS[[v]]
  if (is.null(lbl)) tools::toTitleCase(v) else lbl
}

#' Rename the physical response/count columns to the canonical, assay-agnostic
#' names. This REPLACES the old antibody_mfi -> "mfi" rename, which silently
#' mislabeled ELISA absorbance as MFI.
canonicalize_response <- function(df) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  names(df)[names(df) == ASSAY_RESPONSE_SRC] <- ASSAY_RESPONSE_COL
  names(df)[names(df) == ASSAY_N_SRC]        <- ASSAY_N_COL
  df
}

#' The response-variable code for a set of header rows (usually one experiment).
#' Returns a single lowercased code, NA if unknown, or a "|"-joined string if the
#' experiment mixes types (which should be rare and worth surfacing).
response_var_of <- function(header_df) {
  if (is.null(header_df) || !("assay_response_variable" %in% names(header_df)) ||
      !nrow(header_df)) return(NA_character_)
  v <- unique(tolower(stats::na.omit(header_df$assay_response_variable)))
  if (length(v) == 1) v else if (!length(v)) NA_character_ else paste(v, collapse = "|")
}
