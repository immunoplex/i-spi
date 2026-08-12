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
    tagList(
      numericInput(ns("n_wells"), "Number of wells per plate",
                   value = 96, min = 96, max = 384, step = 288),
      textInput(ns("delimiter"), "Description delimiter", value = "_"),
      tags$label(style = "font-weight:600;", "Include optional elements:"),
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("optional_elements"), label = NULL,
        choices = c("SampleGroupA", "SampleGroupB"),
        selected = c("SampleGroupA", "SampleGroupB"),
        status = "outline-primary",
        checkIcon = list(yes = icon("check"), no = icon("times"))),
      uiOutput(ns("x_element_order_ui")),
      shinyjqui::orderInput(
        inputId = ns("bcs_element_order"),
        label = "Description Label: Blank/Standard/Control Elements (drag to reorder)",
        items = c("Source", "DilutionFactor"), width = "100%", item_class = "info")
    )
  }
)
