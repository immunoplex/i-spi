# =============================================================================
# descriptor_bead.R  —  11.10 Assay Import Refactor, Phase 3 (+ problem 2)
# -----------------------------------------------------------------------------
# Bead-array descriptor for the generic assay import module. Restores the
# constrained/ordered description-field controls from the retired import UI:
#   - wells: 96 or 384 (numericInput, constrained)
#   - Include Optional Elements: SampleGroupA / SampleGroupB (checkboxGroupButtons)
#   - Sample element ORDER: drag-to-order orderInput (base + optional), rendered
#     by the module from description_elements (dynamic on the optional toggle)
#   - B/S/C element ORDER: drag-to-order orderInput (Source, DilutionFactor)
# The module reads the ordered vectors from input$x_element_order /
# input$bcs_element_order. Requires shinyjqui + shinyWidgets (loaded by the app).
# Source AFTER assay_import_contract.R and reader_bead.R.
# =============================================================================

descriptor_bead <- list(
  assay          = "bead",
  label          = "Bead Array",
  default_format = "raw",
  description_elements = list(
    base     = c("PatientID", "DilutionFactor", "TimePeriod"),
    optional = c("SampleGroupA", "SampleGroupB"),
    bcs      = c("Source", "DilutionFactor")
  ),
  assay_controls = function(ns) {
    tagList(
      numericInput(ns("n_wells"), "Number of wells per plate",
                   value = 96, min = 96, max = 384, step = 288),
      textInput(ns("feature_value"), "Feature (isotype), e.g. IgG",
                value = "", placeholder = "\u226415 chars"),
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
