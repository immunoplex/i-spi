output$dilution_standards_controls <- renderUI({
  tagList(
    fluidRow(
      div(
        style = "width: 100%; padding: 0 15px;",  # Added container styling
        span(
          div(style = "display:inline-block; margin-bottom: 10px;",
              title = "Info",
              icon("info-circle", class = "fa-lg", `data-toggle` = "tooltip",
                   `data-placement` = "right",
                   title = paste("To handle multiple MFI values measured at the same dilution factor,
                                 switch the toggle to true to compute the mean MFI at each dilution factor to true.
                                 If false, the raw MFI values will be used in the standard curves. This could result in repeated measures
                                 at each dilution factor in the standard curve."),
                   `data-html` = "true")
          )
        )
      )
    ),
    fluidRow(
      uiOutput("toggle_aggrigate_mfi_dilution")
    )
  )
})

# togle switch to decide if we want to compute aggregate mfi at dilution factors
output$toggle_aggrigate_mfi_dilution <- renderUI({
  switchInput("toggle", label = "Compute Mean MFI at each Dilution Factor", value = aggrigate_mfi_dilution())
})



observeEvent(input$toggle, {
  aggrigate_mfi_dilution(input$toggle)
})

