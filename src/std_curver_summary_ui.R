observeEvent(list(
  input$readxMap_experiment_accession,
  input$readxMap_study_accession,
  input$qc_component,
  input$study_level_tabs,
  input$main_tabs), {


    req(input$qc_component == "Standard Curve Summary",
        input$readxMap_study_accession != "Click here",
        input$readxMap_experiment_accession != "Click here",
        input$study_level_tabs == "Experiments",
        input$main_tabs == "view_files_tab")

    if (input$qc_component == "Standard Curve Summary") {
      message("Std Curver Summary")
      selected_study <- input$readxMap_study_accession
      selected_experiment <- input$readxMap_experiment_accession
      param_group <- "standard_curve_options"

      study_params <- fetch_study_parameters(study_accession = selected_study,
                                             param_user = currentuser(),
                                             param_group =param_group,
                                             project_id = userWorkSpaceID(),
                                             conn = conn)

      best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
                                             experiment_accession = selected_experiment,
                                             project_id = userWorkSpaceID(),
                                             conn = conn)
      
      best_plate_all <- enrich_source_with_wavelength(best_plate_all)
      
      

      # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
      #                                              experiment_accession = selected_experiment,
      #                                              conn = conn)
      best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
                                                           experiment_accession = selected_experiment,
                                                           param_user = currentuser(),
                                                           project_id = userWorkSpaceID(),
                                                           conn = conn)
      
      best_standard_all <- enrich_source_with_wavelength(best_standard_all)


      # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
      #                                      experiment_accession = selected_experiment,
      #                                      conn = conn)
      best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
                                                   experiment_accession = selected_experiment,
                                                   param_user = currentuser(),
                                                   project_id = userWorkSpaceID(),
                                                   conn = conn)
      

      #best_pred_all <- align_source_prefixes(best_standard_all, best_pred_all)



      antigen_families <- fetch_antigen_family_table(selected_study, userWorkSpaceID(),
                                                     selected_experiment)


      best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
                                              antigen_families = antigen_families)
      best_pred_all <- enrich_source_with_wavelength(best_pred_all)

      best_pred_all_2 <- best_pred_all

      best_glance_all <- fetch_best_glance_all_summary(study_accession = selected_study,
                                                       experiment_accession = selected_experiment,
                                                       param_user = currentuser(),
                                                       project_id = userWorkSpaceID(),
                                                       conn = conn)
      best_glance_all <- enrich_source_with_wavelength(best_glance_all)
     # best_glance_all <- align_source_prefixes(best_standard_all, best_glance_all)

      # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
      #                                            experiment_accession = selected_experiment,
      #                                            conn = conn)

      best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
                                                             experiment_accession = selected_experiment,
                                                             param_user = currentuser(),
                                                             project_id = userWorkSpaceID(),
                                                             conn = conn)
      best_sample_se_all <- enrich_source_with_wavelength(best_sample_se_all)

      #best_sample_se_all <- align_source_prefixes(best_standard_all, best_sample_se_all)
      
      antigen_settings <- fetch_antigen_parameters(
        study_accession = selected_study,
        experiment_accession = selected_experiment,
        project_id = userWorkSpaceID(),
        conn = conn
      )


      message("Antigen Settings")
      print(unique(antigen_settings$study_accession))
      print(unique(antigen_settings$experiment_accession))

      cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings,
                                               study_params = study_params)
      message("after calculate cv_df")

      output$std_curver_summary_ui <- renderUI({
        tagList(
          fluidRow(
            column(9,
                   div(
                     style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
                              padding: 10px; margin-bottom: 15px; border-radius: 5px;",
                     tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
                     uiOutput("current_sc_summary_context")
                   )
            ),
            column(3,
                   radioButtons(
                     "summary_curve_method",
                     label    = "Curve Method",
                     choices  = c("Frequentist", "Bayesian"),
                     selected = "Frequentist",
                     inline   = TRUE
                   )
            )
          ),
          fluidRow(
            column(4, uiOutput("best_std_antigen_family_ui")),
            column(4, uiOutput("best_std_antigen_ui")),
            column(4, uiOutput("best_std_antigen_source_ui"))
          ),

          # ── Frequentist summary plot ──
          conditionalPanel(
            condition = "input.summary_curve_method == 'Frequentist'",
            plotlyOutput("std_curve_summary_plot"),
            uiOutput("download_standard_curve_fits_data_button_ui"),
            uiOutput("save_norm_btn_ui")
          ),

          # ── Bayesian summary plot ──
          conditionalPanel(
            condition = "input.summary_curve_method == 'Bayesian'",
            shinycssloaders::withSpinner(
              plotlyOutput("bayes_curve_summary_plot", width = "100%", height = "700px"),
              type = 6, color = "#27ae60",
              caption = "Loading Bayesian curves from database..."
            ),
            br(),
            div(class = "table-container", style = "overflow-x:auto;",
                tableOutput("bayes_summary_table"))
          )

        ) # end tagList
      })

      output$current_sc_summary_context <- renderUI({
        best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
        if (nrow(best_pred_exp) > 0) {
          is_log_response <- unique(best_pred_exp$is_log_response)
          is_log_independent <- unique(best_pred_exp$is_log_x)
          blank_option <- unique(best_pred_exp$bkg_method)
          apply_prozone_correction <- unique(best_pred_exp$apply_prozone)

          return(HTML(glue::glue(
            "Showing Standard Curves Fit with: ",
            "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
            "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
            "Blank Handling: {blank_option} | ",
            "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
          )))
        } else {
          current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
                                                              study_accession = selected_study,
                                                              project_id = userWorkSpaceID(),
                                                              conn = conn)
          is_log_response <- unique(current_sc_options$is_log_mfi_axis)
          #print(is_log_response)
          blank_option <- unique(current_sc_options$blank_option)
          apply_prozone_correction <- unique(current_sc_options$apply_prozone)
          #print(blank_option)
          return(HTML(glue::glue(
            "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
            "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
            "Concentration Scale: waiting for first fit | ",
            "Blank Handling: {blank_option} | ",
            "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
          )))

        }

      })

      output$best_std_antigen_family_ui <- renderUI({
        pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
        req(nrow(pred_exp) > 0)

        # Get unique antigen families, filtering out NA values
        family_choices <- unique(pred_exp$antigen_family)
        family_choices <- family_choices[!is.na(family_choices) & family_choices != ""]

        # If no valid families found, this shouldn't happen with the updated attach_antigen_familes
        # but add defensive check anyway
        if (length(family_choices) == 0) {
          family_choices <- "All Antigens"
        }

        selectInput("best_std_antigen_family",
                    label = "Antigen Family",
                    choices = family_choices)

      })

      output$best_std_antigen_ui <- renderUI({
        req(input$best_std_antigen_family)

        # Filter by experiment and antigen family
        best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
                                                !is.na(best_pred_all$antigen_family) &
                                                best_pred_all$antigen_family == input$best_std_antigen_family,]
        req(nrow(best_std_antigen_fam) > 0)

        antigen_options <- unique(best_std_antigen_fam$antigen)

        my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")

        selectInput("best_std_antigen",
                    label = my_label,
                    choices = antigen_options )
      })

      output$best_std_antigen_source_ui <- renderUI({
        req(input$best_std_antigen_family)
        req(input$best_std_antigen)

        # best_std_source <- best_standard_all[
        #   best_standard_all$antigen_family == input$best_std_antigen_family &
        #     best_standard_all$antigen == input$best_std_antigen, ]

        # req(nrow(best_std_source) > 0)

        selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
                                                  !is.na(best_pred_all$antigen_family) &
                                                  best_pred_all$antigen_family == input$best_std_antigen_family  &
                                                  best_pred_all$antigen ==  input$best_std_antigen,]$source)


        req(length(selected_source) > 0)

        radioButtons("best_std_source",
                     label = "Source",
                     choices = selected_source)
      })




      aggregated_fit <- reactive({
        req(best_glance_all)
        req(nrow(best_pred_all) > 0)
        req(input$best_std_antigen)
        req(input$best_std_source)
        selected_study <- input$readxMap_study_accession
        selected_experiment <- input$readxMap_experiment_accession

        result <- aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
                                            experiment_accession = selected_experiment,
                                            antigen = input$best_std_antigen,
                                            source = input$best_std_source,
                                            indep_var = "concentration",
                                            response_var = "mfi",
                                            antigen_settings = antigen_settings)

        # Validate that we got data back
        if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
          return(NULL)
        }
        result
      })

      output$std_curve_summary_plot <- renderPlotly({
        agg_fit <- aggregated_fit()

        # Check if aggregated_fit returned valid data
        if (is.null(agg_fit) || (is.data.frame(agg_fit) && nrow(agg_fit) == 0)) {
          # Return an empty plot with a message
          plot_ly() %>%
            layout(
              title = list(
                text = paste("No standard curve data available for",
                             input$best_std_antigen, "from", input$best_std_source,
                             "<br><sup>Please verify that standard curves have been fit with the current study parameters</sup>"),
                font = list(size = 14)
              ),
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        } else {
          selected_experiment <- input$readxMap_experiment_accession
          summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = agg_fit,
                                   best_plate_all = best_plate_all,
                                   experiment_accession = selected_experiment,
                                   antigen = input$best_std_antigen, source = input$best_std_source)
        }
      })


      output$save_norm_btn_ui <- renderUI({
        req(selected_experiment)
        if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
          actionButton("save_norm_assay_response", "Save Normalized Assay Response")
        }
      })


      observeEvent(input$save_norm_assay_response, {
        cat("pressed save norm_assay_response")

        showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
                         duration = NULL)

        req(best_pred_all, best_glance_all, best_sample_se_all)


        agg_curves_all_antigens <- compute_aggregated_curves(
          best_pred_all = best_pred_all,
          best_glance_all = best_glance_all,
          experiment_accession = input$readxMap_experiment_accession,
          antigen_settings = antigen_settings
        )




        norm_best_sample <- conduct_linear_interpolation_batch(
          best_sample_se_all = best_sample_se_all,
          aggregated_fit_v   = agg_curves_all_antigens
        )


        tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))

        norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]

        norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
        norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)

        # Upsert the normalized assay response
        upsert_best_curve(
          conn   = conn,
          df     = norm_best_sample,
          schema = "madi_results",
          table  = "best_sample_se_all",
          notify = shiny_notify(session)
        )
        cat("after normalization")

        removeNotification(id = "save_norm_assay_response_progress")

        showNotification("Normalized Assay Response Saved for all Antigens.")
      })




      output$download_standard_curve_fits_data_button_ui <- renderUI({
        req(best_glance_all)
        req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
        req(input$readxMap_study_accession, input$readxMap_experiment_accession)
        button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)

        downloadButton("download_standard_curve_fits_data", button_label)
      })


      output$download_standard_curve_fits_data <-  downloadHandler(
        filename = function() {
          paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
        },
        content = function(file) {
          req(best_glance_all)
          req(input$readxMap_study_accession, input$readxMap_experiment_accession)

          download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]

          # download data component (data frame)
          write.csv(download_df, file, row.names = FALSE)
        }
      )





      # ================================================================
      # Bayesian Summary — overlay all plates' curves from DB
      # ================================================================
      output$bayes_curve_summary_plot <- renderPlotly({
        req(identical(input$summary_curve_method, "Bayesian"))
        req(input$best_std_antigen, input$best_std_source)

        antigen_val <- input$best_std_antigen
        source_val  <- input$best_std_source
        proj_id     <- userWorkSpaceID()

        # Fetch all plates' curve data for this antigen+source
        curves <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT * FROM madi_results.bayes_curves
           WHERE project_id = %s AND study_accession = '%s'
             AND experiment_accession = '%s' AND antigen = '%s' AND source = '%s'
           ORDER BY plateid",
          proj_id, selected_study, selected_experiment, antigen_val, source_val)),
          error = function(e) { message("[bayes_summary] curves error: ", e$message); data.frame() })

        if (nrow(curves) == 0) {
          return(plotly::plot_ly() |>
            plotly::layout(
              title = list(
                text = paste0("No Bayesian curves found for ", antigen_val,
                              "<br><sup>Run Bayesian batch calculation first</sup>"),
                font = list(size = 14)),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
        }

        curve_ids <- paste(curves$bayes_curves_id, collapse = ",")

        grids <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT cg.log10_conc, cg.mfi_median, cg.mfi_lower_95, cg.mfi_upper_95,
                  bc.plateid, bc.curve_family
           FROM madi_results.bayes_curve_grid cg
           JOIN madi_results.bayes_curves bc ON cg.bayes_curves_id = bc.bayes_curves_id
           WHERE cg.bayes_curves_id IN (%s) ORDER BY bc.plateid, cg.log10_conc", curve_ids)),
          error = function(e) data.frame())

        cdan_grids <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT cg.log10_conc, cg.smoothed_cv, bc.plateid
           FROM madi_results.bayes_cdan_grid cg
           JOIN madi_results.bayes_curves bc ON cg.bayes_curves_id = bc.bayes_curves_id
           WHERE cg.bayes_curves_id IN (%s)
             AND cg.smoothed_cv IS NOT NULL AND cg.smoothed_cv < 60
           ORDER BY bc.plateid, cg.log10_conc", curve_ids)),
          error = function(e) data.frame())

        # Also fetch standards for overlay
        nom_row <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT DISTINCT standard_curve_concentration FROM madi_results.xmap_antigen_family
           WHERE study_accession = '%s' AND experiment_accession = '%s' AND antigen = '%s'
             AND standard_curve_concentration IS NOT NULL LIMIT 1",
          selected_study, selected_experiment, antigen_val)),
          error = function(e) data.frame())
        nom <- if (nrow(nom_row) > 0) as.numeric(nom_row$standard_curve_concentration[1]) else {
          as.numeric(curves$nominal_sample_dilution[1])
        }

        stds <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT plateid, dilution as dilution_factor, antibody_mfi as mfi
           FROM madi_results.xmap_standard
           WHERE study_accession = '%s' AND experiment_accession = '%s'
             AND antigen = '%s' AND source = '%s'
             AND antibody_mfi > 0 AND dilution > 0",
          selected_study, selected_experiment, antigen_val, source_val)),
          error = function(e) data.frame())
        if (nrow(stds) > 0) stds$concentration <- nom / stds$dilution_factor

        # Build overlay plot — one trace per plate
        plate_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                          "#E69F00", "#56B4E9", "#F0E442", "#999999")
        plates <- unique(grids$plateid)

        p <- plotly::plot_ly()

        # Standards per plate (small dots, faded)
        if (nrow(stds) > 0) {
          for (i in seq_along(plates)) {
            pid <- plates[i]
            sp <- stds[stds$plateid == pid, , drop = FALSE]
            if (nrow(sp) > 0) {
              sp_agg <- sp |>
                dplyr::group_by(concentration) |>
                dplyr::summarise(mfi = median(mfi, na.rm = TRUE), .groups = "drop")
              col <- plate_colors[((i - 1) %% length(plate_colors)) + 1]
              p <- p |> plotly::add_markers(
                data = sp_agg, x = ~log10(concentration), y = ~log10(mfi),
                name = paste0(pid, " (std)"),
                legendgroup = pid,
                marker = list(color = col, size = 5, opacity = 0.4),
                showlegend = FALSE, hoverinfo = "skip")
            }
          }
        }

        # Fitted curves per plate
        for (i in seq_along(plates)) {
          pid <- plates[i]
          pg <- grids[grids$plateid == pid, , drop = FALSE]
          if (nrow(pg) == 0) next
          col <- plate_colors[((i - 1) %% length(plate_colors)) + 1]
          fam <- unique(pg$curve_family)
          fam_lbl <- switch(as.character(fam[1]),
                            "4pl" = "4PL", "5pl" = "5PL", "gompertz" = "Gomp", fam[1])

          # CI ribbon (faded)
          p <- p |> plotly::add_ribbons(
            data = pg,
            x = ~log10_conc,
            ymin = ~log10(pmax(mfi_lower_95, 1e-9)),
            ymax = ~log10(pmax(mfi_upper_95, 1e-9)),
            name = paste0(pid, " CI"),
            legendgroup = pid,
            fillcolor = paste0(col, "22"),
            line = list(color = "transparent"),
            showlegend = FALSE, hoverinfo = "skip")

          # Fitted line
          p <- p |> plotly::add_lines(
            data = pg,
            x = ~log10_conc,
            y = ~log10(pmax(mfi_median, 1e-9)),
            name = paste0(pid, ".", fam_lbl),
            legendgroup = pid,
            line = list(color = col, width = 2))
        }

        # CDAN precision profiles on secondary y-axis
        if (nrow(cdan_grids) > 0) {
          for (i in seq_along(plates)) {
            pid <- plates[i]
            cg <- cdan_grids[cdan_grids$plateid == pid, , drop = FALSE]
            if (nrow(cg) == 0) next
            col <- plate_colors[((i - 1) %% length(plate_colors)) + 1]
            p <- p |> plotly::add_lines(
              data = cg, x = ~log10_conc, y = ~smoothed_cv,
              name = paste0(pid, " CV%"),
              legendgroup = pid,
              yaxis = "y2",
              line = list(color = col, width = 1.5, dash = "dot"),
              showlegend = FALSE,
              hovertemplate = paste0(pid, "<br>Log10 Conc: %{x:.2f}<br>CV%: %{y:.1f}%<extra></extra>"))
          }
          # Threshold lines
          x_range <- range(cdan_grids$log10_conc, na.rm = TRUE)
          p <- p |>
            plotly::add_lines(x = x_range, y = c(20, 20),
              name = "20% CV threshold", yaxis = "y2",
              line = list(color = "#e68fac", dash = "dash", width = 1.5),
              hoverinfo = "skip") |>
            plotly::add_lines(x = x_range, y = c(15, 15),
              name = "15% CV threshold", yaxis = "y2",
              line = list(color = "#4CAF50", dash = "dash", width = 1.5),
              hoverinfo = "skip")
        }

        # Global best family label
        gbf <- unique(curves$global_best_family)
        gbf_lbl <- switch(as.character(gbf[1]),
                          "4pl" = "4PL", "5pl" = "5PL", "gompertz" = "Gompertz", gbf[1])

        layout_args <- list(
          title = list(
            text = paste0("Bayesian Standard Curves for ", antigen_val,
                          " by Plate (Global Best: ", gbf_lbl, ")"),
            font = list(size = 14)),
          xaxis = list(title = "log\u2081\u2080 Concentration",
            gridcolor = "#E5E5E5", showline = TRUE, linecolor = "#CCCCCC"),
          yaxis = list(title = "log\u2081\u2080 MFI",
            gridcolor = "#E5E5E5", showline = TRUE, linecolor = "#CCCCCC"),
          plot_bgcolor = "white", paper_bgcolor = "white",
          hovermode = "closest",
          legend = list(orientation = "h", x = 0, y = -0.15,
            bgcolor = "rgba(255,255,255,0.85)", bordercolor = "#CCCCCC", borderwidth = 1))

        if (nrow(cdan_grids) > 0) {
          layout_args$yaxis2 <- list(
            overlaying = "y", side = "right",
            title = "Coefficient of Variation (%)",
            range = c(0, 55), showgrid = FALSE, zeroline = FALSE,
            tickfont = list(color = "#1565C0"),
            titlefont = list(color = "#1565C0"))
          layout_args$margin <- list(r = 100)
        }

        do.call(plotly::layout, c(list(p), layout_args))
      })

      # ── Bayesian summary table — per-plate metrics ──
      output$bayes_summary_table <- renderTable({
        req(identical(input$summary_curve_method, "Bayesian"))
        req(input$best_std_antigen, input$best_std_source)

        proj_id <- userWorkSpaceID()
        curves <- tryCatch(DBI::dbGetQuery(conn, sprintf(
          "SELECT plateid, curve_family, plate_best_family,
                  lloq, uloq, lloq_15, uloq_15,
                  lod, lrdl, uod, urdl,
                  inflect_x, lo2d, uo2d,
                  plate_elpd_4pl, plate_elpd_5pl, plate_elpd_gompertz,
                  global_stacking_4pl, global_stacking_5pl, global_stacking_gompertz
           FROM madi_results.bayes_curves
           WHERE project_id = %s AND study_accession = '%s'
             AND experiment_accession = '%s' AND antigen = '%s' AND source = '%s'
           ORDER BY plateid",
          proj_id, selected_study, selected_experiment,
          input$best_std_antigen, input$best_std_source)),
          error = function(e) data.frame())

        if (nrow(curves) == 0) return(NULL)

        fmt <- function(x, d = 3) ifelse(is.na(x), "\u2014", sprintf(paste0("%.", d, "f"), x))

        data.frame(
          Plate   = curves$plateid,
          Model   = vapply(curves$curve_family, function(f)
            switch(f, "4pl"="4PL", "5pl"="5PL", gompertz="Gompertz", f), character(1)),
          LLOQ    = fmt(curves$lloq),
          ULOQ    = fmt(curves$uloq),
          LOD     = fmt(curves$lod, 4),
          LRDL    = fmt(curves$lrdl, 4),
          UOD     = fmt(curves$uod, 4),
          URDL    = fmt(curves$urdl, 4),
          Inflection = fmt(curves$inflect_x, 4),
          LO2D    = fmt(curves$lo2d, 4),
          UO2D    = fmt(curves$uo2d, 4),
          ELPD    = fmt(ifelse(
            curves$curve_family == "4pl", curves$plate_elpd_4pl,
            ifelse(curves$curve_family == "5pl", curves$plate_elpd_5pl,
                   curves$plate_elpd_gompertz)), 1),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      },
      caption           = "Bayesian Per-Plate Summary",
      caption.placement = "top",
      striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")


    } # end inside standard curver summary tab
  }) # end observeEvent



# observeEvent(list(
#   input$readxMap_experiment_accession,
#   input$readxMap_study_accession,
#   input$qc_component,
#   input$study_level_tabs,
#   input$main_tabs), {
#
#
#     req(input$qc_component == "Standard Curve Summary",
#         input$readxMap_study_accession != "Click here",
#         input$readxMap_experiment_accession != "Click here",
#         input$study_level_tabs == "Experiments",
#         input$main_tabs == "view_files_tab")
#
#     if (input$qc_component == "Standard Curve Summary") {
#       message("Std Curver Summary")
#       selected_study <- input$readxMap_study_accession
#       selected_experiment <- input$readxMap_experiment_accession
#       param_group <- "standard_curve_options"
#
#       study_params <- fetch_study_parameters(study_accession = selected_study,
#                                              param_user = currentuser(),
#                                              param_group =param_group, conn = conn)
#
#       best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
#                                              experiment_accession = selected_experiment,
#                                              conn = conn)
#
#       # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
#       #                                              experiment_accession = selected_experiment,
#       #                                              conn = conn)
#       best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
#                                                            experiment_accession = selected_experiment,
#                                                            param_user = currentuser(),
#                                                            conn = conn)
#
#
#
#       # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
#       #                                      experiment_accession = selected_experiment,
#       #                                      conn = conn)
#       best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
#                                                    experiment_accession = selected_experiment,
#                                                    param_user = currentuser(),
#                                                    conn = conn)
#
#
#
#       antigen_families <- fetch_antigen_family_table(selected_study)
#
#
#       best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
#                                               antigen_families = antigen_families)
#
#       best_pred_all_2 <- best_pred_all
#
#       best_glance_all <- fetch_best_glance_all_summary(study_accession = selected_study,
#                                                        experiment_accession = selected_experiment,
#                                                        param_user = currentuser(),
#                                                        conn = conn)
#
#       # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
#       #                                            experiment_accession = selected_experiment,
#       #                                            conn = conn)
#
#       best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
#                                                              experiment_accession = selected_experiment,
#                                                              param_user = currentuser(),
#                                                              conn = conn)
#
#
#
#       antigen_settings <- fetch_antigen_parameters(
#         study_accession = selected_study,
#         experiment_accession = selected_experiment,
#         conn = conn
#       )
#
#
#       message("Antigen Settings")
#       print(unique(antigen_settings$study_accession))
#       print(unique(antigen_settings$experiment_accession))
#
#       cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings)
#       message("after calculate cv_df")
#
#       output$std_curver_summary_ui <- renderUI({
#         tagList(
#           fluidRow(
#             column(9,
#                    div(
#                      style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
#                               padding: 10px; margin-bottom: 15px; border-radius: 5px;",
#                      tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
#                      uiOutput("current_sc_summary_context")
#                    )
#             )
#           ),
#           fluidRow(
#             column(4, uiOutput("best_std_antigen_family_ui")),
#             column(4, uiOutput("best_std_antigen_ui")),
#             column(4, uiOutput("best_std_antigen_source_ui"))
#           ),
#           plotlyOutput("std_curve_summary_plot"),
#           uiOutput("download_standard_curve_fits_data_button_ui"),
#           uiOutput("save_norm_btn_ui")
#
#         ) # end tagList
#       })
#
#       output$current_sc_summary_context <- renderUI({
#         best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
#         if (nrow(best_pred_exp) > 0) {
#           is_log_response <- unique(best_pred_exp$is_log_response)
#           is_log_independent <- unique(best_pred_exp$is_log_x)
#           blank_option <- unique(best_pred_exp$bkg_method)
#           apply_prozone_correction <- unique(best_pred_exp$apply_prozone)
#
#           return(HTML(glue::glue(
#             "Showing Standard Curves Fit with: ",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#         } else {
#           current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
#                                                               study_accession = selected_study, conn = conn)
#           is_log_response <- unique(current_sc_options$is_log_mfi_axis)
#           #print(is_log_response)
#           blank_option <- unique(current_sc_options$blank_option)
#           apply_prozone_correction <- unique(current_sc_options$apply_prozone)
#           #print(blank_option)
#           return(HTML(glue::glue(
#             "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: waiting for first fit | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#
#         }
#
#       })
#
#       output$best_std_antigen_family_ui <- renderUI({
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         selectInput("best_std_antigen_family",
#                     label = "Antigen Family",
#                     choices =   unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]$antigen_family))
#
#       })
#
#       output$best_std_antigen_ui <- renderUI({
#
#         # req(input$best_std_antigen_family)
#         best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                 best_pred_all$antigen_family == input$best_std_antigen_family,]
#         req(nrow(best_std_antigen_fam) > 0)
#
#         antigen_options <- unique(best_std_antigen_fam$antigen)
#
#         my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")
#
#         selectInput("best_std_antigen",
#                     label = my_label,
#                     choices = antigen_options )
#       })
#
#       output$best_std_antigen_source_ui <- renderUI({
#         req(input$best_std_antigen_family)
#         req(input$best_std_antigen)
#
#         # best_std_source <- best_standard_all[
#         #   best_standard_all$antigen_family == input$best_std_antigen_family &
#         #     best_standard_all$antigen == input$best_std_antigen, ]
#
#         # req(nrow(best_std_source) > 0)
#
#         selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                   best_pred_all$antigen_family == input$best_std_antigen_family  &
#                                                   best_pred_all$antigen ==  input$best_std_antigen,]$source)
#
#
#         req(length(selected_source) > 0)
#
#         radioButtons("best_std_source",
#                      label = "Source",
#                      choices = selected_source)
#       })
#
#
#
#
#       aggregated_fit <- reactive({
#         req(best_glance_all)
#         req(nrow(best_pred_all) > 0)
#         req(input$best_std_antigen)
#         req(input$best_std_source)
#         selected_study <- input$readxMap_study_accession
#         selected_experiment <- input$readxMap_experiment_accession
#
#         result <- aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
#                                             experiment_accession = selected_experiment,
#                                             antigen = input$best_std_antigen,
#                                             source = input$best_std_source,
#                                             indep_var = "concentration",
#                                             response_var = "mfi",
#                                             antigen_settings = antigen_settings)
#
#         # Validate that we got data back
#         if (is.null(result) || (is.data.frame(result) && nrow(result) == 0)) {
#           return(NULL)
#         }
#         result
#       })
#
#       output$std_curve_summary_plot <- renderPlotly({
#         agg_fit <- aggregated_fit()
#
#         # Check if aggregated_fit returned valid data
#         if (is.null(agg_fit) || (is.data.frame(agg_fit) && nrow(agg_fit) == 0)) {
#           # Return an empty plot with a message
#           plot_ly() %>%
#             layout(
#               title = list(
#                 text = paste("No standard curve data available for",
#                              input$best_std_antigen, "from", input$best_std_source,
#                              "<br><sup>Please verify that standard curves have been fit with the current study parameters</sup>"),
#                 font = list(size = 14)
#               ),
#               xaxis = list(visible = FALSE),
#               yaxis = list(visible = FALSE)
#             )
#         } else {
#           selected_experiment <- input$readxMap_experiment_accession
#           summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = agg_fit,
#                                    best_plate_all = best_plate_all,
#                                    experiment_accession = selected_experiment,
#                                    antigen = input$best_std_antigen, source = input$best_std_source)
#         }
#       })
#
#
#       output$save_norm_btn_ui <- renderUI({
#         req(selected_experiment)
#         if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
#           actionButton("save_norm_assay_response", "Save Normalized Assay Response")
#         }
#       })
#
#
#       observeEvent(input$save_norm_assay_response, {
#         cat("pressed save norm_assay_response")
#
#         showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
#                          duration = NULL)
#
#         req(best_pred_all, best_glance_all, best_sample_se_all)
#
#
#         agg_curves_all_antigens <- compute_aggregated_curves(
#           best_pred_all = best_pred_all,
#           best_glance_all = best_glance_all,
#           experiment_accession = input$readxMap_experiment_accession,
#           antigen_settings = antigen_settings
#         )
#
#
#
#
#         norm_best_sample <- conduct_linear_interpolation_batch(
#           best_sample_se_all = best_sample_se_all,
#           aggregated_fit_v   = agg_curves_all_antigens
#         )
#
#
#         tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))
#
#         norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]
#
#         norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
#         norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)
#
#         # Upsert the normalized assay response
#         upsert_best_curve(
#           conn   = conn,
#           df     = norm_best_sample,
#           schema = "madi_results",
#           table  = "best_sample_se_all",
#           notify = shiny_notify(session)
#         )
#         cat("after normalization")
#
#         removeNotification(id = "save_norm_assay_response_progress")
#
#         showNotification("Normalized Assay Response Saved for all Antigens.")
#       })
#
#
#
#
#       output$download_standard_curve_fits_data_button_ui <- renderUI({
#         req(best_glance_all)
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)
#
#         downloadButton("download_standard_curve_fits_data", button_label)
#       })
#
#
#       output$download_standard_curve_fits_data <-  downloadHandler(
#         filename = function() {
#           paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
#         },
#         content = function(file) {
#           req(best_glance_all)
#           req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#
#           download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]
#
#           # download data component (data frame)
#           write.csv(download_df, file, row.names = FALSE)
#         }
#       )
#
#
#
#
#
#     } # end inside standard curver summary tab
#   }) # end observeEvent



# observeEvent(list(
#   input$readxMap_experiment_accession,
#   input$readxMap_study_accession,
#   input$qc_component,
#   input$study_level_tabs,
#   input$main_tabs), {
#
#
#     req(input$qc_component == "Standard Curve Summary",
#         input$readxMap_study_accession != "Click here",
#         input$readxMap_experiment_accession != "Click here",
#         input$study_level_tabs == "Experiments",
#         input$main_tabs == "view_files_tab")
#
#     if (input$qc_component == "Standard Curve Summary") {
#       message("Std Curver Summary")
#       selected_study <- input$readxMap_study_accession
#       selected_experiment <- input$readxMap_experiment_accession
#       param_group <- "standard_curve_options"
#
#       study_params <- fetch_study_parameters(study_accession = selected_study,
#                                               param_user = currentuser(),
#                                               param_group =param_group, conn = conn)
#
#       best_plate_all <- fetch_best_plate_all(study_accession = selected_study,
#                                              experiment_accession = selected_experiment,
#                                              conn = conn)
#
#       # best_standard_all <- fetch_best_standard_all(study_accession = selected_study,
#       #                                              experiment_accession = selected_experiment,
#       #                                              conn = conn)
#       best_standard_all <- fetch_best_standard_all_summary(study_accession = selected_study,
#                                                            experiment_accession = selected_experiment,
#                                                            param_user = currentuser(),
#                                                            conn = conn)
#
#
#
#       # best_pred_all <- fetch_best_pred_all(study_accession = selected_study,
#       #                                      experiment_accession = selected_experiment,
#       #                                      conn = conn)
#       best_pred_all <- fetch_best_pred_all_summary(study_accession = selected_study,
#                                                    experiment_accession = selected_experiment,
#                                                    param_user = currentuser(),
#                                                    conn = conn)
#
#
#
#       antigen_families <- fetch_antigen_family_table(selected_study)
#
#
#       best_pred_all <- attach_antigen_familes(best_pred_all = best_pred_all,
#                                               antigen_families = antigen_families)
#
#       best_pred_all_2 <- best_pred_all
#
#       best_glance_all <- fetch_best_glance_all(study_accession = selected_study,
#                                                experiment_accession = selected_experiment,
#                                                 conn = conn)
#
#       # best_sample_se_all <- fetch_best_sample_se_all(study_accession = selected_study,
#       #                                            experiment_accession = selected_experiment,
#       #                                            conn = conn)
#
#       best_sample_se_all <- fetch_best_sample_se_all_summary(study_accession = selected_study,
#                                        experiment_accession = selected_experiment,
#                                        param_user = currentuser(),
#                                        conn = conn)
#
#
#
#       antigen_settings <- fetch_antigen_parameters(
#         study_accession = selected_study,
#         experiment_accession = selected_experiment,
#         conn = conn
#       )
#
#
#       message("Antigen Settings")
#       print(unique(antigen_settings$study_accession))
#       print(unique(antigen_settings$experiment_accession))
#
#       cv_df <- calculate_cv_dilution_platewise(best_standard = best_standard_all, antigen_settings = antigen_settings)
#       message("after calculate cv_df")
#
#       output$std_curver_summary_ui <- renderUI({
#         tagList(
#           fluidRow(
#             column(9,
#                      div(
#                        style = "background-color: #f0f8ff; border: 1px solid #4a90e2;
#                               padding: 10px; margin-bottom: 15px; border-radius: 5px;",
#                        tags$h4("Current Standard Curve Summary Context", style = "margin-top: 0; color: #2c5aa0;"),
#                        uiOutput("current_sc_summary_context")
#                      )
#             )
#           ),
#           fluidRow(
#             column(4, uiOutput("best_std_antigen_family_ui")),
#             column(4, uiOutput("best_std_antigen_ui")),
#             column(4, uiOutput("best_std_antigen_source_ui"))
#           ),
#           plotlyOutput("std_curve_summary_plot"),
#           uiOutput("download_standard_curve_fits_data_button_ui"),
#           uiOutput("save_norm_btn_ui")
#
#         ) # end tagList
#       })
#
#       output$current_sc_summary_context <- renderUI({
#         best_pred_exp <- best_pred_all[best_pred_all$experiment_accession == selected_experiment,]
#         if (nrow(best_pred_exp) > 0) {
#             is_log_response <- unique(best_pred_exp$is_log_response)
#             is_log_independent <- unique(best_pred_exp$is_log_x)
#             blank_option <- unique(best_pred_exp$bkg_method)
#             apply_prozone_correction <- unique(best_pred_exp$apply_prozone)
#
#             return(HTML(glue::glue(
#               "Showing Standard Curves Fit with: ",
#               "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#               "Concentration Scale: {ifelse(is_log_independent, 'log<sub>10</sub>', 'linear')} | ",
#               "Blank Handling: {blank_option} | ",
#               "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#             )))
#         } else {
#           current_sc_options <- fetch_current_sc_options_wide(currentuser = currentuser(),
#                                                                study_accession = selected_study, conn = conn)
#           is_log_response <- unique(current_sc_options$is_log_mfi_axis)
#           #print(is_log_response)
#           blank_option <- unique(current_sc_options$blank_option)
#           apply_prozone_correction <- unique(current_sc_options$apply_prozone)
#           #print(blank_option)
#           return(HTML(glue::glue(
#             "Standard Curves have not been saved for the current combination of standard curve options selected:\n",
#             "Response Scale: {ifelse(is_log_response, 'log<sub>10</sub>', 'linear')} | ",
#             "Concentration Scale: waiting for first fit | ",
#             "Blank Handling: {blank_option} | ",
#             "Prozone Correction: {ifelse(apply_prozone_correction, 'applied', 'not applied')}"
#           )))
#
#         }
#
#       })
#
#       output$best_std_antigen_family_ui <- renderUI({
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         selectInput("best_std_antigen_family",
#                     label = "Antigen Family",
#                     choices =   unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]$antigen_family))
#
#       })
#
#       output$best_std_antigen_ui <- renderUI({
#
#        # req(input$best_std_antigen_family)
#         best_std_antigen_fam <- best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                 best_pred_all$antigen_family == input$best_std_antigen_family,]
#         req(nrow(best_std_antigen_fam) > 0)
#
#         antigen_options <- unique(best_std_antigen_fam$antigen)
#
#         my_label <- paste0("Select a Single Antigen in ", input$best_std_antigen_family," for plotting Standard Curves")
#
#         selectInput("best_std_antigen",
#                     label = my_label,
#                     choices = antigen_options )
#       })
#
#       output$best_std_antigen_source_ui <- renderUI({
#         req(input$best_std_antigen_family)
#         req(input$best_std_antigen)
#
#         # best_std_source <- best_standard_all[
#         #   best_standard_all$antigen_family == input$best_std_antigen_family &
#         #     best_standard_all$antigen == input$best_std_antigen, ]
#
#         # req(nrow(best_std_source) > 0)
#
#         selected_source <- unique(best_pred_all[best_pred_all$experiment_accession == selected_experiment &
#                                                  best_pred_all$antigen_family == input$best_std_antigen_family  &
#                                                  best_pred_all$antigen ==  input$best_std_antigen,]$source)
#
#
#         req(length(selected_source) > 0)
#
#         radioButtons("best_std_source",
#                      label = "Source",
#                      choices = selected_source)
#       })
#
#
#
#
#       aggregated_fit <- reactive({
#         req(best_glance_all)
#         req(nrow(best_pred_all) > 0)
#         selected_study <- input$readxMap_study_accession
#         selected_experiment <- input$readxMap_experiment_accession
#
#         aggregate_standard_curves(best_pred_all = best_pred_all,  best_glance_all = best_glance_all,
#                                   experiment_accession = selected_experiment,
#                                   antigen = input$best_std_antigen,
#                                   source = input$best_std_source,
#                                   indep_var = "concentration",
#                                   response_var = "mfi",
#                                   antigen_settings = antigen_settings)
#
#         })
#
#       output$std_curve_summary_plot <- renderPlotly({
#         req(aggregated_fit)
#         selected_experiment <- input$readxMap_experiment_accession
#         aggregated_fit<- aggregated_fit()
#         #aggregated_fit_v <<- aggregated_fit
#         summarize_sc_fits_plotly(best_pred_all = best_pred_all, cv_df = cv_df, aggregated_fit = aggregated_fit(),
#                                   best_plate_all = best_plate_all,
#                                   experiment_accession = selected_experiment,
#                                   antigen =  input$best_std_antigen, source =  input$best_std_source)
#
#       })
#
#
#       output$save_norm_btn_ui <- renderUI({
#         req(selected_experiment)
#         if (nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0) {
#           actionButton("save_norm_assay_response", "Save Normalized Assay Response")
#         }
#       })
#
#
#       observeEvent(input$save_norm_assay_response, {
#         cat("pressed save norm_assay_response")
#
#         showNotification(id = "save_norm_assay_response_progress", "Saving Normalized Assay Response for all Antigens.",
#                          duration = NULL)
#
#         req(best_pred_all, best_glance_all, best_sample_se_all)
#
#
#       agg_curves_all_antigens <- compute_aggregated_curves(
#             best_pred_all = best_pred_all,
#             best_glance_all = best_glance_all,
#             experiment_accession = input$readxMap_experiment_accession,
#             antigen_settings = antigen_settings
#           )
#
#
#
#
#       norm_best_sample <- conduct_linear_interpolation_batch(
#         best_sample_se_all = best_sample_se_all,
#         aggregated_fit_v   = agg_curves_all_antigens
#       )
#
#
#       tbl_cols <- dbListFields(conn, DBI::Id(schema="madi_results", table="best_sample_se_all"))
#
#       norm_best_sample <- norm_best_sample[, intersect(names(norm_best_sample), tbl_cols)]
#
#       norm_best_sample$best_sample_se_all_id  <- as.numeric(norm_best_sample$best_sample_se_all_id)
#       norm_best_sample$best_glance_all_id     <- as.numeric(norm_best_sample$best_glance_all_id)
#
#       # Upsert the normalized assay response
#       upsert_best_curve(
#         conn   = conn,
#         df     = norm_best_sample,
#         schema = "madi_results",
#         table  = "best_sample_se_all",
#         notify = shiny_notify(session)
#       )
#       cat("after normalization")
#
#       removeNotification(id = "save_norm_assay_response_progress")
#
#       showNotification("Normalized Assay Response Saved for all Antigens.")
#       })
#
#
#
#
#       output$download_standard_curve_fits_data_button_ui <- renderUI({
#         req(best_glance_all)
#         req(nrow(best_pred_all[best_pred_all$experiment_accession == selected_experiment,]) > 0)
#         req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#         button_label <-  paste0("Download Standard Curve Fits Data for ", input$readxMap_experiment_accession, " in ", input$readxMap_study_accession)
#
#         downloadButton("download_standard_curve_fits_data", button_label)
#       })
#
#
#       output$download_standard_curve_fits_data <-  downloadHandler(
#         filename = function() {
#           paste(input$readxMap_study_accession, input$readxMap_experiment_accession, "_fits_data", ".csv", sep = "_")
#         },
#         content = function(file) {
#           req(best_glance_all)
#           req(input$readxMap_study_accession, input$readxMap_experiment_accession)
#
#           download_df <- best_glance_all[best_glance_all$experiment_accession == input$readxMap_experiment_accession,]
#
#           # download data component (data frame)
#           write.csv(download_df, file, row.names = FALSE)
#         }
#       )
#
#
#
#
#
#     } # end inside standard curver summary tab
#   }) # end observeEvent
