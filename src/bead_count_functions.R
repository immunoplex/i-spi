# Functions for Bead Count Analysis

# Calculate Log Dilution Factor and clean up plate names
calculate_log_dilution <- function(dat){
  dat <- dat %>% mutate(dilution_factor = 1/dat$dilution,
                        log_dilution = log10(dilution_factor))

  # dat$mfi <- log10(dat$mfi)
  #dat_v <<- dat
  return(dat)
}

# function takes in df which can be sample data or standard curve data with
#well column and converts character well to a well number in 96 well multiplex bead assay
#plate
obtain_well_number <- function(df, well_col) {
  # Define row and column indices of a 96-well plate
  rows <- LETTERS[1:8]  # A to H
  cols <- 1:12          # 1 to 12

  # Extract the well values from the specified column
  all_wells <- df[[well_col]]

  # Compute well numbers
  well_vector <- sapply(all_wells, function(well) {
    row_letter <- substr(well, 1, 1)
    col_number <- as.numeric(substr(well, 2, nchar(well)))

    # Check validity of row and column
    if (!(row_letter %in% rows) || is.na(col_number) || !(col_number %in% cols)) {
      return(NA)
    }

    # Compute well number
    row_index <- match(row_letter, rows)  # Convert row letter to index
    well_number <- (col_number - 1) * 8 + row_index

    return(ifelse(well_number >= 1 & well_number <= 96, well_number, NA))
  })

  # Add the new column to the original data frame
  df$well_number <- well_vector
  df_view <- df
  return(df)
}

# plot bead count with df with well, thresholds and criteria.
# Plotly outputs.
plot_bead_count <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
  if (failed_well_criteria == "Below_Upper_Threshold" && !is.na(upper_threshold)) {
    bead_count_threshold <- upper_threshold
  } else if (failed_well_criteria == "Below_Lower_Threshold" && !is.na(lower_threshold)) {
    bead_count_threshold <- lower_threshold
  } else if (is.na(upper_threshold) && is.na(lower_threshold)) {
    bead_count_threshold <- NA
  } else if (failed_well_criteria == "Below_Upper_Threshold" && is.na(upper_threshold)) {
    bead_count_threshold <- NA
  } else if (failed_well_criteria == "Below_Lower_Threshold" && is.na(lower_threshold)) {
    bead_count_threshold <- NA
  }
  # Add indicator of bead count gate class
  if (!is.na(bead_count_threshold)) {
    df_well$bead_count_gc <- ifelse(df_well$n >= bead_count_threshold,
                                             "Sufficient Bead Count", "Low Bead Count")
  } else {
    df_well$bead_count_gc <- "Sufficient Bead Count"
  }

  df_well$bead_count_gc <- factor(df_well$bead_count_gc,
                                           levels = c("Sufficient Bead Count", "Low Bead Count"))

  # Order the data by well number
  df_well <- df_well[order(df_well$well_number), ]
  df_well$well_number <- as.numeric(as.character(df_well$well_number))

  # calculate the average bead count to whole number
  mean_bead_count <- round(mean(df_well$n, na.rm = T),0)
  # hover text condition for sample data or standards
  # df_well <- df_well
  df_well$hover_text <- with(df_well, ifelse(bead_count_gc == "Sufficient Bead Count" & stype == "X",
                                             paste0("<br>Well: ", well,
                                                    "<br>Bead Count: ", n,
                                                    "<br>Subject: ", subject_accession
                                             ),
                                             paste0("<br>Well: ", well,
                                                    "<br>Bead Count: ", n)
                                      ))
  # for standards or sample
  data_type <- ifelse(unique(df_well$stype) == "S", "Standards", "Samples")

  # Create plot
  p <- plot_ly() %>%
    add_trace(
      data = df_well,
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "lines",
      line = list(color = 'lightgrey'),
      showlegend = F
    ) %>%
    # Points for Sufficient Bead Count
    add_trace(
      data = subset(df_well, bead_count_gc == "Sufficient Bead Count"),
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#0067a5"),
      showlegend = TRUE,
      name = "Sufficient Bead Count",
      text = ~hover_text,
      hoverinfo = "text"
    ) %>%
    # Points for Low Bead Count
    add_trace(
      data = subset(df_well, bead_count_gc == "Low Bead Count"),
      x = ~well_number,
      y = ~n,
      type = "scatter",
      mode = "markers",
      marker = list(color = "#be0032"),
      showlegend = TRUE,
      name = "Low Bead Count",
      text = ~hover_text,
      hoverinfo = "text"
    )
    # horizontal line at threshold
    # add_trace(
    #   x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
    #   y = bead_count_threshold,
    #   type = "scatter",
    #   mode = "lines",
    #   line = list(color = "#be0032", dash = "dash"),
    #   name = "Bead Count Threshold",
    #   text = ~paste0("Bead Count Threhold: ", bead_count_threshold),
    #   hoverinfo = "text"
    #
    # ) %>%
    if (!is.na(lower_threshold)) {
       p <- p %>%   add_trace(
          x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
          y = lower_threshold,
          type = "scatter",
          mode = "lines",
          line = list(color = "#be0032", dash = "dash"),
          name = "Lower Bead Count Threshold",
          text = ~paste0("Lower Bead Count Threhold: ", lower_threshold),
          hoverinfo = "text"

        )
    }
  if (!is.na(upper_threshold)) {
   p <- p %>%  add_trace(
      x = c(0, 100), # 96 wells expected. Add a little buffer room on left and right
      y = upper_threshold,
      type = "scatter",
      mode = "lines",
      line = list(color = "#0067a5", dash = "dash"),
      name = "Upper Bead Count Threshold",
      text = ~paste0("Upper Bead Count Threhold: ", upper_threshold),
      hoverinfo = "text"

    )
  }
    # average bead count
   p <- p %>%  add_trace(
      x = c(0, 100), # add a little buffer room on left and right. Expecting 96 wells.
      y = mean_bead_count,
      type = "scatter",
      mode = "lines",
      line = list(color = "black", dash = "solid"),
      name = "Average Bead Count",
      text = ~paste0("Average Bead Count: ", mean_bead_count),
      hoverinfo = "text"
    ) %>%
    layout(title  = paste("Bead Count Analysis for", data_type ),
           xaxis = list(title = "Well order"),
           yaxis = list(title = "Bead count"),
           font = list(size = 12))


  return(p)
}

# compute gate class of bead count
# bead_count_gc <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
#   if (failed_well_criteria == "upper") {
#     bead_count_threshold <- upper_threshold
#   } else if (failed_well_criteria == "lower") {
#     bead_count_threshold <- lower_threshold
#   }
#   # Add indicator of bead count gate class
#   df_well$bead_count_gc <- ifelse(df_well$n >= bead_count_threshold,
#                                   "Sufficient Bead Count", "Low Bead Count")
#   df_well$bead_count_gc <- factor(df_well$bead_count_gc,
#                                   levels = c("Sufficient Bead Count", "Low Bead Count"))
#
#   # boolean indicator of if it is low or not.
#   df_well$is_low_bead_count <- ifelse(df_well$n >= bead_count_threshold, F, T)
#   # Order the data by well number
#   df_well <- df_well[order(df_well$well_number), ]
#
#   return(df_well)
# }

bead_count_gc <- function(df_well, lower_threshold, upper_threshold, failed_well_criteria) {
  # Determine which threshold to use
  if (failed_well_criteria == "Below_Upper_Threshold" && !is.na(upper_threshold)) {
    bead_count_threshold <- upper_threshold
  } else if (failed_well_criteria == "Below_Lower_Threshold" && !is.na(lower_threshold)) {
    bead_count_threshold <- lower_threshold
  } else if (is.na(upper_threshold) && is.na(lower_threshold)) {
    bead_count_threshold <- NA
    return(df_well[0, ])
  } else if (failed_well_criteria == "Below_Upper_Threshold" && is.na(upper_threshold)) {
    bead_count_threshold <- NA
    return(df_well[0, ])
   } else if (failed_well_criteria == "Below_Lower_Threshold" && is.na(lower_threshold)) {
     bead_count_threshold <- NA
     return(df_well[0, ])
   } else {
    warning("Invalid or missing threshold value. Skipping bead count classification.")
    df_well$bead_count_gc <- NA
    df_well$is_low_bead_count <- NA
    df_well <- df_well[order(df_well$well_number), ]
    return(df_well)
  }

  # Add indicator of bead count gate class
  df_well$bead_count_gc <- ifelse(
    is.na(df_well$n),
    NA,
    ifelse(df_well$n >= bead_count_threshold, "Sufficient Bead Count", "Low Bead Count")
  )
  df_well$bead_count_gc <- factor(
    df_well$bead_count_gc,
    levels = c("Sufficient Bead Count", "Low Bead Count")
  )

  # Boolean indicator
  df_well$is_low_bead_count <- ifelse(
    is.na(df_well$n),
    NA,
    ifelse(df_well$n >= bead_count_threshold, F, T)
  )

  # Order by well number
  df_well <- df_well[order(df_well$well_number), ]
  df_well <- df_well[, c("study_accession", "experiment_accession", "plateid", "visit_name", "patientid",
                         "well", "stype", "sampleid", "arm_name", "dilution", "pctaggbeads", "samplingerrors",
                         "antigen",	"value_reported",	"n", "nominal_sample_dilution", "plate", "plate_nom",
                         "subject_accession"	,"bead_count_gc","is_low_bead_count")]
  return(df_well)
}

# download bead count gate class data
download_bead_count_data <- function(download_df, selected_study, selected_experiment) {
  download_df <- download_df[download_df$experiment_accession == selected_experiment,]
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study, "_",selected_experiment, "_bead_count_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Bead Count Data for ",selected_experiment, " in ", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}



# =============================================================================
# % aggregate beads (QC > Beads > Aggregate Beads)
# -----------------------------------------------------------------------------
# pctaggbeads is read from the instrument file at import and stored per well in
# xmap_sample, xmap_standard, xmap_control and xmap_buffer. It is a property of
# the well (the same on every antigen row), so each well is shown once.
# =============================================================================

PCT_AGG_TYPES <- c(Samples = "#0067a5", Standards = "#2f9e6e", Controls = "#e08a2c", Blanks = "#7b6fa8")

# Plate position of each well: column-wise order (A1, B1 ... H1, A2 ...), the
# same order the Bead Count plot uses. 96-well (A-H x 1-12) and 384-well
# (A-P x 1-24) plates are both handled; the layout is taken from the wells
# present. Returns a data frame(well, row, col, position); NA for a label that
# is not a letter followed by a number.
pct_agg_well_position <- function(wells) {
  w <- toupper(trimws(as.character(wells)))
  row_l <- substr(w, 1, 1)
  col_n <- suppressWarnings(as.integer(substr(w, 2, nchar(w))))
  row_i <- match(row_l, LETTERS)
  bad <- is.na(row_i) | is.na(col_n) | col_n < 1
  row_i[bad] <- NA_integer_; col_n[bad] <- NA_integer_
  n_rows <- if (any(row_i > 8L, na.rm = TRUE) || any(col_n > 12L, na.rm = TRUE)) 16L else 8L
  data.frame(well = w, row = row_i, col = col_n,
             position = (col_n - 1L) * n_rows + row_i, n_rows = n_rows,
             stringsAsFactors = FALSE)
}

# One row per well and well type from the four raw tables. `tabs` is a named
# list(sample=, standard=, control=, blank=) of data frames (any may be NULL or
# lack pctaggbeads). Wells with the same type, plate and nominal dilution are
# collapsed to the highest value across antigen rows.
pct_agg_prep <- function(tabs) {
  kinds <- c(sample = "Samples", standard = "Standards", control = "Controls", blank = "Blanks")
  out <- list()
  for (k in names(kinds)) {
    d <- tabs[[k]]
    if (is.null(d) || !is.data.frame(d) || !nrow(d) || !all(c("well", "pctaggbeads") %in% names(d))) next
    g <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
    plate <- as.character(g("plate")); plate[is.na(plate)] <- as.character(g("plateid"))[is.na(plate)]
    nom <- as.character(g("nominal_sample_dilution"))
    out[[k]] <- data.frame(
      type = kinds[[k]], plate = plate, plate_nom = paste(plate, nom, sep = "-"),
      well = toupper(trimws(as.character(d$well))),
      pctagg = suppressWarnings(as.numeric(d$pctaggbeads)),
      sampleid = as.character(g("sampleid")), patientid = as.character(g("patientid")),
      timeperiod = as.character(g("timeperiod")), dilution = as.character(g("dilution")),
      masked = as.logical(g("masked")), stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  df <- do.call(rbind, out)
  df <- df[order(-ifelse(is.na(df$pctagg), -Inf, df$pctagg)), , drop = FALSE]
  df <- df[!duplicated(df[, c("type", "plate_nom", "well")]), , drop = FALSE]
  pos <- pct_agg_well_position(df$well)
  df$well_position <- pos$position
  df <- df[order(df$plate_nom, df$well_position, df$type), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Plot for one plate: y = percent aggregate beads, x = well position. Points are
# coloured by well type; a dashed line marks the study's pct_agg_threshold (NA =
# none) and wells above it are ringed in red. Returns a plotly object.
pct_agg_plot <- function(df, threshold = NA_real_, title = "Percent aggregate beads by well") {
  df <- df[is.finite(df$pctagg) & is.finite(df$well_position), , drop = FALSE]
  p <- plotly::plot_ly()
  if (!nrow(df))
    return(plotly::layout(plotly::add_trace(p, x = 0, y = 0, type = "scatter", mode = "markers",
                                            marker = list(opacity = 0), hoverinfo = "skip", showlegend = FALSE),
                          annotations = list(text = "No % aggregate beads for this plate", showarrow = FALSE,
                                             x = .5, y = .5, xref = "paper", yref = "paper")))
  thr <- if (length(threshold) && is.finite(threshold[1])) threshold[1] else NA_real_
  df$above <- if (is.na(thr)) FALSE else df$pctagg > thr
  hov <- function(d) paste0("<b>", d$type, "</b><br>Well: ", d$well, "<br>% aggregate beads: ", format(round(d$pctagg, 2), nsmall = 0),
                            ifelse(!is.na(d$sampleid) & nzchar(d$sampleid), paste0("<br>Sample: ", d$sampleid), ""),
                            ifelse(!is.na(d$patientid) & nzchar(d$patientid) & d$patientid != "__none__", paste0("<br>Patient: ", d$patientid), ""),
                            ifelse(!is.na(d$timeperiod) & nzchar(d$timeperiod) & d$timeperiod != "__none__", paste0("<br>Timeperiod: ", d$timeperiod), ""),
                            ifelse(!is.na(d$dilution) & nzchar(d$dilution), paste0("<br>Dilution: ", d$dilution), ""),
                            ifelse(!is.na(d$masked) & d$masked, "<br><i>masked</i>", ""))
  for (ty in names(PCT_AGG_TYPES)) {
    d <- df[df$type == ty, , drop = FALSE]
    if (!nrow(d)) next
    p <- plotly::add_trace(p, data = d, x = ~well_position, y = ~pctagg, type = "scatter", mode = "markers", name = ty,
                           text = hov(d), hoverinfo = "text",
                           marker = list(color = PCT_AGG_TYPES[[ty]], size = 8,
                                         line = list(color = ifelse(d$above, "#be0032", "#ffffff"), width = ifelse(d$above, 2.5, 0.6))))
  }
  rng <- range(df$well_position)
  if (!is.na(thr))
    p <- plotly::add_trace(p, x = c(rng[1] - 1, rng[2] + 1), y = c(thr, thr), type = "scatter", mode = "lines",
                           line = list(color = "#be0032", dash = "dash"), name = "% Agg threshold",
                           text = paste0("% aggregate beads threshold: ", thr), hoverinfo = "text")
  pos <- unique(df[, c("well", "well_position")]); pos <- pos[order(pos$well_position), ]
  step <- if (nrow(pos) > 100L) 4L else 1L; pos <- pos[seq(1L, nrow(pos), by = step), ]
  n_above <- sum(df$above)
  plotly::layout(p, title = paste0(title, if (!is.na(thr)) sprintf(" (%d above the threshold of %s)", n_above, format(thr)) else ""),
                 xaxis = list(title = "Well position (column order: A1, B1, ... )", tickmode = "array",
                              tickvals = pos$well_position, ticktext = pos$well, tickangle = -90, tickfont = list(size = 9)),
                 yaxis = list(title = "% aggregate beads", rangemode = "tozero"), font = list(size = 12))
}

# Wells to list under the plot: those above the threshold, or (no threshold) the
# ten highest.
pct_agg_table <- function(df, threshold = NA_real_) {
  df <- df[is.finite(df$pctagg), , drop = FALSE]
  if (!nrow(df)) return(df[, c("type", "plate", "well", "sampleid", "patientid", "timeperiod", "pctagg"), drop = FALSE])
  df <- df[order(-df$pctagg), , drop = FALSE]
  df <- if (length(threshold) && is.finite(threshold[1])) df[df$pctagg > threshold[1], , drop = FALSE] else utils::head(df, 10L)
  df[, c("type", "plate", "well", "sampleid", "patientid", "timeperiod", "pctagg"), drop = FALSE]
}
