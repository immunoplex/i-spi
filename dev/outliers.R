

# Context initialization function
initialize_context <- function(conn,
                               userWorkSpaceID,
                               selected_study,
                               selected_experiment,
                               selected_type) {
  message("=== Starting initialize_context ===")
  message(sprintf(
    "Checking existing context for workspace_id: %s",
    userWorkSpaceID
  ))

  context_id <- dbGetQuery(
    conn,
    "SELECT id FROM madi_lumi_reader_outliers.main_context
     WHERE workspace_id = $1 AND study = $2 AND experiment = $3 AND value_type = $4;",
    params = list(
      userWorkSpaceID,
      selected_study,
      selected_experiment,
      selected_type
    )
  )$id

  if (is.null(context_id) || length(context_id) == 0) {
    message("No existing context found. Creating new context...")
    context_id <- dbGetQuery(
      conn,
      "INSERT INTO madi_lumi_reader_outliers.main_context
       (workspace_id, study, experiment, value_type, job_status)
       VALUES ($1, $2, $3, $4, $5) RETURNING id;",
      params = list(
        userWorkSpaceID,
        selected_study,
        selected_experiment,
        selected_type,
        "pending"
      )
    )$id
    message(sprintf("Created new context with ID: %s", context_id))
  } else {
    message(sprintf("Found existing context with ID: %s", context_id))
  }

  message("=== initialize_context completed ===")
  return(context_id)
}
computeOutliers <- function(selected_type,
                            multiplier,
                            selected_study,
                            selected_experiment,
                            sample_data_outlier,
                            userWorkSpaceID,
                            conn) {
  tryCatch({
    sample_data_outlier$selected_str <- paste0(
      sample_data_outlier$study_accession,
      sample_data_outlier$experiment_accession
    )
    sample_data_outlier <- sample_data_outlier[sample_data_outlier$selected_str == paste0(selected_study, selected_experiment), ]
    data <- sample_data_outlier

    data <- dplyr::rename(data, arm_name = agroup)
    data <- dplyr::rename(data, visit_name = timeperiod)
    data$subject_accession <- data$patientid

    if (selected_type == "MFI") {
      data <- dplyr::rename(data, value_reported = mfi)
    } else if (selected_type == "Antibody AU") {
      data <- dplyr::rename(data, value_reported = au)
      data$value_reported <- data$value_reported * multiplier
    } else{
      data <- dplyr::rename(data, value_reported = n)
      data$value_reported <- data$value_reported
    }


    arm_choices <- unique(data$arm_name)
    visits <- unique(data$visit_name)
    antigens <- unique(data$antigen)


    context_id <- dbGetQuery(
      conn,
      "SELECT id FROM madi_lumi_reader_outliers.main_context WHERE workspace_id = $1 AND study = $2 AND experiment = $3 AND value_type = $4;",
      params = list(
        userWorkSpaceID,
        selected_study,
        selected_experiment,
        selected_type
      )
    )$id

    if (is.null(context_id) || length(context_id) == 0) {
      context_id <- dbGetQuery(
        conn,
        "INSERT INTO madi_lumi_reader_outliers.main_context
(workspace_id, study, experiment, value_type, job_status)
VALUES ($1, $2, $3, $4, $5) RETURNING id;",
        params = list(
          userWorkSpaceID,
          selected_study,
          selected_experiment,
          selected_type,
          "pending"
        )
      )$id
    }

    # Iterate over each antigen
    i <- length(antigens)
    x <- 0

    for (antigen in antigens) {
      # Generate all pairwise combinations of visits
      print(i)
      print(x)
      x = x + 1
      for (i in seq_along(visits)) {
        for (j in seq_along(visits)) {
          if (i != j) {
            visit_1 <- visits[i]
            visit_2 <- visits[j]

            # Function call (assuming it modifies some shared state)
            apply_function(antigen, visit_1, visit_2)


            # Obtain the plot and filtered data
            #problem here here at visit v00. visit cb1

            rds_and_filtered_data <- calculate_plot_and_ouliers(antigen, visit_1, visit_2, data)

            if (!exists("rds_and_filtered_data") ||
                !("rds_data" %in% names(rds_and_filtered_data)) ||
                !("filtered_data" %in% names(rds_and_filtered_data))) {
              next
            }


            if (!is.null(rds_and_filtered_data$rds_data) &&
                length(rds_and_filtered_data$rds_data) > 0 &&
                !is.null(rds_and_filtered_data$filtered_data) &&
                nrow(rds_and_filtered_data$filtered_data) > 0) {
              # Start a transaction
              dbBegin(conn)
              tryCatch({
                # Insert into comparisons


                # Before the INSERT query
                print("Parameters for comparison insert:")
                print("context_id:")
                print(context_id)
                print("antigen:")
                print(antigen)
                print("visit_1:")
                print(visit_1)
                print("visit_2:")
                print(visit_2)
                print("value_type:")
                print(selected_type)


                comparison_id <- dbGetQuery(
                  conn,
                  "INSERT INTO madi_lumi_reader_outliers.comparisons
               (context_id, antigen, visit_1, visit_2, serialized_plot, value_type)
               VALUES ($1, $2, $3, $4, $5, $6) RETURNING id;",
                  params = list(
                    context_id,
                    antigen,
                    visit_1,
                    visit_2,
                    list(rds_and_filtered_data$rds_data),
                    selected_type
                  )
                )$id

                # Insert each outlier into the outliers table
                filtered_data <- rds_and_filtered_data$filtered_data
                for (outlier in seq_len(nrow(filtered_data))) {
                  row <- filtered_data[outlier, ]

                  visit_1_col <- paste0("Visit : ", visit_1)
                  visit_2_col <- paste0("Visit : ", visit_2)
                  gate_class_1_col <- paste0("Gate Class:  ", visit_1)
                  gate_class_2_col <- paste0("Gate Class:  ", visit_2)

                  dbExecute(
                    conn,
                    "INSERT INTO madi_lumi_reader_outliers.outliers (
    comparison_id,
    subject_accession,
    visit_1,
    visit_2,
    gate_class_1,
    gate_class_2,
    hample_outlier,
    bagplot_outlier,
    kde_outlier,
    antigen,
    feature,
    visit_1_name,
    visit_2_name,
    context_id
  ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14);",
                    params = list(
                      comparison_id,
                      row[["Subject Accession"]],
                      row[[visit_1_col]],
                      row[[visit_2_col]],
                      row[[gate_class_1_col]],
                      # Gate class for visit 1
                      row[[gate_class_2_col]],
                      # Gate class for visit 2
                      row[["Hample Outlier"]],
                      row[["Bagplot Outlier"]],
                      row[["KDE Outlier"]],
                      antigen,
                      selected_experiment,
                      visit_1,
                      visit_2,
                      context_id
                    )
                  )



                }

                # Commit the transaction on success
                dbCommit(conn)
              }, error = function(e) {
                # Rollback the transaction on error
                dbRollback(conn)
                message(
                  paste(
                    "Transaction failed for antigen:",
                    antigen,
                    "with error:",
                    e$message
                  )
                )
              })
            } else {
              # Handle the error case
              warning("Either RDS data or filtered data is missing or empty: Small Data")
            }

          }
        }
      }

    }

    dbExecute(
      conn,
      "UPDATE madi_lumi_reader_outliers.main_context
SET job_status = 'completed'
WHERE workspace_id = $1 AND study = $2 AND experiment = $3 AND value_type = $4;",
      params = list(
        userWorkSpaceID,
        selected_study,
        selected_experiment,
        selected_type
      )
    )

    return(TRUE)
  }, error = function(e) {
    dbExecute(
      conn,
      "UPDATE madi_lumi_reader_outliers.main_context
SET job_status = 'failed'
WHERE workspace_id = $1 AND study = $2 AND experiment = $3 AND value_type = $4;",
      params = list(
        userWorkSpaceID,
        selected_study,
        selected_experiment,
        selected_type
      )
    )

    message("Error in computation: ", e$message)
    return(FALSE)
  })


}




apply_function <- function(antigen, visit_1, visit_2) {
  # Example: just print the combination
  message("Antigen: ", antigen, " - Visit 1: ", visit_1, " - Visit 2: ", visit_2)
}

calculate_plot_and_ouliers <- function(antigen, visit_1, visit_2, data) {
  data_summary <- list()


  visit_ref <- visit_1
  visit1_col <- visit_1
  visit2_col <- visit_2

  filtered_data <- data

  filtered_data$arm_name <- factor(filtered_data$arm_name)
  visits <- c(visit1_col, visit2_col)
  filtered_data <- filtered_data[filtered_data$visit_name %in% visits, ]

  #filtered_data$visit_name <- factor(filtered_data$visit_name)
  #filtered_data$visit_name <- droplevels(filtered_data$visit_name)
  #filtered_data$visit_name <- relevel(filtered_data$visit_name, ref = visit_ref)
  filtered_data <- filtered_data[filtered_data$visit_name %in% visits, ]

  data_exna <- filtered_data


  data_exna <- data_exna[data_exna$antigen == antigen, ]

  data_exna <- data_exna[, c(
    "subject_accession",
    "antigen",
    "visit_name",
    "value_reported",
    "well",
    "feature",
    "gc",
    "plateid"
  )]


  gc_table <- data_exna[, c("subject_accession", "visit_name", "gc")]


  tdata <- as.data.frame(
    pivot_wider(
      data = data_exna,
      id_cols = c("subject_accession", "antigen", "feature"),
      # remove gc from here
      names_from = "visit_name",
      values_from = c("value_reported")  # add gc here to track gc for each visit
    )
  )

  tdata <- dplyr::rename(tdata, vaccinated = visit_1)
  tdata <- dplyr::rename(tdata, boosted = visit_2)

  #tdata$gc <- sapply(tdata$gc, function(x) ifelse(is.null(x), "Not_defined", x))
  #gc_mapping <- c("Not_defined" = 1, "Between_Limits" = 2, "Below_Lower_Limit" = 3, "Above_Upper_Limit" = 4)
  #tdata$subject_accession <- paste0(tdata$subject_accession, "_", gc_mapping[tdata$gc])

  vax_df <- tdata[, c("subject_accession", "vaccinated")]
  vax_df_expanded <- vax_df %>%
    unnest(vaccinated, keep_empty = TRUE)

  bos_df <- tdata[, c("subject_accession", "boosted")]
  bos_df_expanded <- bos_df %>%
    unnest(boosted, keep_empty = TRUE)

  tdata <- merge(bos_df_expanded,
                 vax_df_expanded,
                 by = "subject_accession",
                 allow.cartesian = TRUE)
  #tdata$subject_accession_temp <- tdata$subject_accession
  #tdata$subject_accession <- sub("_.*", "", tdata$subject_accession_temp)
  #tdata$gc_code <- as.numeric(sub(".*_", "", tdata$subject_accession_temp))
  #tdata$gc_code <- 2

  #gc_values <- names(gc_mapping)
  #tdata$gc <- gc_values[as.numeric(tdata$gc)]

  #tdata <- subset(tdata, select = -subject_accession_temp)

  # First create the new columns with "null"
  tdata$gcv1 <- "null"  # for visit1
  tdata$gcv2 <- "null"  # for visit2


  tdata$gc <- 0  # initialize with 0

  for (i in 1:nrow(tdata)) {
    subject <- tdata$subject_accession[i]

    gc_v1 <- gc_table[gc_table$subject_accession == subject &
                        gc_table$visit_name == visit_1, "gc"]
    if (length(gc_v1) > 0) {
      tdata$gcv1[i] <- gc_v1[1]
    }

    gc_v2 <- gc_table[gc_table$subject_accession == subject &
                        gc_table$visit_name == visit_2, "gc"]
    if (length(gc_v2) > 0) {
      tdata$gcv2[i] <- gc_v2[1]
    }

    # Add NA checks
    if (!is.na(tdata$gcv1[i]) && !is.na(tdata$gcv2[i])) {
      if (tdata$gcv1[i] == "Between_Limits" &&
          tdata$gcv2[i] == "Between_Limits") {
        tdata$gc[i] <- 1
      }
    }
  }




  data_summary$median_vacc <- median(tdata$vaccinated, na.rm = TRUE)
  data_summary$null_vacc <- max(tdata$vaccinated, na.rm = TRUE) * 1.1
  median_est_vacc <- median(log(tdata$vaccinated), na.rm = TRUE)
  data_summary$lower_vacc <- exp(median_est_vacc - 3 * mad(
    log(tdata$vaccinated),
    constant = 1,
    na.rm = TRUE
  ))
  data_summary$upper_vacc <- exp(median_est_vacc + 3 * mad(
    log(tdata$vaccinated),
    constant = 1,
    na.rm = TRUE
  ))
  tdata$vaccinated_null <- ifelse(is.na(tdata$vaccinated),
                                  data_summary$null_vacc,
                                  tdata$vaccinated)

  data_summary$median_boos <- median(tdata$boosted, na.rm = TRUE)
  data_summary$null_boos <- max(tdata$boosted, na.rm = TRUE) * 1.1
  median_est_boos <- median(log(tdata$boosted), na.rm = TRUE)
  data_summary$lower_boos <- exp(median_est_boos - 3 * mad(log(tdata$boosted), constant = 1, na.rm = TRUE))
  data_summary$upper_boos <- exp(median_est_boos + 3 * mad(log(tdata$boosted), constant = 1, na.rm = TRUE))
  tdata$boosted_null <- ifelse(is.na(tdata$boosted),
                               data_summary$null_boos,
                               tdata$boosted)

  data_summary$tdata <- tdata


  plot_outlier <- plot_function(data_summary, visit_1, visit_2)

  #problem is here it is not returning something properly.
  return (plot_outlier)


}




plot_function <- function(data_summary, visit_1, visit_2) {
  tdata <- data_summary$tdata


  p_comp5 <- plot_ly()

  p_comp5 <- p_comp5 %>%
    layout(
      title = "Identifying Outliers",
      xaxis = list(
        title = paste("Visit :", visit_1),
        type = "linear",
        # Default to linear
        showgrid = TRUE,
        titlefont = list(
          size = 14,
          color = "black",
          family = "Arial",
          weight = "bold"
        )  # Bold font
      ),
      yaxis = list(
        title = paste("Visit :", visit_2),
        type = "linear",
        # Default to linear
        showgrid = TRUE,
        titlefont = list(
          size = 14,
          color = "black",
          family = "Arial",
          weight = "bold"
        )  # Bold font
      ),
      shapes = list(
        vline(data_summary$median_vacc, color = "darkgreen"),
        # Vertical line
        vline(data_summary$null_vacc, color = "orange"),
        # Vertical line
        hline(data_summary$median_boos, color = "darkgreen"),
        # Horizontal line
        hline(data_summary$null_boos, color = "orange"),
        # Horizontal line
        list(
          type = "rect",
          # Rectangle for bounds
          line = list(color = "lightblue"),
          x0 = data_summary$lower_vacc,
          x1 = data_summary$upper_vacc,
          y0 = data_summary$lower_boos,
          y1 = data_summary$upper_boos
        )
      ),

      # Configure the legend to prevent overlap and customize its layout
      legend = list(
        orientation = "h",
        # Horizontal layout
        x = 0,
        # Left align
        y = -0.2             # Move it below the plot
      ),

      # Add interactive buttons for toggling between linear and log axis scales
      updatemenus = list(
        list(
          type = "buttons",
          direction = "right",
          x = 1.1,
          # Position to the right of the plot
          y = 1.1,
          showactive = TRUE,
          buttons = list(
            list(
              method = "relayout",
              args = list(list(
                xaxis = list(
                  type = "linear",
                  title = paste("Visit :", visit_1),
                  titlefont = list(
                    size = 14,
                    color = "black",
                    family = "Arial",
                    weight = "bold"
                  )
                ),
                yaxis = list(
                  type = "linear",
                  title = paste("Visit :", visit_2),
                  titlefont = list(
                    size = 14,
                    color = "black",
                    family = "Arial",
                    weight = "bold"
                  )
                )
              )),
              label = "Linear"
            ),
            list(
              method = "relayout",
              args = list(list(
                xaxis = list(
                  type = "log",
                  title = paste("Visit :", visit_1),
                  titlefont = list(
                    size = 14,
                    color = "black",
                    family = "Arial",
                    weight = "bold"
                  )
                ),
                yaxis = list(
                  type = "log",
                  title = paste("Visit :", visit_2),
                  titlefont = list(
                    size = 14,
                    color = "black",
                    family = "Arial",
                    weight = "bold"
                  )
                )
              )),
              label = "Log"
            )
          )
        )
      )
    )



  tdata_null <- tdata[is.na(tdata$vaccinated) |
                        is.na(tdata$boosted), ]
  tdata <- tdata[!(is.na(tdata$vaccinated) |
                     is.na(tdata$boosted)), ]


  if (nrow(tdata) > 20) {
    # Define the rectangular boundary from reactive_data
    x0 <- data_summary$lower_vacc
    x1 <- data_summary$upper_vacc
    y0 <- data_summary$lower_boos
    y1 <- data_summary$upper_boos

    # Mark points as rectangle outliers in tdata
    tdata$rectangle_outlier <- apply(tdata, 1, function(row) {
      x <- as.numeric(row["vaccinated"])
      y <- as.numeric(row["boosted"])

      # Check if the point is outside the rectangle
      is_outside <- (x < x0 | x > x1 | y < y0 | y > y1)
      return(is_outside)
    })


    tdata$log10vaccinated <- log10(tdata$vaccinated + 1)
    tdata$log10boosted <- log10(tdata$boosted + 1)



    # Compute the bagplot
    valid_data <- !is.na(tdata$vaccinated) &
      !is.na(tdata$boosted) &
      is.finite(tdata$vaccinated) &
      is.finite(tdata$boosted)

    clean_vaccinated <- tdata$vaccinated[valid_data]
    clean_boosted <- tdata$boosted[valid_data]

    pcbagplot5 <- NULL

    cat("Attempting bagplot computation with explicit parameters...\n")
    tryCatch({
      pcbagplot5 <- compute.bagplot(
        x = clean_vaccinated,
        y = clean_boosted,
        factor = 3,
        na.rm = TRUE,
        approx.limit = 300,
        dkmethod = 2,
        precision = 1,
        verbose = TRUE,
        debug.plots = "no"
      )
      cat("Bagplot computation completed successfully\n")
    }, error = function(e) {
      cat(sprintf("Error in compute.bagplot: %s\n", e$message))
      cat("\nData characteristics:\n")
      cat(sprintf(
        "Range of x: [%f, %f]\n",
        min(clean_vaccinated),
        max(clean_vaccinated)
      ))
      cat(sprintf(
        "Range of y: [%f, %f]\n",
        min(clean_boosted),
        max(clean_boosted)
      ))
      # Ensure pcbagplot5 remains NULL in case of error
      pcbagplot5 <- NULL
    })



    # Extract components from pcbagplot5
    hull.bag <- if (!is.null(pcbagplot5$hull.bag))
      data.frame(pcbagplot5$hull.bag)
    else
      NULL
    hull.loop <- if (!is.null(pcbagplot5$hull.loop))
      data.frame(pcbagplot5$hull.loop)
    else
      NULL
    pxy.outer <- if (!is.null(pcbagplot5$pxy.outer))
      data.frame(pcbagplot5$pxy.outer)
    else
      NULL
    pxy.bag <- if (!is.null(pcbagplot5$pxy.bag))
      data.frame(pcbagplot5$pxy.bag)
    else
      NULL
    pxy.outlier <- if (!is.null(pcbagplot5$pxy.outlier))
      data.frame(pcbagplot5$pxy.outlier)
    else
      NULL


    if (!is.null(pxy.outlier)) {
      # Mark tdata$bagplot_outlier as TRUE or FALSE based on whether the point is in pxy.outlier
      tdata$bagplot_outlier <- apply(tdata, 1, function(row) {
        x <- as.numeric(row["vaccinated"])  # Ensure the values are numeric
        y <- as.numeric(row["boosted"])

        # Check if this (x, y) pair matches any point in pxy.outlier
        is_outlier <- any(pxy.outlier[, 1] == x &
                            pxy.outlier[, 2] == y)

        return(is_outlier)
      })
    } else {
      # If pxy.outlier is NULL, no points are considered outliers
      tdata$bagplot_outlier <- FALSE
    }


    # Add the loop polygon if available
    if (!is.null(hull.loop) && nrow(hull.loop) > 0) {
      # Ensure the polygon is closed by adding the first point to the end
      closed_hull <- rbind(hull.loop, hull.loop[1, ])

      p_comp5 <- p_comp5 %>%
        add_polygons(
          x = closed_hull[, 1],
          y = closed_hull[, 2],
          fillcolor = "rgba(170, 204, 255, 0.1)",
          # Very light shading
          line = list(color = "rgba(170, 204, 255, 1)"),
          # Outline color
          name = "Loop"
        )
    }


    # Add the bag polygon if available
    if (!is.null(hull.bag) && nrow(hull.bag) > 0) {
      # Ensure the polygon is closed by adding the first point to the end
      closed_bag <- rbind(hull.bag, hull.bag[1, ])

      p_comp5 <- p_comp5 %>%
        add_polygons(
          x = closed_bag[, 1],
          y = closed_bag[, 2],
          fillcolor = "rgba(119, 153, 255, 0.1)",
          # Very light shading
          line = list(color = "rgba(119, 153, 255, 1)"),
          # Outline color
          name = "Bag"
        )
    }


    # Step 1: Data preparation
    log_vaccinated <- log10(tdata$vaccinated)
    log_boosted <- log10(tdata$boosted)

    clean_data <- cbind(log_vaccinated, log_boosted)
    clean_data <- clean_data[complete.cases(clean_data) &
                               is.finite(rowSums(clean_data)), ]

    # Step 2: Check data validity and perform KDE
    if (!anyNA(clean_data) && nrow(clean_data) > 2) {
      # Compute density scores
      density_scores <- calc_kde_scores(as.matrix(clean_data))
      if (is.null(density_scores)) {
        showNotification("Not enough data points after cleaning to perform KDE.")
        return(NULL)
      }
      density_values <- exp(-density_scores$scores)  # Convert log-scores to density values
    } else {
      showNotification("Not enough data points after cleaning to perform KDE.")
      return(NULL)
    }

    # Step 3: Calculate outliers
    outlier_threshold <- 0.01  # Define threshold for outliers (bottom 1%)
    outlier_flag <- density_values < quantile(density_values, outlier_threshold)
    tdata$kde_outlier <- outlier_flag

    # Step 4: Create the plot
    p_comp5 <- p_comp5 %>%
      add_markers(
        x = tdata$vaccinated,
        y = tdata$boosted,
        marker = list(
          color = density_values,
          colorscale = list(
            c(0, 'orange'),
            c(0.5, 'yellow'),
            c(0.9, 'green'),
            c(1, 'blue')
          ),
          size = 6,
          symbol = ifelse(
            tdata$rectangle_outlier & tdata$kde_outlier & tdata$bagplot_outlier,
            ifelse(tdata$gc == 0, "cross-open", "cross"),
            ifelse((
              tdata$rectangle_outlier + tdata$kde_outlier + tdata$bagplot_outlier
            ) == 2,
            ifelse(tdata$gc == 0, "star-open", "star"),
            ifelse((
              tdata$rectangle_outlier + tdata$kde_outlier + tdata$bagplot_outlier
            ) == 1,
            ifelse(tdata$gc == 0, "triangle-up-open", "triangle-up"),
            ifelse(tdata$gc == 0, "circle-open", "circle")
            )
            )
          ),
          colorbar = list(
            title = "Density",
            tickvals = c(
              min(density_values),
              mean(density_values),
              max(density_values)
            ),
            ticktext = c("Low", "Medium", "High")
          )
        ),
        hoverinfo = "text",
        hovertext = paste(
          "Subject Accession:",
          tdata$subject_accession,
          "<br>",
          paste("X - Visit ", visit_1, ": "),
          tdata$vaccinated,
          "<br>",
          paste("Y - Visit ", visit_2, ": "),
          tdata$boosted,
          "<br>",
          paste0(visit_1, " Gate Class: ", tdata$gcv1),
          "<br>",
          paste0(visit_2, " Gate Class: ", tdata$gcv2),
          "<br>",
          "Hample Outlier:",
          tdata$rectangle_outlier,
          "<br>",
          "KDE Outlier:",
          tdata$kde_outlier,
          "<br>",
          "Bagplot Outlier:",
          tdata$bagplot_outlier
        ),
        name = "Bivariate MFI Value point"
      )

    filtered_data <- tdata %>%
      filter(rectangle_outlier == TRUE |
               bagplot_outlier == TRUE | kde_outlier == TRUE)
    filtered_data <- subset(
      filtered_data,
      select = -c(
        gc,
        vaccinated_null,
        boosted_null,
        log10vaccinated,
        log10boosted
      )
    )

    colnames(filtered_data)[colnames(filtered_data) == "subject_accession"] <- "Subject Accession"
    colnames(filtered_data)[colnames(filtered_data) == "vaccinated"] <- paste("Visit :", visit_1)
    colnames(filtered_data)[colnames(filtered_data) == "boosted"] <- paste("Visit :", visit_2)
    colnames(filtered_data)[colnames(filtered_data) == "gcv1"] <- paste("Gate Class: " , visit_1)
    colnames(filtered_data)[colnames(filtered_data) == "gcv2"] <- paste("Gate Class: " , visit_2)
    colnames(filtered_data)[colnames(filtered_data) == "rectangle_outlier"] <- "Hample Outlier"
    colnames(filtered_data)[colnames(filtered_data) == "bagplot_outlier"] <- "Bagplot Outlier"
    colnames(filtered_data)[colnames(filtered_data) == "kde_outlier"] <- "KDE Outlier"



  }


  if (nrow(tdata_null) > 0) {
    p_comp5 <- p_comp5 %>%
      add_markers(
        x = tdata_null$vaccinated_null,
        # Plot using vaccinated_null
        y = tdata_null$boosted_null,
        # Plot using boosted_null
        marker = list(color = "red", size = 5),
        hoverinfo = "text",
        # Enable hover text
        hovertext = paste(
          "Subject Accession:",
          tdata_null$subject_accession,
          "<br>",
          paste("X - Visit ", visit_1, ":"),
          tdata_null$vaccinated,
          "<br>",
          # Display actual vaccinated values
          paste("Y - Visit ", visit_2, ":"),
          tdata_null$boosted,
          "<br>",
          # Display actual boosted values
          paste0(visit_1, " Gate Class: ", tdata_null$gcv1),
          "<br>",
          paste0(visit_2, " Gate Class: ", tdata_null$gcv2)

        ),
        name = "Exceptions"
      )
  }


  rds_data <- serialize(p_comp5, NULL)

  #retrieved_plot <- unserialize(rds_data)

  combined_data <- list(
    rds_data = if (exists("rds_data"))
      rds_data
    else
      NULL,
    filtered_data = if (exists("filtered_data"))
      filtered_data
    else
      NULL
  )

  return (combined_data)


}


calc_kde_scores <- function(y,
                            num_classes = 10,
                            outlier_threshold = 0.01,
                            ...) {
  n <- NROW(y)  # Number of rows (observations)
  d <- NCOL(y)  # Number of columns (dimensions)

  # Check and clean data for Inf or NA values
  clean_data <- y[complete.cases(y) & is.finite(rowSums(y)), ]

  # If there are too few data points after cleaning, exit the function
  if (nrow(clean_data) <= 2) {
    showNotification("Not enough data points after cleaning to perform KDE.")
    return(NULL)  # Gracefully exit the function without further processing
  }

  # Bandwidth estimation using ks::Hpi
  H <- ks::Hpi(x = clean_data)  # Use Hpi for bandwidth selection

  # For univariate case (1D)
  if (d == 1L) {
    gridsize <- 10001
    h <- H  # Hpi will return a scalar for univariate data as h
    K0 <- 1 / (h * sqrt(2 * pi))  # Normalization constant for KDE
    fi <- ks::kde(
      clean_data,
      h = h,
      gridsize = gridsize,
      binned = n > 2000,
      eval.points = clean_data,
      compute.cont = FALSE,
      ...
    )$estimate
  }
  # For multivariate case (2D or higher)
  else {
    gridsize <- 101
    K0 <- det(H) ^ (-1 / 2) * (2 * pi) ^ (-d / 2)  # Normalization constant for multivariate KDE
    fi <- ks::kde(
      clean_data,
      H = H,
      gridsize = gridsize,
      binned = n > 2000,
      eval.points = clean_data,
      compute.cont = FALSE,
      ...
    )$estimate
  }

  # Safeguard against small or zero densities
  fi <- pmax(fi, 1e-10)  # Avoid zero densities that would cause -log(0) issues

  # Calculate leave-one-out (LOO) and KDE scores
  loo_scores <- -log(pmax(0, (n * fi - K0) / (n - 1)))
  scores <- -log(pmax(0, fi))

  # Step 1: Dynamic classification
  density_values <- exp(-scores)  # Convert log-scores back to densities

  # Handle infinite density values
  density_values[!is.finite(density_values)] <- 1e-10  # Replace Inf with a small value

  # Create density thresholds dynamically based on percentiles
  thresholds <- quantile(density_values, probs = seq(0, 1, length.out = num_classes + 1))

  # Check if thresholds are unique, if not, perturb them slightly
  if (anyDuplicated(thresholds)) {
    thresholds <- thresholds + cumsum(c(0, rep(1e-10, length(thresholds) - 1)))
  }

  # Step 2: Assign density classes
  density_groups <- cut(
    density_values,
    breaks = thresholds,
    labels = FALSE,
    include.lowest = TRUE
  )

  # Step 3: Mark outliers (densities below the outlier threshold)
  outlier_flag <- density_values < quantile(density_values, outlier_threshold)

  # Assign red color for outliers and dynamic colors for other points
  colors <- colorRampPalette(c("blue", "green", "yellow", "orange"))(num_classes)
  point_colors <- ifelse(outlier_flag, "red", colors[density_groups])

  return(
    list(
      scores = scores,
      loo_scores = loo_scores,
      density_groups = density_groups,
      colors = point_colors,
      outliers = outlier_flag
    )
  )
}

vline <- function(x = 0, color = "red") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

hline <- function(y = 0, color = "red") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


observeEvent(input$outlierDownload, {
  # Show the progress modal

  selected_study <- selected_studyexpplate$study_accession
  selected_experiment <- selected_studyexpplate$experiment_accession


  context_id <- dbGetQuery(
    conn,
    "SELECT id FROM madi_lumi_reader_outliers.main_context WHERE workspace_id = $1 AND study = $2 AND experiment = $3;",
    params = list(userWorkSpaceID(), selected_study, selected_experiment)
  )$id


  showModal(
    modalDialog(
      title = "Preparing Download",
      "Your data is being prepared. This may take a few moments.",
      footer = NULL,
      easyClose = FALSE,
      fade = TRUE,
      tags$div(
        class = "progress",
        tags$div(
          id = "progress-bar-modal",
          class = "progress-bar progress-bar-striped progress-bar-animated",
          role = "progressbar",
          style = "width: 100%",
          "Loading..."
        )
      )
    )
  )

  # Attempt to query the database and download the file
  tryCatch({
    # Query database for all outliers for the given context ID
    outliers_query <- "SELECT * FROM madi_lumi_reader_outliers.outliers
                       WHERE context_id = $1"
    outlier_data <- dbGetQuery(conn, outliers_query, params = list(context_id))


    # Specify download handler to create the file
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("all-outliers-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(outlier_data, file, row.names = FALSE)
      }
    )

    # Trigger download automatically
    shinyjs::runjs("$('#downloadData')[0].click();")

  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("An error occurred: ", e$message),
      easyClose = TRUE
    ))
  }, finally = {
    # Ensure the modal is removed once finished
    removeModal()
  })
})
