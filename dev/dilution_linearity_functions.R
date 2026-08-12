
# get_dilution_parameters <- function(study_accession, project_id) {
#   query <- paste0("SELECT study_accession, node_order, valid_gate_class, is_binary_gate
#   FROM madi_results.xmap_dilution_parameters
#   WHERE project_id = ", project_id, " AND study_accession IN ('", study_accession,"');")
#
#   result_df <- dbGetQuery(conn, query)
# }

# fetch_sample_data_linearity <- function(study_accession, experiment_accession) {
#   query <- paste0("SELECT study_accession, experiment_accession, patientid, plate_id, timeperiod, antigen, dilution, agroup, antibody_mfi, gate_class, floor(100000*antibody_au)/100 AS AU, in_linear_region, in_quantifiable_range
# 	FROM madi_results.xmap_sample
# 	WHERE study_accession IN ('",study_accession,"')
# 	AND experiment_accession IN ('",experiment_accession,"')
#                   ORDER BY patientid, timeperiod, dilution;")
#
#   # Run the query and fetch the result as a data frame
#   result_df <- dbGetQuery(conn, query)
# }

# fetch_standard_curves_dilution <- function(study_accession, experiment_accession, bkg_method, is_log_mfi) {
#   query <- paste0("
#     SELECT DISTINCT ON (study_accession, experiment_accession, plateid,
#            antigen) study_accession, experiment_accession, plateid,
#            antigen,  iter, status, crit, l_asy, r_asy, x_mid, scale, bendlower, bendupper,
#            llod, ulod, loglik, aic, bic, deviance, dfresidual, nobs, rsquare_fit, source, g, lloq, uloq, loq_method, bkg_method
#     FROM madi_results.xmap_standard_fits
#     WHERE study_accession = '", study_accession, "'
#     AND experiment_accession = '", experiment_accession, "'
#     AND bkg_method = '", bkg_method, "'
#     and is_log_mfi_axis = '", is_log_mfi,"'
#     ORDER BY study_accession, experiment_accession, plateid,
#            antigen, loglik")
#
#
#   # Run the query and fetch the result as a data frame
#   result_df <- dbGetQuery(conn, query)
#
# }


# Helper: map a node name → the column that stores its
node_to_col <- function(node) {
  if (node == "limits_of_detection") {
    return("gate_class_lod")
  } else if (node == "limits_of_quantification") {
    return("gate_class_loq")
  }
  #stop(glue("Unsupported node: {node}"))
}



# -------------------------------------------------
calculate_sample_concentration_status_new <- function(
    conn,                # DBI connection object
    study_accession,
    experiment_accession,
    project_id,
    node_order = c("limits_of_quantification")   # any permutation of the two nodes
) {

  query_samples <- glue::glue("
                  SELECT se.best_sample_se_all_id, se.raw_predicted_concentration, se.study_accession, se.experiment_accession,
                  se.timeperiod, se.patientid, se.well, se.stype, se.sampleid, se.agroup, se.pctaggbeads, se.samplingerrors, se.antigen,
                  se.antibody_n, se.plateid, se.plate, se.sample_dilution_factor,
                  se.assay_response_variable, se.assay_independent_variable, se.dilution, se.overall_se, se.assay_response, se.norm_assay_response, se.se_concentration,
                  se.final_predicted_concentration as au, se.pcov, se.source, se.gate_class_loq, se.gate_class_lod, se.gate_class_pcov,
                  se.best_glance_all_id, g.is_log_response, g.is_log_x
                  FROM madi_results.best_sample_se_all se
                  LEFT JOIN madi_results.best_glance_all g
                      ON se.best_glance_all_id = g.best_glance_all_id
                  WHERE se.project_id = {project_id}
                  AND se.study_accession = '{study_accession}'
                  AND se.experiment_accession = '{experiment_accession}';")

  df <- dbGetQuery(conn, query_samples)

  print("node order: \n")
  print(node_order)

  # -----------------------------------------------------------------
  # 2  NA‑filter **per node** (remove rows that do not have a QC flag
  #     for any node that is part of the workflow)  AND create pass flags
  # -----------------------------------------------------------------
  for (node in node_order) {
    col_name  <- node_to_col(node)                 # real column name in the DB
    safe_name <- make.names(node)                   # e.g. "limits_of_detection"
    flag_name <- paste0("pass_", safe_name)         # e.g. "pass_limits_of_detection"

    # (i)  Drop rows where the QC column is missing (NA)
    df <- df %>% filter(!is.na(.data[[col_name]]))

    # (ii) Create a logical flag: TRUE only when the value equals "Acceptable"
    df[[flag_name]] <- df[[col_name]] == "Acceptable"
  }

  # -----------------------------------------------------------------
  # 3  Overall pass/fail = AND of all the per‑node pass flags
  # -----------------------------------------------------------------
  pass_cols <- paste0("pass_", make.names(node_order))
  df$overall_pass <- apply(df[pass_cols], 1, all, na.rm = FALSE)

  # # -----------------------------------------------------------------
  # # 4  concentration_status = raw value of the *last* node in node_order
  # # -----------------------------------------------------------------
  # last_node <- tail(node_order, 1)
  # last_col  <- node_to_col(last_node)
  # df$concentration_status <- df[[last_col]]

  # -----------------------------------------------------------------
  # 4 Build `concentration_status`
  #     - If the sample passed all nodes → keep the value of the last node
  #       (which will be “Acceptable”).
  #     - If the sample failed any node → use the **first** node that is
  #       not Acceptable and copy its raw gate‑class value.
  # -----------------------------------------------------------------
  # Helper: a vector with the real column names in the same order as node_order
  node_cols <- vapply(node_order, node_to_col, character(1))

  # Initialise with the value of the **last** node (covers the “all pass” case)
  df$concentration_status <- df[[ tail(node_cols, 1) ]]

  # Identify rows that failed (overall_pass == FALSE)
  fail_idx <- which(!df$overall_pass)

  if (length(fail_idx) > 0) {
    # For each failing row, find the *first* node whose pass flag is FALSE
    for (i in fail_idx) {
      # which flag is FALSE for this row?
      false_flags <- which(!df[i, pass_cols])
      # take the first one (if more than one node failed)
      first_false <- false_flags[1]
      # corresponding column name in the original data
      fail_col <- node_cols[first_false]
      # copy the raw gate‑class value into concentration_status
      df$concentration_status[i] <- df[[fail_col]][i]
    }
  }

  df$Classification <- ifelse(df$overall_pass,
                              "Pass Classification",
                              "Does Not Pass Classification")

  # # -------------------------------------------------
  # #  Filter per‑node:
  # #      -drop rows where the required QC column is NA
  # #      - keep only rows that are "Acceptable" for that node
  # #    -  Remember the column of the last successful node – that will become
  # #      the `concentration_status`
  # # -------------------------------------------------
  # last_col <- NULL
  #
  # for (node in node_order) {
  #   col_name <- node_to_col(node)               # e.g. "gate_class_lod"
  #   #(i) Remove rows that are missing the QC flag required for this node
  #   df <- df %>% filter(!is.na(.data[[col_name]]))
  #   # (ii) Keep only rows that are "Acceptable" for this node
  #   df <- df %>% filter(.data[[col_name]] == "Acceptable")
  #   last_col <- col_name
  # }
  #
  # # -------------------------------------------------
  # # 4️⃣ Populate `concentration_status`
  # # -------------------------------------------------
  # if (is.null(last_col)) {
  #   df <- df %>% mutate(concentration_status = NA_character_)
  # } else {
  #   df <- df %>% mutate(concentration_status = .data[[last_col]])
  # }
#
#   df <- df %>%
#     mutate(
#       Classification = ifelse(
#         concentration_status == "Acceptable",
#         "Pass Classification",
#         "Does Not Pass Classification"
#       )
#     )

  return(df)
}


# ### Read in data and find concentration status
# calculate_sample_concentration_status <- function(study_accession, experiment_accession, node_order) {
#   query_samples <- glue::glue("SELECT xmap_sample_id, study_accession, experiment_accession, plate_id, timeperiod, patientid, well, stype, sampleid, id_imi, agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, antibody_au, antibody_au_se, norm_mfi, gate_class, in_linear_region, gate_class_linear_region,in_quantifiable_range, gate_class_loq,  quality_score
# 	FROM madi_results.xmap_sample
# 	WHERE study_accession = '{study_accession}'
# 	AND experiment_accession = '{experiment_accession}';")
#
#   sample_data_da <-  dbGetQuery(conn, query_samples)
#
#   # filter out samples with not evaluated on all 3
#   sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated" & sample_data_da$gate_class_linear_region != "Not Evaluated"
#                                    & sample_data_da$gate_class_loq != "Not Evaluated",]
#
#   cat("node_order")
#   print(node_order)
#  # sample_data_da_v <- sample_data_da
#
#   if (identical(node_order, c("limits_of_quantification"))) {
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_loq != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_loq
#   }  else if (identical(node_order, c("linear_region"))) {
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_linear_region != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_linear_region
#   } else if (identical(node_order, c("linear_region", "limits_of_quantification"))) {
#     linear_passed <- sample_data_da$gate_class_linear == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_loq != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_loq
#   } else if (identical(node_order, c("limits_of_quantification", "linear_region"))) {
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_linear_region != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_linear_region
#   } else if (identical(node_order, c("limits_of_detection"))) {
#     sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class
#   } else if (identical(node_order, c("limits_of_detection", "linear_region"))) {
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_linear_region != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_linear_region
#   } else if (identical(node_order, c("linear_region", "limits_of_detection"))) {
#     linear_passed <- sample_data_da$gate_class_linear_region == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class
#   } else if (identical(node_order, c("limits_of_detection", "limits_of_quantification"))) {
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_loq != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_loq
#   } else if (identical(node_order, c("limits_of_quantification", "limits_of_detection"))) {
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class
#   } else if (identical(node_order, c("limits_of_detection", "linear_region", "limits_of_quantification"))) {
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     linear_passed <- sample_data_da$gate_class_linear_region == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_loq != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_loq
#   } else if (identical(node_order, c("limits_of_detection", "limits_of_quantification", "linear_region"))) {
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_linear_region != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_linear_region
#   } else if (identical(node_order, c("linear_region", "limits_of_detection", "limits_of_quantification"))) {
#     linear_passed <- sample_data_da$gate_class_linear_region == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_loq != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_loq
#   } else if (identical(node_order, c("linear_region", "limits_of_quantification", "limits_of_detection"))) {
#     linear_passed <- sample_data_da$gate_class_linear_region == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class
#   } else if (identical(node_order, c("limits_of_quantification", "limits_of_detection", "linear_region"))){
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     gate_class_passed <- sample_data_da$gate_class == "Acceptable"
#     sample_data_da <- sample_data_da[gate_class_passed,]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class_linear_region != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class_linear_region
#   } else if (identical(node_order, c("limits_of_quantification", "linear_region", "limits_of_detection"))) {
#     quantifiable_passed <- sample_data_da$gate_class_loq == "Acceptable"
#     sample_data_da <- sample_data_da[quantifiable_passed, ]
#     linear_passed <- sample_data_da$gate_class_linear_region == "Acceptable"
#     sample_data_da <- sample_data_da[linear_passed, ]
#     sample_data_da <- sample_data_da[sample_data_da$gate_class != "Not Evaluated",]
#     sample_data_da$concentration_status <- sample_data_da$gate_class
#   }
#
#   ## Classification based on tree (node order)
#   sample_data_da$Classification <- ifelse(sample_data_da$concentration_status == "Acceptable", "Pass Classification", "Does Not Pass Classification")
#
#   # add plateid
#   sample_data_da$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", sample_data_da$plate_id, fixed=TRUE)))
#
#
#   return(sample_data_da)
#
# }

## 1st table independently get sample and standards joined
join_sample_standard_data <- function(study_accession, experiment_accession, bkg_method, is_log_mfi_axis, node_order, valid_gate_class) {

  cat("log MFI?: \n")
  print(is_log_mfi_axis)

  cat("Node Order\n")
  print(node_order)

  query_standard_fits <<- glue::glue("SELECT DISTINCT study_accession,
experiment_accession, plateid, antigen,
iter, status, crit, bendlower, bendupper, llod, ulod, source, lloq, uloq, loq_method, bkg_method, is_log_mfi_axis
	FROM madi_results.xmap_standard_fits
	WHERE study_accession = '{study_accession}'
	AND experiment_accession = '{experiment_accession}'
	and bkg_method = '{bkg_method}'
  AND is_log_mfi_axis = '{is_log_mfi_axis}'")


#   query_samples <- glue::glue("SELECT DISTINCT study_accession,
# experiment_accession, plate_id, antigen, dilution, agroup,
# patientid, timeperiod, antibody_mfi, gate_class, floor(100000*antibody_au)/100 AS au, in_linear_region, in_quantifiable_range
# 	FROM madi_results.xmap_sample
# 	WHERE study_accession = '{study_accession}'
# 	AND experiment_accession = '{experiment_accession}';"
#   )
#   query_samples <- glue::glue("SELECT * FROM (
# 		SELECT DISTINCT ON (
# 	    study_accession, experiment_accession, plate_id, antigen, dilution, agroup,
# 	    patientid, timeperiod, well
# 	) study_accession, experiment_accession, plate_id, antigen, dilution, agroup,
# patientid, timeperiod,well, antibody_mfi, gate_class, floor(100000*antibody_au)/100 AS au, in_linear_region, in_quantifiable_range
# 	FROM madi_results.xmap_sample
# 	WHERE study_accession = '{study_accession}'
# 	 AND experiment_accession = '{experiment_accession}'
# 	ORDER BY
# 	    study_accession,
# 	    experiment_accession,
# 	    plate_id,
# 	    antigen,
# 	    dilution,
# 	    agroup,
# 	    patientid,
# 	    timeperiod,
# 	    well
# 	 ) as distinct_samples")
  #floor(100000*antibody_au)/100
  query_samples <<- glue::glue("SELECT DISTINCT ON (plate_id, antigen, dilution, patientid, agroup, timeperiod, well)
study_accession, experiment_accession, plate_id, antigen, dilution, agroup,
patientid, timeperiod,well, antibody_mfi, antibody_au, gate_class, antibody_au AS au, in_linear_region, in_quantifiable_range
	FROM madi_results.xmap_sample
	WHERE study_accession = '{study_accession}'
	 AND experiment_accession = '{experiment_accession}'
 AND antibody_au IS NOT NULL
	ORDER BY
	    plate_id,
	    antigen,
	    dilution,
	    patientid,
		agroup,
	    timeperiod,
	    well")



  standard_fits <<- dbGetQuery(conn, query_standard_fits)
  sample_data <- dbGetQuery(conn, query_samples)
  sample_data <- sample_data
  sample_data_da <<- sample_data
  # rename plate_id variable to match when joining.
  sample_data$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", sample_data$plate_id, fixed=TRUE)))
  if (is_log_mfi_axis) {
    sample_data$antibody_mfi <- log10(sample_data$antibody_mfi)
  }

  # Predicted dilution fraction is back calculated from the rau
  #to represent the patient/time period specific prediction of the equivalent log10 dilution factor from the standard curve
  sample_data$predicted_dilution_fraction <- log10(sample_data$antibody_au/1000)

  joined_data <- merge(
    standard_fits,
    sample_data,
    by = c("study_accession","experiment_accession", "plateid", "antigen"),
    all.x = TRUE
  )

  #add the dilution fraction
  #joined_data$dilution_fraction <- log10(1 / joined_data$dilution)

  if (identical(node_order, c("quantifiable"))) {
    joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq),]


    joined_data$concentration_status <- ifelse(joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
                                               ifelse(joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated", "Acceptable"))
  } else if (identical(node_order, c("linear"))) {
  joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]


  joined_data$concentration_status <- ifelse(joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
                                             ifelse(joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated", "Acceptable"))
  } else if (identical(node_order, c("linear", "quantifiable"))) {

    joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper),]

    joined_data$concentration_status <- ifelse(
      joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
      ifelse(
        joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
        "Acceptable"  # mark as acceptable linear range for next step
      )
    )

    linear_passed <- joined_data$concentration_status == "Acceptable"#is.na(joined_data$concentration_status)
    joined_data <- joined_data[linear_passed, ]

    # Step 2: check quantification limits on those that passed linear
    joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

    joined_data$concentration_status <- ifelse(
      joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
      ifelse(
        joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
        "Acceptable"
      )
    )
  } else if (identical(node_order, c("quantifiable", "linear"))) {
    # Step 1: filter by quantifiable limits
    joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

    joined_data$concentration_status <- ifelse(
      joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
      ifelse(
        joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
        "Acceptable"  # explicitly assign Acceptable for quantifiable
      )
    )

    # Keep only samples that passed quantifiable check
    quantifiable_passed <- joined_data$concentration_status == "Acceptable"
    joined_data <- joined_data[quantifiable_passed, ]

    # Step 2: check linear range on those that passed quantifiable region
    joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

    joined_data$concentration_status <- ifelse(
      joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
      ifelse(
        joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
        "Acceptable"
      )
    )
  }  else if ("gate" %in% node_order) {
    joined_data <- joined_data[!is.na(joined_data$llod) & !is.na(joined_data$ulod), ]
    joined_data$gate_class <- ifelse(joined_data$antibody_mfi < joined_data$llod, "Below_Lower_Limit",
                                     ifelse(joined_data$antibody_mfi > joined_data$ulod, "Above_Upper_Limit", "Between_Limits"))

    # gate_class_passed <- joined_data$gate_class %in% valid_gate_class
    # joined_data <- joined_data[gate_class_passed,]
    #
    if (identical(node_order, "gate")) {
      # First check if it is valid by the user; then do the default status.
      joined_data$concentration_status <- ifelse(joined_data$gate_class %in% valid_gate_class, "Acceptable",
                                                 ifelse(joined_data$gate_class == "Above_Upper_Limit", "Too Concentrated",
                                                        ifelse(joined_data$gate_class == "Below_Lower_Limit", "Too Diluted", "Acceptable")))
    } else if (identical(node_order, c("gate", "linear"))) {
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]

      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"
        )
      )
    } else if (identical(node_order, c("linear", "gate"))) {
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )

      linear_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[linear_passed, ]

      joined_data$concentration_status <- ifelse(joined_data$gate_class %in% valid_gate_class, "Acceptable",
                                                 ifelse(joined_data$gate_class == "Above_Upper_Limit", "Too Concentrated",
                                                        ifelse(joined_data$gate_class == "Below_Lower_Limit", "Too Diluted", "Acceptable")))

    } else if (identical(node_order, c("gate", "quantifiable"))) {
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]

      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"
        )
      )

    } else if (identical(node_order, c("quantifiable", "gate"))) {
      # filter by quantifiable limits
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )

      # Keep only samples that passed quantifiable check
      quantifiable_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[quantifiable_passed, ]

      # then do gate class
      joined_data$concentration_status <- ifelse(joined_data$gate_class %in% valid_gate_class, "Acceptable",
                                                 ifelse(joined_data$gate_class == "Above_Upper_Limit", "Too Concentrated",
                                                        ifelse(joined_data$gate_class == "Below_Lower_Limit", "Too Diluted", "Acceptable")))

    } else if (identical(node_order, c("gate", "linear", "quantifiable"))) {
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]

      # now filter by linear
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper),]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )

      linear_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[linear_passed, ]

      #now quantifiable
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
    } else if (identical(node_order, c("gate", "quantifiable", "linear"))) {
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]
      # filter quantifiable classification
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
      quantifiable_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[quantifiable_passed, ]

      # now classify by linear
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )
    } else if (identical(node_order, c("linear", "gate", "quantifiable"))) {
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )

      linear_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[linear_passed, ]

      # filter by gate
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]

      # filter and classify by quantifiable
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
    } else if (identical(node_order, c("linear", "quantifiable", "gate"))) {
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )

      linear_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[linear_passed, ]
      # filter and classify by quantifiable
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
      quantifiable_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[quantifiable_passed, ]

      joined_data$concentration_status <- ifelse(joined_data$gate_class %in% valid_gate_class, "Acceptable",
                                                 ifelse(joined_data$gate_class == "Above_Upper_Limit", "Too Concentrated",
                                                        ifelse(joined_data$gate_class == "Below_Lower_Limit", "Too Diluted", "Acceptable")))


    } else if (identical(node_order, c("quantifiable", "gate", "linear"))){
      # filter and classify by quantifiable
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
      quantifiable_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[quantifiable_passed, ]

      # filter by gate
      gate_class_passed <- joined_data$gate_class %in% valid_gate_class
      joined_data <- joined_data[gate_class_passed,]

      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )


    } else if (identical(node_order, c("quantifiable", "linear", "gate"))) {
      # filter and classify by quantifiable
      joined_data <- joined_data[!is.na(joined_data$lloq) & !is.na(joined_data$uloq), ]

      joined_data$concentration_status <- ifelse(
        joined_data$predicted_dilution_fraction < joined_data$lloq, "Too Diluted",
        ifelse(
          joined_data$predicted_dilution_fraction > joined_data$uloq, "Too Concentrated",
          "Acceptable"  # explicitly assign Acceptable for quantifiable
        )
      )
      quantifiable_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[quantifiable_passed, ]

      # Linear filter
      joined_data <- joined_data[!is.na(joined_data$bendlower) & !is.na(joined_data$bendupper), ]

      joined_data$concentration_status <- ifelse(
        joined_data$antibody_mfi < joined_data$bendlower, "Too Diluted",
        ifelse(
          joined_data$antibody_mfi > joined_data$bendupper, "Too Concentrated",
          "Acceptable"  # mark as acceptable linear range for next step
        )
      )

      linear_passed <- joined_data$concentration_status == "Acceptable"
      joined_data <- joined_data[linear_passed, ]

      # classify by gate
      joined_data$concentration_status <- ifelse(joined_data$gate_class %in% valid_gate_class, "Acceptable",
                                                 ifelse(joined_data$gate_class == "Above_Upper_Limit", "Too Concentrated",
                                                        ifelse(joined_data$gate_class == "Below_Lower_Limit", "Too Diluted", "Acceptable")))



    }



  }
  ## Classification based on tree (node order)
  joined_data$Classification <- ifelse(joined_data$concentration_status == "Acceptable", "Pass Classification", "Does Not Pass Classification")

  return(joined_data)
}

# produce first contingency summary
produce_contigency_summary <- function(joined_data) {
  #joined_data_v <<- joined_data
  contingency_df <- as.data.frame(table(joined_data$dilution, joined_data$experiment_accession, joined_data$antigen, joined_data$concentration_status))

  names(contingency_df) <- c("dilution", "experiment_accession", "antigen", "concentration_status", "count")


  contigency_wide <- contingency_df %>%
    tidyr::pivot_wider(
      names_from = dilution,
      values_from = count,
      values_fill = 0
    )


  contigency_wide <- contigency_wide[, c("antigen", setdiff(names(contigency_wide), "antigen"))]

  contigency_wide <- contigency_wide[order(contigency_wide$antigen,
                                           contigency_wide$experiment_accession,
                                           contigency_wide$concentration_status), ]

  return(contigency_wide)
}

fetch_antigen_family_df <- function(study_accession, project_id) {
  query <- paste0("SELECT xmap_antigen_family_id, study_accession, antigen, antigen_family
  	FROM madi_results.xmap_antigen_family
  	WHERE project_id = ",project_id,"
    AND study_accession IN ('",study_accession,"');")
  antigen_family_df <- dbGetQuery(conn, query)
  return(antigen_family_df)
}

## Plot summary barplot figure
summary_dilution_plot <- function(dilution_summary_df, antigen_families, antigen_family_order) {
  dil_col <- names(dilution_summary_df)[!names(dilution_summary_df) %in% c("antigen", "antigen_family", "experiment_accession", "concentration_status" ) ]

  df_long <- dilution_summary_df %>% pivot_longer(cols = all_of(dil_col),
                                                  names_to = "dilution",
                                                  values_to = "count")
  df_long$dilution <- as.numeric(as.character(df_long$dilution))

  # add proportion
  df_long$proportion <- with(df_long, count / ave(count, antigen, dilution, FUN = sum))


  # ensure 1 row/antigen
  antigen_families <-antigen_families %>%
    distinct(antigen, .keep_all = TRUE)

  df_prop <- merge(df_long, antigen_families[, c("antigen", "antigen_family")], by = "antigen", all.x = T)

  # Get unique dilutions
  dilutions <- unique(df_prop$dilution)

  df_prop$concentration_status <- factor(df_prop$concentration_status,
                                         levels = c("Acceptable","Too Diluted", "Too Concentrated"))


  concentration_colors <- c(
    "Too Diluted" = "#d46a6a",   # softer muted red
    "Acceptable" = "#6699cc",    # softer muted blue
    "Too Concentrated" = "#e6b800"  # softer golden yellow
  )

  df_prop$antigen_family <- factor(
    df_prop$antigen_family,
    levels = antigen_family_order)

  #antigen_family_order <-  c("H1N1", "H3N2", "B/Yamagata", "B/Victoria", "Pertussis", "TT", "CMV", "SARS-CoV-2", "Polio")
  family_rank <- setNames(seq_along(antigen_family_order), antigen_family_order)
  df_prop$family_rank <- family_rank[df_prop$antigen_family]

  df_prop <- df_prop[order(df_prop$family_rank, df_prop$antigen), ]
  antigen_levels <- unique(df_prop$antigen)

  df_prop$antigen <- factor(df_prop$antigen, levels = rev(antigen_levels))

  format_dilution <- function(d) paste0("1:", d)

  # Create a plot for each dilution
  plots <- lapply(seq_along(dilutions), function(i) {
    dil <- dilutions[i]
    subset_data <- df_prop[df_prop$dilution == dil, ]
    show_leg <- i == 1

    #subset_data$legend_label <- paste0(subset_data$concentration_status, "- Dilution: ", dil)

    plot_ly() %>%
      add_bars(data = subset_data[subset_data$concentration_status == "Acceptable", ],
               x = ~proportion, y = ~antigen,
               color = I(concentration_colors["Acceptable"]),
               name = paste("Acceptable"),#<br> Dilution:", dil ),
               legendgroup = "Acceptable",
               showlegend = show_leg
               )%>%
      add_bars(data = subset_data[subset_data$concentration_status == "Too Diluted", ],
               x = ~proportion, y = ~antigen,
               color = I(concentration_colors["Too Diluted"]),
               name = paste("Too Diluted"), # <br> Dilution:", dil),
               legendgroup = "Too Diluted",
               showlegend = show_leg) %>%
      add_bars(data = subset_data[subset_data$concentration_status == "Too Concentrated", ],
               x = ~proportion, y = ~antigen,
               color = I(concentration_colors["Too Concentrated"]),
               name = paste("Too Concentrated"), #"<br> Dilution:", dil),
               legendgroup = "Too Concentrated",
               showlegend = show_leg) %>%
      layout(title = paste0(unique(df_prop$experiment_accession), ": Proportion of Subjects by Antigen, Sample Dilution, and Concentration Status"),
             xaxis = list(title = format_dilution(dil), tickangle = 45, tickformat = ".1f"),
             barmode = "stack")
  })

  # Arrange the plots side by side
  combined_plot <- subplot(plots, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
                            layout(
                              margin = list(b = 90),
                              legend = list(title = list(text = "Concentration Status")),
                              yaxis = list(
                                title = "Antigen",
                                tickmode = "array",
                                tickvals = antigen_levels,
                                ticktext = antigen_levels
                              ),
                              annotations = list(
                                list(
                                  text = "Sample Dilution",
                                  x = 0.5,
                                  y = -0.24,
                                  xref = "paper",
                                  yref = "paper",
                                  showarrow = FALSE,
                                  xanchor = "center",
                                  yanchor = "top",
                                  font = list(size = 14)
                                )
                              )
                            )


  return(combined_plot)

}

#Add classification of the sample data AUs. with selected values and passing dilutions
classify_sample_data <- function(sample_data, selectedAntigen, selectedPatient = NULL,selectedTimepoints = NULL, selectedDilutions = NULL, passing_dilution_df) {

  sample_data$dilution_fraction <- 1 / sample_data$dilution

  sample_data <- sample_data[sample_data$antigen %in% selectedAntigen, ]
  if (!is.null(selectedPatient)){
    sample_data <- sample_data[sample_data$patientid %in% selectedPatient,]
  }
  if (!is.null(selectedTimepoints)) {
    sample_data <- sample_data[sample_data$timeperiod %in% selectedTimepoints,]
  }
  if (!is.null(selectedDilutions)) {
    sample_data <- sample_data[sample_data$dilution %in% selectedDilutions,]
  }

 # merge by patient, timeperiod, antigen, and dilution
  classified_data <- merge(sample_data, passing_dilution_df, by = c("patientid", "timeperiod", "antigen", "dilution"), all.x = T)

  #sample_data_view_class <<- sample_data

  classified_data$Classification <- ifelse(is.na(classified_data$n_pass_dilutions), "Does Not Pass Classification", "Pass Classification")

  classified_sample_dilution_linearity_rv(classified_data)

}


plot_patient_dilution_series  <- function(sample_data, selectedAntigen, selectedPatient,selectedTimepoints, selectedDilutions) {


  sample_data <- sample_data[sample_data$antigen %in% selectedAntigen, ]
  sample_data <- sample_data[sample_data$patientid %in% selectedPatient,]
  sample_data <- sample_data[sample_data$timeperiod %in% selectedTimepoints,]
  sample_data <- sample_data[sample_data$dilution %in% selectedDilutions,]

  sample_data$dilution_fraction <- 1 / sample_data$dilution

  sample_data <- sample_data[order(sample_data$dilution), ]
  
 

  #sample_data_v <<- sample_data
  # antigens <- unique(sample_data$antigen)
  # timeperiods <- unique(sample_data$timeperiod)
  # patientids <- unique(sample_data$patientid)
  # fig <- plot_ly()

  #selectedTimepoints <<- selectedTimepoints
  line_types_visit <- c("solid", "dash", "dot", "dashdot")
  line_types_assigned <- line_types_visit[1:length(selectedTimepoints)]  # Use only the first n timepoint line types
  named_line_types <- setNames(line_types_assigned, selectedTimepoints)

  # symbol map for all possible gate classes
  gate_class_symbol_map <- c("Between_Limits" = "circle", "Below_Lower_Limit" = "diamond", "Above_Upper_Limit" = "square")

  # # keep track of the maximum au
  # max_au <- max(sample_data$au)
  # pad <- 0.05 * max_au   # 5% padding

  sample_data <- sample_data %>% group_by(patientid, timeperiod)
  
  #sample_data_v <<- sample_data
  # sample_data$class_flag <- ifelse(
  #   trimws(tolower(sample_data$Classification)) == "pass classification",
  #   TRUE, FALSE
  # )
  # sample_data$class_flag <- factor(sample_data$class_flag, levels = c(FALSE, TRUE),
  #                                  labels = c("Does Not Pass", "Pass"))

  # named_colors <- c("Pass Classification" = "#0067a5", "Does Not Pass Classification" = "#be0032")
  # sample_data$color <- named_colors[sample_data$Classification]

  assay_response_variable <- format_assay_terms(unique(sample_data$assay_response_variable))
  assay_independent_variable <- format_assay_terms(unique(sample_data$assay_independent_variable))

  # format in dataset
  sample_data$assay_response_variable <- format_assay_terms(sample_data$assay_response_variable)
  sample_data$assay_independent_variable <- format_assay_terms(sample_data$assay_independent_variable)

  if (unique(sample_data$is_log_x)) {
    is_log_concentration_text <- "Log <sub>10</sub>"
  } else {
    is_log_concentration_text <- ""
  }


  linetype <- named_line_types[sample_data$timeperiod]

  iq_text <- if ("pass_limits_of_quantification" %in% names(sample_data)) {
    paste0("<br>In Quantifiable Region: ", sample_data$pass_limits_of_quantification)
  } else {
    paste0("<br> Quantifiable Region Gate Class: ", sample_data$gate_class_loq)
  }


  fig <- plot_ly()

  fig <- fig %>%

    add_trace(
      data = sample_data,
      x = ~jitter(dilution_fraction),
      y = ~au,
      type = 'scatter',
      mode = 'lines+markers',  # Lines and markers for connecting patient id
      # group = ~patientid,
      # marker = list(color = ~class_flag),#list(color = ~I(color)),#list(color = named_colors[sample_data$Classification]), # Assign in linear region color per point
      color = ~concentration_status,
      colors = c(
        "Too Diluted" = "#d46a6a",
        "Acceptable" = "#6699cc",
        "Too Concentrated" = "#e6b800",
        "Too Concentrated_Too Diluted" = "#dd8f35"
      ), #c("Does Not Pass" = "#be0032", "Pass" = "#0067a5"),#symbol = ~marker_symbol),  # symbol is gate class
      line = list(color = "grey", dash = linetype),    # Assign grey color for a line and line type is the timeperiod. named_line_types[timeperiod]
      hoverinfo = 'text',
      text = ~paste0("Subject: ", patientid, "<br>Antigen: ",antigen, "<br>Visit: ",timeperiod,
                     "<br>Dilution Factor: ", dilution,
                     "<br> Dilution Fraction: ",dilution_fraction,
                     "<br>",assay_independent_variable,": ", raw_predicted_concentration,
                     "<br>AU: ", au,
                     "<br>",assay_response_variable, ": ", assay_response,
                     iq_text,
                     "<br> LOD Gate Class: ", gate_class_lod,
                     "<br> Concentration Status: ", concentration_status)# as.character(class_flag))
    )




  fig <- fig %>%
    layout(
      title = paste("Sample Concentrations Predicted from Plate Dilution Series"),
      xaxis = list(title = "Plate Dilution Fraction", type = "log"),
      yaxis = list(title = "Sample Concentration", #range = c(1, max_au + pad),
                   type = "log"),

      showlegend = TRUE,
      legend = list(title = list(text = "Concentration Status")),
      annotations = list(

        list(
          x = 0.5,
          y = -0.30,
          xref = "paper",
          yref = "paper",
          text = paste(
            "The highest concentration values correspond to samples with", toupper(assay_response_variable), "values above the upper asymptote of the standard curve."
          ),
          showarrow = FALSE,
          font = list(size = 12),
          align = "center"
        )
      ),

      margin = list(b = 90)
    )

  fig
}


# plot_dilutional_linearity <- function(sample_data, selectedAntigen, selectedPatient,selectedTimepoints, selectedDilutions) {
#
#     sample_data_plot <<- sample_data
#    #
#    # selected_antigen_plot <<- selectedAntigen
#    # selected_patient_plot <<- selectedPatient
#    # selected_timepoints_plot <<- selectedTimepoints
#    # selectedDilutions_plot <<- selectedDilutions
#
#   sample_data <- sample_data[sample_data$antigen %in% selectedAntigen, ]
#   sample_data <- sample_data[sample_data$patientid %in% selectedPatient,]
#   sample_data <- sample_data[sample_data$timeperiod %in% selectedTimepoints,]
#   sample_data <- sample_data[sample_data$dilution %in% selectedDilutions,]
#   #sample_data <- sample_data[sample_data$status %in% selected_status,]
#   #sample_data <- sample_data[sample_data$n_pass_dilutions ==selected_n_dilutions, ]
#
#   sample_data$dilution_fraction <- 1 / sample_data$dilution
#
#   sample_data <- sample_data[order(sample_data$dilution), ]
#
#   antigens <- unique(sample_data$antigen)
#   timeperiods <- unique(sample_data$timeperiod)
#   patientids <- unique(sample_data$patientid)
#   fig <- plot_ly()
#
#   selectedTimepoints <- selectedTimepoints
#   line_types_visit <- c("solid", "dash", "dot", "dashdot")
#   line_types_assigned <- line_types_visit[1:length(selectedTimepoints)]  # Use only the first n timepoint line types
#   named_line_types <- setNames(line_types_assigned, selectedTimepoints)
#
#   # symbol map for all possible gate classes
#   gate_class_symbol_map <- c("Between_Limits" = "circle", "Below_Lower_Limit" = "diamond", "Above_Upper_Limit" = "square")
#
#   # keep track of the maximum au
#   max_au_vector <- c()
#
#   for (antigen in antigens) {
#     for (patient in patientids) {
#       for (i in seq_along(selectedTimepoints)) {
#         timeperiod <- selectedTimepoints[i]
#
#         filtered_data <- sample_data[sample_data$antigen == antigen & sample_data$patientid == patient & sample_data$timeperiod == timeperiod,]
#
#         filtered_data$marker_symbol <-  gate_class_symbol_map[as.character(filtered_data$gate_class)]
#         # Assign colors on if it passes or not.
#         #colors <- ifelse(filtered_data$dilution %in% unique(passing_dilution_df$dilution), "#0067a5", "#be0032")
#         colors <- ifelse(filtered_data$Classification == "Pass Classification", "#0067a5", "#be0032")
#
#         linetype <- line_types_assigned[i]
#
#
#         # append current max au to vector
#         max_au_vector <- c(max_au_vector, max(filtered_data$au))
#         fig <- fig %>%
#           add_trace(
#             data = filtered_data,
#             x = ~dilution_fraction,
#             y = ~au,
#             type = 'scatter',
#             mode = 'lines+markers',  # Lines and markers for connecting patient id
#             marker = list(color = colors), # Assign in linear region color per point
#             #symbol = ~marker_symbol),  # symbol is gate class
#             line = list(color = "grey", dash = linetype),    # Assign grey color for a line and line type is the timeperiod. named_line_types[timeperiod]
#             hoverinfo = 'text',
#             text = ~paste("Subject: ",patient,"<br>Antigen: ",antigen, "<br>Visit: ",timeperiod, "<br>Dilution Factor:", dilution, "<br> Dilution Fraction:",dilution_fraction,
#                           "<br>AU:", au,
#                           "<br>In Linear Region:", in_linear_region,
#                           "<br> LOD Gate Class:", gate_class,
#                           "<br> Pass Classification:", ifelse(Classification == "Pass Classification", "TRUE", "FALSE")),
#             name = paste("Antigen:", filtered_data$antigen, "Subject:", patient, "Visit:", timeperiod)
#           )
#       }
#     }
#   }
#
#
#   fig <- fig %>%
#     layout(
#       title = "Plate Dilution Series",
#       xaxis = list(title = "Log <sub>10</sub> Plate Dilution", type = "log"),
#       yaxis = list(title = "Antibody AU", range = c(0, max(max_au_vector) + 10)),
#
#       showlegend = TRUE,
#       annotations = list(
#
#         list(
#           x = 0.5,
#           y = -0.8,
#           xref = "paper",
#           yref = "paper",
#           text = paste(
#             "Color represents Passing Classification (blue) or not passing Classification (red)"
#           ),
#           showarrow = FALSE,
#           font = list(size = 12),
#           align = "center"
#         )
#       ),
#
#       margin = list(b = 200)
#     )
#
#   fig
# }

# download sample data for linearity
download_processed_dilution_data <- function(download_df, selected_study, selected_experiment) {
 # download_df <- download_df[download_df$antigen %in% selected_antigen,]
  # if (length(selected_antigen) > 1) {
  #   antigen_string <- paste(selected_antigen, collapse = "_")
  # } else {
  #   antigen_string <- selected_antigen
  # }

  # if (length(selected_antigen) > 1) {
  #   antigen_string_label <- paste(selected_antigen, collapse = ", ")
  # } else {
  #   antigen_string_label <- selected_antigen
  # }

  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study, "_", selected_experiment, "_", "_sample_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Processed Dilution Data: ",selected_experiment, "-",selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

### Decision Tree Classification
## Truth Table

create_truth_table <- function(binary_gate, exclude_linear = FALSE, exclude_quantifiable = FALSE, exclude_gate = FALSE) {
  if (exclude_linear & exclude_quantifiable & exclude_gate) {
    return(NULL)
  }
  if (!exclude_gate) {
    gate_class <- c("Below_Lower_Limit_of_Detection", "Between_Limits_of_Detection", "Above_Upper_Limit_of_Detection")

    if (binary_gate) {
      gate_class <- c(TRUE, FALSE)
    }

    if (!binary_gate & !exclude_gate) {
      cat("Both false")
      in_linear_region <- c(TRUE, FALSE)
      in_quantifiable_range <- c(TRUE, FALSE)

      # Create all combinations using expand.grid()
      truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                 In_Quantifiable_Range = in_quantifiable_range,
                                 Gate_Class = gate_class,
                                 stringsAsFactors = FALSE)
      truth_table$Classification <- F

      return(truth_table)

    } else if (exclude_quantifiable & exclude_linear) {
      truth_table <- data.frame(Gate_Class = gate_class)
      return(truth_table)
    } else if (exclude_linear) {
      in_quantifiable_range <- c(TRUE, FALSE)
      truth_table <- expand.grid(In_Quantifiable_Range = in_quantifiable_range,
                                 Gate_Class = gate_class,
                                 stringsAsFactors = FALSE)
      if (binary_gate) {
        truth_table$Classification <- with(truth_table, In_Quantifiable_Range & Gate_Class)
      }

      return(truth_table)
    } else if (exclude_quantifiable) {
      in_linear_region <- c(TRUE, FALSE)
      # Create all combinations using expand.grid()
      truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                 Gate_Class = gate_class,
                                 stringsAsFactors = FALSE)
      if (binary_gate) {
        truth_table$Classification <- with(truth_table, In_Linear_Region & Gate_Class)
      }
      return(truth_table)

    } else {

      if (exclude_gate) {
        in_linear_region <- c(TRUE, FALSE)
        in_quantifiable_range <- c(TRUE, FALSE)

        # Create all combinations using expand.grid()
        truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                   In_Quantifiable_Range = in_quantifiable_range,

                                   stringsAsFactors = FALSE)

        truth_table$Classification <- with(truth_table, In_Linear_Region & In_Quantifiable_Range)
        return(truth_table)
      } else if (exclude_gate & exclude_linear) {
        in_quantifiable_range <- c(TRUE, FALSE)

        # Create all combinations using expand.grid()
        truth_table <- expand.grid(In_Quantifiable_Range = in_quantifiable_range,
                                   stringsAsFactors = FALSE)

        truth_table$Classification <- with(truth_table, In_Quantifiable_Range)
        return(truth_table)
      } else if (exclude_gate & exclude_quantifiable) {
        in_linear_region <- c(TRUE, FALSE)
        truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                   stringsAsFactors = FALSE)

        truth_table$Classification <- with(truth_table,In_Linear_Region)
        return(truth_table)
      } else {
        in_linear_region <- c(TRUE, FALSE)
        in_quantifiable_range <- c(TRUE, FALSE)

        # Create all combinations using expand.grid()
        truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                   In_Quantifiable_Range = in_quantifiable_range,
                                   Gate_Class = gate_class,
                                   stringsAsFactors = FALSE)

        if (binary_gate) {
          truth_table$Classification <- with(truth_table, In_Linear_Region & (In_Quantifiable_Range & Gate_Class))
        }

        return(truth_table)
      }
    }
  }

  else { # exclude gate class
    # cat("excluding gc")
    if (exclude_linear) {
      in_quantifiable_range <- c(TRUE, FALSE)
      truth_table <- expand.grid(In_Quantifiable_Range = in_quantifiable_range,
                                 stringsAsFactors = FALSE)
      #if (binary_gate) {
      truth_table$Classification <- truth_table$In_Quantifiable_Range
      #}
      return(truth_table)
    } else if (exclude_quantifiable) {
      in_linear_region <- c(TRUE, FALSE)
      truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                 stringsAsFactors = FALSE)
      # if (binary_gate) {
      truth_table$Classification <- truth_table$In_Linear_Region
      #}
      return(truth_table)
    }
    # both exclusion are false when gc is excluded
    else if (!(exclude_linear & exclude_quantifiable)) {
      in_linear_region <- c(TRUE, FALSE)
      in_quantifiable_range <- c(TRUE, FALSE)
      truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                 In_Quantifiable_Range = in_quantifiable_range,
                                 stringsAsFactors = FALSE)
      truth_table$Classification <- with(truth_table, In_Linear_Region & In_Quantifiable_Range)
      return(truth_table)
    }  else {
      in_linear_region <- c(TRUE, FALSE)
      in_quantifiable_range <- c(TRUE, FALSE)

      # Create all combinations using expand.grid()
      truth_table <- expand.grid(In_Linear_Region = in_linear_region,
                                 In_Quantifiable_Range = in_quantifiable_range,
                                 stringsAsFactors = FALSE)
      if (binary_gate) {
        truth_table$Classification <- with(truth_table, In_Linear_Region & In_Quantifiable_Range)
      }
      return(truth_table)
    }

  }
}

## Helper function to get leaf nodes of the decision tree
get_leaf_nodes <- function(node) {
  if (node$isLeaf) {
    return(node)  # Return the leaf node's name as a list
  }

  leaf_nodes <- list()  # Initialize an empty list to store leaf nodes

  for (child in node$children) {
    leaf_nodes <- c(leaf_nodes, get_leaf_nodes(child))  # Recursively call for child nodes
  }

  return(leaf_nodes)  # Return all the leaf nodes found
}

## Produce the Decision Tree based on truth table
node_to_tree_col <- function(node) {
  if (node == "limits_of_quantification") {
    return("quantifiable")
  } else if (node == "limits_of_detection")
    return("gate")
}

create_decision_tree_tt <- function(truth_table, binary_gate, sufficient_gc_vector, node_order = c("gate", "linear", "quantifiable")) {

  if (length(node_order) == 0) {
    return(NULL)
  }
  decision_tree <- Node$new("Dilution")  # Root node

  # Define the order of checks based on the node_order parameter
  if (!"linear" %in% node_order) {
    exclude_linear <- TRUE
  } else {
    exclude_linear <- FALSE
  }

  if (!"quantifiable" %in% node_order) {
    exclude_quantifiable <- TRUE
  } else {
    exclude_quantifiable <- FALSE
  }
  if (!"gate" %in% node_order) {
    exclude_gate <- TRUE
  } else {
    exclude_gate <- FALSE
  }

  for (i in seq_len(nrow(truth_table))) {
    row <- truth_table[i, ]
    current_node <- decision_tree
    next_node <- T
    # Iterate through the decision nodes in the specified order
    for (node in node_order) {
      if(!next_node) {
        break
      }
      if (node == "gate" && !exclude_gate) {
        gc_row <- row$Gate_Class
        if (binary_gate && ("gate" %in% node_order) && length(node_order)  == 1) {
          cat("Only gate and binary gate")
          true_gate_node_name <- "In Detection Region: TRUE" # Gate Class
          if (!true_gate_node_name %in% names(current_node$children)) {
            true_gate_node <- current_node$AddChild(true_gate_node_name)
            true_gate_node$condition <- function(x) x$sufficient_gc == TRUE
          }
          false_gate_node_name <- "In Detection Region: FALSE"
          if (!false_gate_node_name %in% names(current_node$children)) {
            false_gate_node <- current_node$AddChild(false_gate_node_name)
            false_gate_node$condition <- function(x) x$sufficient_gc == FALSE
          }
        } else if (binary_gate) {
          # Create TRUE gate node
          true_gate_node_name <- "In Detection Region: TRUE"
          if (!true_gate_node_name %in% names(current_node$children)) {
            true_gate_node <- current_node$AddChild(true_gate_node_name)
            true_gate_node$condition <- function(x) x$sufficient_gc == TRUE
          }
          # if gate is the only node listed do split on Dilution (root) node
          if (!(("gate" %in% node_order) && length(node_order)  == 1)){
            current_node <- current_node[[true_gate_node_name]]
          }

          # Create FALSE gate node
          false_gate_node_name <- "In Detection Region: FALSE"
          if (!false_gate_node_name %in% names(current_node$parent$children)) {
            false_gate_node <- current_node$parent$AddChild(false_gate_node_name)
            false_gate_node$condition <- function(x) x$sufficient_gc == FALSE
          }
        } else {
          # Handle non-binary case

         gc_row_clean <- gsub("_", " ", gc_row)

          if (gc_row %in% sufficient_gc_vector) {

            gate_node_name <- paste("Detection Region:", gc_row_clean, "TRUE")
          } else {
            gate_node_name <- paste("Detection Region:", gc_row_clean, "FALSE")
          }

          if (!gate_node_name %in% names(current_node$children)) {
            gate_node <- current_node$AddChild(gate_node_name)
            gate_node$condition <- function(x) x$gate_class %in% sufficient_gc_vector
          }

          current_node <- current_node[[gate_node_name]]
        }
        # if (binary_gate) {
        #   gate_node_name <- paste("Gate Class: TRUE")
        # } else {
        #   if (gc_row %in% sufficient_gc_vector) {
        #     gate_node_name <- paste("Gate Class:", gc_row, "TRUE")
        #   } else {
        #     gate_node_name <- paste("Gate Class:", gc_row, "FALSE")
        #   }
        # }
        #
        # if (!gate_node_name %in% names(current_node$children)) {
        #   gate_node <- current_node$AddChild(gate_node_name)
        #   if (binary_gate) {
        #     gate_node$condition <- function(x) x$sufficient_gc == T
        #   } else {
        #     gate_node$condition <- function(x) x$gate_class %in% sufficient_gc_vector
        #   }
        # }
        # current_node <- current_node[[gate_node_name]]
        #
        if (!binary_gate) {
          if (!(row$Gate_Class %in% sufficient_gc_vector)) {
            next_node <- FALSE
          }
        }
      }

      if (node == "linear" && !exclude_linear) {
        linear_node_name <- paste("Linear:", row$In_Linear_Region)
        if (!linear_node_name %in% names(current_node$children)) {
          linear_node <- current_node$AddChild(linear_node_name)
          linear_node$condition <- function(x) x$in_linear_region == row$In_Linear_Region
          #current_node <- linear_node
        }
        current_node <- current_node[[linear_node_name]]


        # if (!row$In_Linear_Region) next  # Stop if FALSE
        if (!row$In_Linear_Region) {
          next_node <- FALSE
        }
      }

      if (node == "quantifiable" && !exclude_quantifiable) {
        quantifiable_node_name <- paste("In Quantifiable Region:", row$In_Quantifiable_Range)
        if (!quantifiable_node_name %in% names(current_node$children)) {
          quantifiable_node <- current_node$AddChild(quantifiable_node_name)
          quantifiable_node$condition <- function(x) x$in_quantifiable_range == row$In_Quantifiable_Range
          # current_node <- quantifiable_node
        }
        current_node <- current_node[[quantifiable_node_name]]

        # if (!row$In_Quantifiable_Range) #next  # Stop if FALSE
        if (!row$In_Quantifiable_Range) {
          next_node <- FALSE
        }
      }
    }
  }

  leaf_nodes <- get_leaf_nodes(decision_tree)
  for (i in 1:length(leaf_nodes)) {
    if (grepl("TRUE", leaf_nodes[[i]]$name, ignore.case = TRUE)) {
      print("T")
      leaf_nodes[[i]]$AddChild("Pass Classification")
    } else {
      print("F")
      leaf_nodes[[i]]$AddChild("Does not Pass Classification")
    }
  }

  return(decision_tree)
}


### Obtaining info and classification of the tree
# Leaf Paths from root to leaf
get_leaf_path <- function(node, path = "") {
  # Check if the current node is a leaf node
  if (node$isLeaf) {
    # Check if the leaf node's name contains "TRUE" (case-insensitive)
    if (node$name == "Pass Classification") {
      # Return the path of the leaf node
      return(path)  # Return the path as a string
    } else {
      return(character(0))  # Return an empty character vector if no match
    }
  }

  leaf_paths <- character(0)  # Initialize an empty vector to store paths of leaf nodes

  # Iterate over each child of the current node
  for (child in node$children) {
    # Recursively call the function for each child and update the path
    new_path <- paste(path, child$name, sep = "/")  # Update the path
    leaf_paths <- c(leaf_paths, get_leaf_path(child, new_path))  # Append path to the list
  }

  return(leaf_paths)  # Return all paths of leaf nodes that contain "TRUE"
}

## Parse the leaf path
#  call parse string Make sure binary is correct
# parsed_data <- do.call(rbind, lapply(paths, parse_leaf_path, binary_gc = T, sufficient_gc_vector = c("Between_Limits", "Below_Lower_Limit") ))
# parsed_data

parse_leaf_path <- function(s, binary_gc, sufficient_gc_vector) {
  # Use regexpr and regmatches to check for existence and extract values
  #gate_class <- ifelse(grepl("Gate Class", s), sub(".*Gate Class: ([^/]+).*", "\\1", s), NA)
  if (binary_gc) {
    df <- data.frame(in_quantifiable_range = NA, gate_class = NA, in_linear_region = NA, Classification = NA)
    for (gc in sufficient_gc_vector) {
      quantifiable <- ifelse(
        grepl("In Quantifiable Region", s),
        sub(".*In Quantifiable Region: (TRUE|FALSE).*", "\\1", s),
        NA
      )

     # quantifiable <- ifelse(grepl("Quantifiable", s), sub(".*Quantifiable: (TRUE|FALSE).*", "\\1", s), NA)
      gate_class <- TRUE
      linear <- ifelse(grepl("Linear", s), sub(".*Linear: (TRUE|FALSE).*", "\\1", s), NA)
      classification <- ifelse(grepl("/", s), sub(".*/(.*)", "\\1", s), NA)
      df <- rbind(df, data.frame(in_quantifiable_range = quantifiable, gate_class = gc, in_linear_region = linear, Classification = classification))
    }
    df <- df[-1, ]
    # return(df)
  } else {
    quantifiable <- ifelse(
      grepl("In Quantifiable Region", s),
      sub(".*In Quantifiable Region: (TRUE|FALSE).*", "\\1", s),
      NA
    )

   # quantifiable <- ifelse(grepl("Quantifiable", s), sub(".*Quantifiable: (TRUE|FALSE).*", "\\1", s), NA)
    gate_class <- ifelse(grepl("Gate Class", s), sub(".*Gate Class: ([^/]+)(?: TRUE|$).*", "\\1", s), NA)
    linear <- ifelse(grepl("Linear", s), sub(".*Linear: (TRUE|FALSE).*", "\\1", s), NA)
    classification <- ifelse(grepl("/", s), sub(".*/(.*)", "\\1", s), NA)

    df <- data.frame(in_quantifiable_range = quantifiable, gate_class = gate_class, in_linear_region = linear, Classification = classification)
  }
  # remove columns that are excluded from decision tree.
  df <- df[, colSums(is.na(df)) < nrow(df)]
  return(df)
}

## Classify Sample
#call: BK_classified <- classify_sample(BK_study_sample, parsed_classification = parsed_data)
classify_sample <- function(sample_data, parsed_classification) {
 # parsed_classification_view <<- parsed_classification
 # sample_data_view <<- sample_data
  if (nrow(parsed_classification) == 0 || is.null(parsed_classification)) {
    sample_data$Classification <- "Does Not Pass Classification"
    return(sample_data)
  } else {
    common_columns <- intersect(names(sample_data), names(parsed_classification))
    new_df <- merge(sample_data, parsed_classification, by = common_columns, all.x = TRUE)
    # add string for not passing classification
    new_df$Classification[is.na(new_df$Classification)] <- "Does Not Pass Classification"
    return(new_df)
  }
}


get_edges <- function(node) {
  edges <- c()
  if (!is.null(node$children)) {
    for (child in node$children) {
      edges <- c(edges, paste0("\"", node$name, "\" -> \"", child$name, "\""))
      edges <- c(edges, get_edges(child))
    }
  }
  return(edges)
}


# download_classified data
download_classified_sample <- function(download_df, selected_study, selected_experiment) {

  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study,"_", selected_experiment, "_", "_classified_sample_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download Classified Sample Data: ", selected_experiment, "-", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}


download_dilution_contigency_summary_fun <- function(download_df, selected_study, selected_experiment) {

  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_study,"_", selected_experiment, "_dilution_summary"),
    output_extension = ".xlsx",
    button_label = paste0("Download Dilution Summary ", selected_experiment, "-", selected_study),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}


### Heatmap of classifications passing decision tree
plot_classification_heatmap <- function(classified_sample, selectedAntigens, selectedDilutions) {

  # Subset the data
  heatmap_data <- classified_sample[classified_sample$antigen %in% selectedAntigens &
                                      classified_sample$dilution %in% selectedDilutions &
                                      classified_sample$Classification == "Pass Classification", ]

  # Create a frequency table for patient counts grouped by antigen, timeperiod, and patientid and get distinct values
  heatmap_data <- heatmap_data[!duplicated(heatmap_data[c("patientid", "timeperiod", "antigen", "dilution")]), ]
  heatmap_table <- xtabs(~ antigen + timeperiod + patientid , data = heatmap_data)

  # Convert to data frame while keeping all factors intact
  heatmap_data <- as.data.frame(heatmap_table)


  # Reshape to wide format where each patientid becomes a column
  heatmap_data <- reshape(heatmap_data,
                          idvar = c("antigen", "timeperiod"),
                          timevar = "patientid",
                          direction = "wide")

  colnames(heatmap_data) <- gsub("Freq\\.", "", colnames(heatmap_data))
  # sort antigens
  heatmap_data <- heatmap_data[order(heatmap_data$antigen), ]

  heatmap_data[is.na(heatmap_data)] <- 0


  heatmap_matrix <- as.matrix(heatmap_data[ -c(1,2)])
  rownames(heatmap_matrix) <- paste(heatmap_data$timeperiod, heatmap_data$antigen)



  classified_heatmap <-  heatmaply(t(heatmap_matrix),
                                   xlab = "Timeperiod and Antigen",
                                   ylab = "Subject ID",
                                   main = "Dilutions Passing Classification by Subject, Timeperiod, and Antigen",
                                   dendrogram = "row",
                                   label_names = c("Subject","Timeperiod and Antigen", "Number of passing dilutions")
                                   #colorbar = list(tickmode = "array", tickvals = c(0, 1, 2,3, 4)),
                                   #limits = c(0, 4)


                                   #     custom_hovertext =  hover_text_matrix
  )

  return(classified_heatmap)
}


# obtain a contingency table for the passed classification
obtain_passing_subject_contigency <- function(classified_sample, selectedAntigens) {
  #classified_sample_contigency <<- classified_sample
  classified_sample_antigen <- classified_sample[classified_sample$antigen %in% selectedAntigens,]

  classified_by_timeperiod <- as.data.frame(table(classified_sample_antigen$patientid, classified_sample_antigen$Classification, classified_sample_antigen$timeperiod))


  margin_table <- addmargins(table(classified_by_timeperiod$Freq, classified_by_timeperiod$Var3, classified_by_timeperiod$Var2))


  margin_table <- margin_table[, , "Pass Classification"]


  margin_table <- as.data.frame.matrix(margin_table)
  # get rid of row sum (Sum as a column)
  margin_table <- margin_table[, !colnames(margin_table) %in% "Sum"]
  # add number of passing dilution column to beginning of dataset
  margin_table$`Number of Passing Dilutions` <- rownames(margin_table)
  margin_table <- margin_table[, c("Number of Passing Dilutions", setdiff(names(margin_table), "Number of Passing Dilutions"))]

  rownames(margin_table) <- NULL

  return(margin_table)
}


obtain_contigency_table <- function(classified_sample, selectedAntigens) {
  classified_merged <- compute_concentration_status(classified_sample, selectedAntigens = selectedAntigens)

  classified_merged$n_dilution_status <- paste(classified_merged$n_pass_dilutions, classified_merged$status, sep = "_")

  classified_merged$au_treatment <- ifelse(classified_merged$n_pass_dilutions == 0,
                                             "all_au",
                                             "geom_passing_au")


    classified_merged$n_dilution_status_treatment <- paste(classified_merged$n_dilution_status, classified_merged$au_treatment, sep = "_")


    # Save data frame to make contingency table in a reactive Value.
    classified_merged_rv(classified_merged)

}

create_margin_table <- function(classified_merged) {
 # cl_m <<- classified_merged
  classified_merged_1 <- classified_merged[, c("patientid", "n_pass_dilutions", "status", "timeperiod")]

  test_tab_1_0 <- as.data.frame(table(classified_merged_1$n_pass_dilutions,
                                  classified_merged_1$status,
                                  classified_merged_1$timeperiod))

  test_tab1 <- test_tab_1_0[(test_tab_1_0$Var1 == "0" & test_tab_1_0$Var2 != "Acceptable") |
                              (test_tab_1_0$Var1 != "0" & test_tab_1_0$Var2 == "Acceptable"),]

  transpose_tab_1 <- pivot_wider(data=test_tab1,
                               id_cols = c("Var1", "Var2"),
                               names_from = "Var3",
                               values_from = "Freq")

 # timepoints <- setdiff(names(transpose_tab_1), c("Var1", "Var2"))
  timepoints <- names(transpose_tab_1)[!names(transpose_tab_1) %in% c("Var1", "Var2")]


  classified_merged_2 <- classified_merged[, c("patientid", "n_pass_dilutions", "status", "au_treatment", "timeperiod")]

   test_tab_2_0 <- as.data.frame(table(classified_merged_2$n_pass_dilutions,
                                  classified_merged_2$status,
                                  classified_merged_2$timeperiod,
                                  classified_merged_2$au_treatment))
  test_tab2 <- test_tab_2_0[(test_tab_2_0$Var1 == "0" & test_tab_2_0$Var2 != "Acceptable") | (test_tab_2_0$Var1 != "0" & test_tab_2_0$Var2 == "Acceptable"),]
  test_tab2_2 <- test_tab2[test_tab2$Freq > 0, c("Var1", "Var2", "Var3", "Var4")]
  test_tab2_2$Var3 <- paste(test_tab2_2$Var3, "color", sep = "_")


  transpose_tab_2 <- pivot_wider(data=test_tab2_2,
                               id_cols = c("Var1", "Var2"),
                               names_from = "Var3",
                               values_from = "Var4")

  #timepoint_colors <- setdiff(names(transpose_tab_2), c("Var1", "Var2"))
 # timepoint_colors <- names(transpose_tab_2)[!names(transpose_tab_2) %in% c("Var1", "Var2")]
  timepoint_colors <- paste0(timepoints, "_color") # in revision

  transpose_tab_3 <- merge(transpose_tab_1, transpose_tab_2, by = c("Var1", "Var2"))
  names(transpose_tab_3)[names(transpose_tab_3)=="Var1"] <- "Number of Passing Dilutions"
  names(transpose_tab_3)[names(transpose_tab_3)=="Var2"] <- "concentration_status"

  # enforce column order
  desired_col_order <- c("Number of Passing Dilutions", "concentration_status", timepoints, timepoint_colors)
  transpose_tab_3 <- transpose_tab_3[, desired_col_order]

  # order by number of passing dilutions
  transpose_tab_3 <- transpose_tab_3[order(transpose_tab_3$`Number of Passing Dilutions`), ]

  totals <- as.data.frame(colSums(transpose_tab_3[, timepoints]))
  totals <-  t(totals)
  rownames(totals) <- NULL

  totals <- as.data.frame(totals)
  new_cols <- setdiff(names(transpose_tab_3), timepoints)

  totals <- cbind(totals, setNames(as.list(rep("Sum", length(new_cols))), new_cols))

#  total_row <- totals[, c("Number of Passing Dilutions", "concentration_status", timepoints, timepoint_colors)]
  total_row <- totals[, desired_col_order]
  margin_table <- rbind(transpose_tab_3, total_row)

  i <- sapply(margin_table, is.factor)
  margin_table[i] <- lapply(margin_table[i], as.character)

  #margin_table <-margin_table %>% mutate_at(timepoint_colors, ~as.character)

  margin_table <-margin_table %>% mutate_at(timepoint_colors, ~replace_na(.,"Blank"))
  return(margin_table)

}

## Add Concentration status
compute_concentration_status <- function(classified_sample, selectedAntigens) {

  classified_sample <- classified_sample[classified_sample$antigen %in% selectedAntigens,]


    classified_sample$midpoint_mfi <-
      ifelse(classified_sample$bendlower < classified_sample$bendupper,
             ((classified_sample$bendupper - classified_sample$bendlower) / 2 ) + classified_sample$bendlower,
             ifelse(is.na(classified_sample$ulod) & !is.na(classified_sample$llod),
                    classified_sample$llod + 1000,
                    ((classified_sample$ulod - classified_sample$llod) / 2 ) + classified_sample$llod
             ))




    classified_sample$fail_reason <- ifelse(classified_sample$Classification == "Pass Classification",
                                            "Passed",
                                            ifelse(classified_sample$antibody_mfi < classified_sample$midpoint_mfi,
                                                   "Too Diluted",
                                                   "Too Concentrated"))

    #classified_sample_v <<- classified_sample
    #classified_sample_antigen <- classified_sample[classified_sample$antigen %in% selectedAntigens,]

    # ensure distinct combination to avoid an extra n_pass_dilution
    #classified_sample <- unique(classified_sample[, c("patientid", "Classification", "timeperiod", "dilution")])
    classified_sample <- classified_sample[!duplicated(classified_sample[c("patientid", "Classification", "timeperiod", "dilution")]), ]



    classified_by_timeperiod <- as.data.frame(table(classified_sample$patientid,
                                                    classified_sample$Classification,
                                                    classified_sample$timeperiod))

    classified_by_timeperiod <- classified_by_timeperiod[classified_by_timeperiod$Var2 == "Pass Classification",]

    pass_0 <- classified_by_timeperiod[classified_by_timeperiod$Freq == 0, c("Var1", "Var3")] #add var4 for antigen
    #classified_by_timeperiod$pass_n <- paste(classified_by_timeperiod$Var2, classified_by_timeperiod$Freq, sep = "_")

    classified_by_timeperiod_np <- as.data.frame(table(classified_sample$patientid,
                                                       classified_sample$Classification,
                                                       classified_sample$timeperiod,
                                                       classified_sample$fail_reason))

    classified_by_timeperiod_np <- classified_by_timeperiod_np[classified_by_timeperiod_np$Var2 == "Does Not Pass Classification",]

    classified_by_timeperiod_np <- merge(classified_by_timeperiod_np, pass_0, by = c("Var1", "Var3")) #var5 for antigen

    classified_by_timeperiod_np <- classified_by_timeperiod_np[classified_by_timeperiod_np$Freq >= 1,]

    classified_by_timeperiod_np <- classified_by_timeperiod_np[, c("Var1", "Var3", "Var4")]  #var5 for antigen

    class_np_wide <- pivot_wider(classified_by_timeperiod_np, id_cols = c("Var1", "Var3"), names_from = "Var4", values_from = "Var4") #var5 for antigen

    #class_np <<- class_np_wide

    names(class_np_wide) <- c("patientid", "timeperiod", "too_diluted", "too_concentrated")


    class_np_wide$status <- ifelse(is.na(class_np_wide$too_diluted) & is.na(class_np_wide$too_concentrated),
                                   "Acceptable",
                                   ifelse(is.na(class_np_wide$too_diluted),
                                    "Too Diluted",
                                   ifelse(is.na(class_np_wide$too_concentrated),
                                          "Too Concentrated",
                                                 paste(class_np_wide$too_diluted, class_np_wide$too_concentrated, sep = "_")
                                          )
                                   )
    )

    #class_np <<- class_np_wide

    class_np_wide <- class_np_wide[, c("patientid", "timeperiod", "status")]

    classified_by_timeperiod <- classified_by_timeperiod[, c("Var1", "Var3", "Freq")]

    names(classified_by_timeperiod) <- c("patientid", "timeperiod", "n_pass_dilutions")


    #classified_by_timeperiod_v <<- classified_by_timeperiod
    #class_np_wide_v <<- class_np_wide

    classified_merged <- merge(classified_by_timeperiod, class_np_wide, by = c("patientid", "timeperiod"), all.x = TRUE)
    classified_merged$status[is.na(classified_merged$status)] <- "Acceptable"

    #classified_merged_view <<- classified_merged
    return(classified_merged)
}



obtain_passing_patients <- function(classified_sample, selected_status, selectedAntigens, timeperiod, num_passing_dilutions) {
  classified_sample_view <- classified_sample

  passing_patients <- classified_sample[classified_sample$antigen %in% selectedAntigens, ]
  passing_patients <- unique(classified_sample[classified_sample$status == selected_status &
                                                 classified_sample$timeperiod == timeperiod &
                                                 classified_sample$n_pass_dilutions == num_passing_dilutions,]$patientid)
  return(as.numeric(passing_patients))
}


obtain_passing_dilutions_df <- function(classified_sample, selectedAntigen) {
  cat("In obtain passing dilutions\n")
  #classified_passing_dil_in <<- classified_sample
  #classified_concentration_in <<- classified_concentration
  #selected_antigen <<- selectedAntigen

  classified_concentration <- compute_concentration_status(classified_sample, selectedAntigens = selectedAntigen)


  classified_sample_filtered <- classified_sample[classified_sample$antigen %in% selectedAntigen &
                                                    #classified_sample$timeperiod  == selectedTimepoints &
                                                    classified_sample$Classification == "Pass Classification",]



  # classified_concentration_filtered_v <<- classified_concentration_filtered

  merged_df <- merge(classified_sample_filtered, classified_concentration, by = c("patientid", "timeperiod"), all.x = T )


  pass_dilutions_list <- split(merged_df$dilution,
                                      list(merged_df$antigen, merged_df$timeperiod,
                                                                   merged_df$patientid, merged_df$status, merged_df$n_pass_dilutions),
                                                              drop = TRUE)  # Remove empty groups

  # Convert the list into a structured data frame and expand dilutions
  df_dilutions <- do.call(rbind, lapply(names(pass_dilutions_list), function(name) {
                                   split_values <- unlist(strsplit(name, "\\."))  # Extract antigen, time point, patient ID, status, n pass dilutions
                                   data.frame(antigen = split_values[1],
                                              timeperiod = split_values[2],
                                              patientid = as.numeric(split_values[3]),
                                              status = split_values[4],
                                              n_pass_dilutions = split_values[5],
                                              dilution = unlist(pass_dilutions_list[[name]]))  # Expand each dilution into separate rows
                                 }))

 return(df_dilutions)
}


# get average AU via geometric mean. If only 1 pass use that AU
compute_average_au <- function(classified_sample_linearity) {
  # find those that pass in the classified data
  pass_dilution_data <- classified_sample_linearity[classified_sample_linearity$Classification == "Pass Classification",]
  if (nrow(pass_dilution_data) > 1) {
    avg_au <- round(geom_mean(pass_dilution_data$antibody_au),2)
  } else if (nrow(pass_dilution_data) == 1) {
    avg_au <- pass_dilution_data$antibody_au
  } else {
    avg_au <- NA
  }
  return(avg_au)
}

### AU Treatment functions

## Keep all AU
preserve_all_au <- function(classified_sample_linearity) {
  classified_sample_linearity <- classified_sample_linearity
  names(classified_sample_linearity)[names(classified_sample_linearity) == "au"] <- "processed_au"
  return(classified_sample_linearity)
}

# exclude_all_au <- function(classified_sample_linearity) {
#   classified_sample_linearity <- classified_sample_linearity[0,]
#   return(classified_sample_linearity)
# }

# Keep Passing AU
preserve_passing_au <- function(classified_sample) {
  pass_dilution_data <- classified_sample[classified_sample$au_treatment == "passing_au", ]
  names(pass_dilution_data)[names(pass_dilution_data) == "au"] <- "processed_au"
  return(pass_dilution_data)
}

# Geometric Mean of all AU Measurements
geometric_mean_all_au_2 <- function(classified_sample) {
  results <- aggregate(au ~ antigen + patientid, classified_sample, geom_mean)

  names(classified_sample)[names(classified_sample) == "au"] <- "original_au"

  result_df <- merge(classified_sample, results, by = c("antigen", "patientid"))
 # result_df <- result_df[!names(result_df) %in% "au.x"]
  names(result_df)[names(result_df) == "au"] <- "processed_au"

  return(result_df)
}
# geometric_mean_all_au <- function(classified_sample_linearity) {
#  results <- aggregate(final_au ~ patientid, classified_sample_linearity, geom_mean)
#
#  result_df <- merge(classified_sample_linearity, results, by = c("patientid"))
#  result_df <- result_df[!names(result_df) %in% "final_au.x"]
#  names(result_df)[names(result_df) == "final_au.y"] <- "final_au"
#
#  return(result_df)
# }



# Average AU among passing subjects
# geometric_mean_passing_au <- function(classified_sample_linearity) {
#   # Find rows that pass classification
#   pass_dilution_data <- classified_sample_linearity[classified_sample_linearity$au_treatment == "geom_passing_au", ]
#
#   results <- aggregate(final_au ~ patientid, pass_dilution_data, geom_mean)
#   result_df <- merge(pass_dilution_data, results, by = c("patientid"))
#   result_df <- result_df[!names(result_df) %in% "final_au.x"]
#   names(result_df)[names(result_df) == "final_au.y"] <- "final_au"
#
#   return(result_df)
# }

geometric_mean_passing_au_2 <- function(classified_sample) {
  # Find rows that pass classification
  pass_dilution_data <- classified_sample[classified_sample$au_treatment == "geom_passing_au", ]

  results <- aggregate(au ~ antigen + patientid, pass_dilution_data, geom_mean)
  names(pass_dilution_data)[names(pass_dilution_data) == "au"] <- "original_au"

  result_df <- merge(pass_dilution_data, results, by = c("antigen", "patientid"))
  #result_df <- result_df[!names(result_df) %in% "au.x"]
  names(result_df)[names(result_df) == "au"] <- "processed_au"

  return(result_df)
}



# Replace AU with geometric mean of positive controls
# most concentrated value for positive control if dilution fraction is a 1, then AU for positive control = 1000

geometric_mean_positive_controls <- function(classified_sample_linearity, positive_controls) {
  positive_controls <- positive_controls
  most_concentrated_dilution <- min(positive_controls$dilution)

  if (most_concentrated_dilution == 1) {
    au_positive_control <- 1000
  } else {
    au_positive_control <- (1000 / most_concentrated_dilution) * 2
  }

  classified_sample_linearity$processed_au <- au_positive_control

  return(classified_sample_linearity)
}
# geometric_mean_positive_controls <- function(classified_sample_linearity, positive_controls) {
#   antigen_geo_mean <- geom_mean(positive_controls$mfi)
#   classified_sample_linearity$final_au <- antigen_geo_mean
#
#   return(classified_sample_linearity)
# }

# Replace AU with geometric mean of blanks
# 1/2 of the most diluted value in the dilution series    ---from the x-axis values
# 1000/ 32000 (factor) = AU for most diluted value
# divide by 2 that is AU for the blank.
geometric_mean_blanks <- function(classified_sample_linearity, standard_curve_data) {
  most_diluted_dilution <- max(standard_curve_data$dilution)
  most_diluted_au <- 1000 / most_diluted_dilution

  au_blank <- most_diluted_au / 2
  au_blank_mean <- geom_mean(au_blank)
  classified_sample_linearity$processed_au <- au_blank_mean

  return(classified_sample_linearity)
}


# geometric_mean_blanks <- function(classified_sample_linearity, blanks_df) {
#   antigen_geo_mean <- geom_mean(blanks_df$mfi)
#   classified_sample_linearity$final_au <- antigen_geo_mean
#
#   return(classified_sample_linearity)
# }

save_average_au <- function(conn, average_au_table, dilution_table_cols) {

  average_au_table <- average_au_table[,dilution_table_cols]


  cat("Before delete query")


  # delete_query <- glue::glue("DELETE
  #   FROM madi_results.xmap_dilution_analysis
  #   WHERE xmap_dilution_analysis_id IN (
  #     SELECT xmap_dilution_analysis_id
  #     FROM madi_results.xmap_dilution_analysis
  #     WHERE study_accession = '{average_au_table$study_accession}'
  #     AND experiment_accession = '{average_au_table$experiment_accession}'
  #     AND antigen = '{average_au_table$antigen}'
  #   );"
  # )
  # Get unique combinations
  unique_combos <- unique(average_au_table[, c("study_accession", "experiment_accession", "antigen")])

  # Loop over each unique combo to generate queries
  for (i in seq_len(nrow(unique_combos))) {
    study <- unique_combos$study_accession[i]
    experiment <- unique_combos$experiment_accession[i]
    antigen <- unique_combos$antigen[i]

    delete_query <- glue::glue("
    DELETE FROM madi_results.xmap_dilution_analysis
    WHERE xmap_dilution_analysis_id IN (
      SELECT xmap_dilution_analysis_id
      FROM madi_results.xmap_dilution_analysis
      WHERE study_accession = '{study}'
        AND experiment_accession = '{experiment}'
        AND antigen = '{antigen}'
    );"
    )

    #print(delete_query)
    # DBI::dbExecute(conn, delete_query)  # Uncomment to run
  }

  print(delete_query)
  cat("after delete query")
  delete_outcome <- DBI::dbExecute(conn, delete_query)
  cat("after delete outcome")
  # print(delete_outcome)
  # print(delete_query)

  tryCatch({
    DBI::dbAppendTable(conn, Id(schema = "madi_results", table = "xmap_dilution_analysis"), average_au_table)
    # output$saveModelFitMessage <- renderText("Model fit row inserted successfully")
    showNotification(glue::glue("Dilution Analysis successfully saved"), type = "message")
    #showNotification("Model fit row inserted successfully.", type = "message")
  }, error = function(e) {
    # output$saveModelFitMessage <- renderText("Error inserting model fit row")
    showNotification(glue::glue("Error in updating dilution analysis table: {e$message}"), type = "error")
  })
}


#### Dilution Linearity Models
is_optimization_plate <- function(plate_data_in) {
  dilution <- plate_data_in[grepl("^X", plate_data_in$Type),]
  dilution <- sub(".*_(\\d+)$", "\\1", dilution$Description)
  return(length(unique(dilution)) >= 2)
}


is_optimization_experiment_parsed <- function(study_accession, experiment_accession, plate_id, plate_number) {

  cat("In parsing optimization ")
  cat(study_accession)
  cat(experiment_accession)

   if (nchar(experiment_accession) >= 14) {
    diluted_experiment <- substr(experiment_accession, 1, 13)
  } else {
    diluted_experiment <- experiment_accession
  }

  diluted_experiment2 <- paste0(diluted_experiment, "_d")
  # Plates have this pattern
 # plate_pattern <- glue::glue("study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x)")

#dilutions on original plate
  distinct_dilution_query <- glue::glue("SELECT  DISTINCT dilution
	FROM madi_results.xmap_sample
	WHERE study_accession = '{study_accession}'
    AND experiment_accession = '{experiment_accession}'
	AND plate_id = '{plate_id}';
	")

sample_data <- dbGetQuery(conn, distinct_dilution_query)

unique_dilutions <- unique(sample_data$dilution)


  # check_query <- glue::glue("
  #       SELECT COUNT(*) AS count
  #       FROM madi_results.xmap_header
  #       WHERE study_accession = '{study_accession}'
  #         AND experiment_accession = '{diluted_experiment}'
  #
  #     ")
all_count <- c()
for (dil in unique_dilutions) {
  check_query <- glue::glue("SELECT DISTINCT plate_id, sample_dilution_factor ,COUNT(*) AS count
        FROM madi_results.xmap_header
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{diluted_experiment2}'
          AND plate_id = '{study_accession}_{diluted_experiment}_pt{plate_number}_{dil}x'
		GROUP BY  plate_id, sample_dilution_factor
		 HAVING COUNT(*) > 0")
#   check_query <- glue::glue("SELECT plate_id, COUNT(*) AS count
#         FROM madi_results.xmap_header
#         WHERE study_accession = '{study_accession}'
#           AND experiment_accession = '{diluted_experiment}'
# 		 GROUP BY plate_id;"
#     )

  result <- dbGetQuery(conn, check_query)
  count <- nrow(result)
  all_count <- c(all_count, count)
}

  cat("count")
  print(all_count)

  if (all(all_count) > 0) {
    #showNotification(paste("Plates have been split for ", experiment_accession, sep = ""))
    all_exist <- TRUE
  } else if (grepl("_d$", experiment_accession)) {
     all_exist <- TRUE
  }  else {
    all_exist <- FALSE
  }

  return(all_exist)
}
  # print(study_accession)
  # print(experiment_accession)
  # query_sample_plates <- glue::glue("
  #   SELECT DISTINCT plate_id, dilution
  #   FROM madi_results.xmap_sample
  #   WHERE study_accession = '{study_accession}'
  #     AND experiment_accession = '{experiment_accession}'
  # ")
  #
  #
  # sample_data <- dbGetQuery(conn, query_sample_plates)
  #
  #
  # # Count unique dilutions per plate
  # dilution_counts <- tapply(sample_data$dilution, sample_data$plate_id, function(x) length(unique(x)))
  #
  #
  # plate_ids_with_2_or_more_dilutions <- names(dilution_counts[dilution_counts >= 2])
  # all_exist <- TRUE
  # for (plate in plate_ids_with_2_or_more_dilutions) {
  #   cat("Plate ID with multiple dilutions:", plate, "\n")
  #
  #   plate_dilutions <- unique(sample_data$dilution[sample_data$plate_id == plate])
  #
  #   for (dil in plate_dilutions) {
  #     new_plate_id <- paste0(plate, "_", dil)
  #
  #     check_query <- glue::glue("
  #       SELECT COUNT(*) AS count
  #       FROM madi_results.xmap_header
  #       WHERE study_accession = '{study_accession}'
  #         AND experiment_accession = '{experiment_accession}_d'
  #         AND plate_id = '{new_plate_id}'
  #     ")
  #
  #     result <- dbGetQuery(conn, check_query)
  #     count <- result$count[1]
  #
  #     if (count > 0) {
  #       showNotification(" New diluted plate exists:", new_plate_id, "\n")
  #     } else {
  #       all_exist <- FALSE
  #     }
  #   }
  # }
  # # Check if any plate has >= 2 dilutions
  #  return((length(names(dilution_counts[dilution_counts >= 2])) > 0) && !all_exist)
split_optimization_single_upload <- function(study_accession, experiment_accession, plate_id, plate_number) {

  cat("In optimize single upload ")
  cat(study_accession)
  cat(experiment_accession)
  cat(plate_number)



  exclude_antigens <- c("Well", "Type", "Description", "Region", "Gate", "Total", "% Agg Beads", "Sampling Errors", "Rerun Status",
                        "Device Error", "Plate ID", "Regions Selected", "RP1 Target", "Platform Heater Target", "Platform Temp (°C)",
                        "Bead Map", "Bead Count", "Sample Size (µl)", "Sample Timeout (sec)", "Flow Rate (µl/min)", "Air Pressure (psi)",
                        "Sheath Pressure (psi)", "Original DD Gates", "Adjusted DD Gates", "RP1 Gates", "User", "Access Level", "Acquisition Time",
                        "acquisition_time", "Reader Serial Number", "Platform Serial Number", "Software Version", "LXR Library", "Reader Firmware",
                        "Platform Firmware", "DSP Version", "Board Temp (°C)", "DD Temp (°C)", "CL1 Temp (°C)", "CL2 Temp (°C)", "DD APD (Volts)",
                        "CL1 APD (Volts)", "CL2 APD (Volts)", "High Voltage (Volts)", "RP1 PMT (Volts)", "DD Gain", "CL1 Gain", "CL2 Gain", "RP1 Gain")
  # Create a SQL-safe comma-separated string
  exclude_antigens_sql <- paste0("'", paste(exclude_antigens, collapse = "', '"), "'")

  # create a shortened experiment name if greater than 15 characters for optimization plates
  if (nchar(experiment_accession) >= 14) {
    diluted_experiment <- substr(experiment_accession, 1, 13)
  } else {
    diluted_experiment <- experiment_accession
  }

  # keep track of plates pre and post split
  original_plates <- c()
  created_plates <- c()

  # query unique serum dilutions
  query_sample_plate <- glue::glue("
    SELECT DISTINCT plate_id, dilution
    FROM madi_results.xmap_sample
    WHERE study_accession = '{study_accession}'
      AND experiment_accession = '{experiment_accession}'
      AND plate_id = '{plate_id}';
  ")

  sample_data <- dbGetQuery(conn, query_sample_plate)

  unique_dilutions <- unique(sample_data$dilution)


  # #  Count unique dilutions per plate_id
  # dilution_counts <- tapply(sample_data$dilution, sample_data$plate_id, function(x) length(unique(x)))
  #
  # #  Get plate_ids with at least 2 dilutions
  # plate_ids_with_2_or_more_dilutions <- names(dilution_counts[dilution_counts >= 2])
  #
  # #  Iterate over qualifying plates
  # for (plate in plate_ids_with_2_or_more_dilutions) {
  #   cat("Plate ID with multiple dilutions:", plate, "\n")
  #
  #   plate_dilutions <- unique(sample_data$dilution[sample_data$plate_id == plate])
 if (length(unique_dilutions) >= 2) {
    for (dil in unique_dilutions) {
      #new_plate_id <- paste0(plate, "_", dil)
      new_plate_id <- glue("{study_accession}_{diluted_experiment}_pt{plate_number}_{dil}x")

      # Check if this new diluted plate already exists in new experiment
      check_query <- glue::glue("
        SELECT DISTINCT plate_id, sample_dilution_factor, COUNT(*) AS count
        FROM madi_results.xmap_header
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = CONCAT('{diluted_experiment}', '_d')
          AND plate_id = '{study_accession}_{diluted_experiment}_pt{plate_number}_{dil}x'
          GROUP BY plate_id, sample_dilution_factor
          HAVING COUNT(*) > 0;
      ")

      exists_result <- dbGetQuery(conn, check_query)
      #count <-  as.numeric(exists_result$count[1])

        if (nrow(exists_result) > 0) {
        cat("  - Skipping existing plate:", new_plate_id, "\n")
        next
      }

      cat("  - Creating new dilution:", dil, " plate", new_plate_id, "\n")

      # === HEADER ===
      new_dilution_header <- glue::glue("
        INSERT INTO madi_results.xmap_header(
          study_accession, experiment_accession, plate_id, file_name, acquisition_date,
          reader_serial_number, rp1_pmt_volts, rp1_target, auth0_user, workspace_id,
          plateid, plate, sample_dilution_factor, n_wells)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x'),
          CONCAT(file_name),
          acquisition_date,
          reader_serial_number,
          rp1_pmt_volts,
          rp1_target,
          auth0_user,
          workspace_id,
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x'),
          CONCAT('plate_', '{plate_number}'),
          {dil},
          n_wells
        FROM madi_results.xmap_header
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}';
      ")

      dbExecute(conn, new_dilution_header)

      # === BUFFER ===
      new_dilution_blank <- glue::glue("
        INSERT INTO madi_results.xmap_buffer(
          study_accession, experiment_accession, plate_id, well, stype, pctaggbeads,
          samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, dilution, feature)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x'),
          well, stype, pctaggbeads, samplingerrors, antigen, antibody_mfi,
          antibody_n, antibody_name, dilution, feature
        FROM madi_results.xmap_buffer
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}'
           AND antigen NOT IN ({exclude_antigens_sql});
      ") #{new_plate_id}',

      dbExecute(conn, new_dilution_blank)

      # === STANDARD ===
      new_dilution_standard <- glue::glue("
        INSERT INTO madi_results.xmap_standard(
          study_accession, experiment_accession, plate_id, well, stype, sampleid, source, dilution,
          pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, predicted_mfi)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x'),
          well, stype, sampleid, source, dilution, pctaggbeads, samplingerrors,
          antigen, antibody_mfi, antibody_n, antibody_name, feature, predicted_mfi
        FROM madi_results.xmap_standard
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}'
          AND antigen NOT IN ({exclude_antigens_sql});
      ")

      dbExecute(conn, new_dilution_standard)

      insert_sample <- glue::glue("
     INSERT INTO madi_results.xmap_sample (
  study_accession, experiment_accession, plate_id, well, stype,
  sampleid, dilution, pctaggbeads, samplingerrors,
  antigen, antibody_mfi, antibody_n, antibody_name, feature,
  timeperiod, patientid, id_imi, agroup, gate_class, antibody_au,
  antibody_au_se, reference_dilution, gate_class_dil, norm_mfi,
  in_linear_region, gate_class_loq, in_quantifiable_range,
  gate_class_linear_region, quality_score
)
SELECT
  study_accession,
  CONCAT('{diluted_experiment}_d') AS experiment_accession,
  CONCAT(study_accession, '_{diluted_experiment}_pt{plate_number}_', '{dil}x') AS plate_id,
  well, stype, sampleid, dilution,
  pctaggbeads, samplingerrors, antigen,
  antibody_mfi, antibody_n, antibody_name, feature,
  timeperiod, patientid, id_imi, agroup, gate_class, antibody_au,
  antibody_au_se, reference_dilution, gate_class_dil, norm_mfi,
  in_linear_region, gate_class_loq, in_quantifiable_range,
  gate_class_linear_region, quality_score
FROM madi_results.xmap_sample
WHERE study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'
  AND plate_id = '{plate_id}'
  AND dilution = {dil}
  AND antigen NOT IN ({exclude_antigens_sql});")


dbExecute(conn, insert_sample)
    }
    # created_plates <- c(created_plates, new_plate_id)
    # original_plates <- c(original_plates, plate)





    #   if (length(created_plates) > 0) {
    #     plates_list_sql <- paste0("'", paste(original_plates, collapse = "','"), "'")
    #   #   update_sample <- glue::glue("
    #   #   UPDATE madi_results.xmap_sample
    #   #   SET experiment_accession = CONCAT(experiment_accession, '_dilut'),
    #   #       plate_id = CONCAT(plate_id, '_', dilution)
    #   #   WHERE study_accession = '{study_accession}'
    #   #     AND experiment_accession = '{experiment_accession}'
    #   #     AND plate_id IN ({plates_list_sql});
    #   # ")
    #     update_sample <- glue::glue("
    # INSERT INTO madi_results.xmap_sample (
    #   study_accession, experiment_accession, plate_id, well, stype,
    #   sampleid, dilution, pctaggbeads, samplingerrors,
    #   antigen, antibody_mfi, antibody_n, antibody_name, feature
    # )
    # SELECT
    #   study_accession,
    #   CONCAT('{diluted_experiment}_d') AS experiment_accession,
    #   '{new_plate_id}' AS plate_id,
    #   well, stype, sampleid, dilution,
    #   pctaggbeads, samplingerrors, antigen,
    #   antibody_mfi, antibody_n, antibody_name, feature
    # FROM madi_results.xmap_sample
    # WHERE study_accession = '{study_accession}'
    #   AND experiment_accession = '{experiment_accession}'
    #   AND plate_id IN ({plates_list_sql});
    # ")
    #
    #  #print(update_sample)
    #   dbExecute(conn, update_sample)
  #}



  cat("Split optimization completed.\n")
  showNotification("Split optimization completed")
 }
  else {
    showNotification("This plate has 1 serum dilution and cannot be split.")
  }
}

split_optimization_plates <- function(study_accession, experiment_accession) {

  exclude_antigens <- c("Well", "Type", "Description", "Region", "Gate", "Total", "% Agg Beads", "Sampling Errors", "Rerun Status",
                        "Device Error", "Plate ID", "Regions Selected", "RP1 Target", "Platform Heater Target", "Platform Temp (°C)",
                        "Bead Map", "Bead Count", "Sample Size (µl)", "Sample Timeout (sec)", "Flow Rate (µl/min)", "Air Pressure (psi)",
                        "Sheath Pressure (psi)", "Original DD Gates", "Adjusted DD Gates", "RP1 Gates", "User", "Access Level", "Acquisition Time",
                        "acquisition_time", "Reader Serial Number", "Platform Serial Number", "Software Version", "LXR Library", "Reader Firmware",
                        "Platform Firmware", "DSP Version", "Board Temp (°C)", "DD Temp (°C)", "CL1 Temp (°C)", "CL2 Temp (°C)", "DD APD (Volts)",
                        "CL1 APD (Volts)", "CL2 APD (Volts)", "High Voltage (Volts)", "RP1 PMT (Volts)", "DD Gain", "CL1 Gain", "CL2 Gain", "RP1 Gain")
  # Create a SQL-safe comma-separated string
  exclude_antigens_sql <- paste0("'", paste(exclude_antigens, collapse = "', '"), "'")

  # create a shortened experiment name if greater than 15 characters for optimization plates
  if (nchar(experiment_accession) >= 14) {
    diluted_experiment <- substr(experiment_accession, 1, 13)
  } else {
    diluted_experiment <- experiment_accession
  }

  # keep track of plates pre and post split
  original_plates <- c()
  created_plates <- c()

  query_sample_plates <- glue::glue("
    SELECT DISTINCT plate_id, dilution
    FROM madi_results.xmap_sample
    WHERE study_accession = '{study_accession}'
      AND experiment_accession = '{experiment_accession}'
  ")

  sample_data <- dbGetQuery(conn, query_sample_plates)

  #  Count unique dilutions per plate_id
  dilution_counts <- tapply(sample_data$dilution, sample_data$plate_id, function(x) length(unique(x)))

  #  Get plate_ids with at least 2 dilutions
  plate_ids_with_2_or_more_dilutions <- names(dilution_counts[dilution_counts >= 2])

  #  Iterate over qualifying plates
  for (plate in plate_ids_with_2_or_more_dilutions) {
    cat("Plate ID with multiple dilutions:", plate, "\n")

    plate_dilutions <- unique(sample_data$dilution[sample_data$plate_id == plate])

    plate_counter <- 1

    for (dil in plate_dilutions) {
      #new_plate_id <- paste0(plate, "_", dil)
      new_plate_id <- glue("{study_accession}_{diluted_experiment}_pt{plate_counter}_{dil}x")

      # Check if this new diluted plate already exists in new experiment
      check_query <- glue::glue("
        SELECT COUNT(*) AS count
        FROM madi_results.xmap_header
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = CONCAT('{diluted_experiment}', '_d')
          AND plate_id = '{study_accession}_{diluted_experiment}_pt{plate_counter}_{dil}x'
      ")

      exists_result <- dbGetQuery(conn, check_query)

      if (exists_result$count > 0) {
        cat("  - Skipping existing plate:", new_plate_id, "\n")
        next
      }

      cat("  - Creating new dilution:", dil, " plate", new_plate_id, "\n")

      # === HEADER ===
      new_dilution_header <- glue::glue("
        INSERT INTO madi_results.xmap_header(
          study_accession, experiment_accession, plate_id, file_name, acquisition_date,
          reader_serial_number, rp1_pmt_volts, rp1_target, auth0_user, workspace_id,
          plateid, plate, sample_dilution_factor, n_wells)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_counter}_', '{dil}x'),
          CONCAT(file_name, '{dil}'),
          acquisition_date,
          reader_serial_number,
          rp1_pmt_volts,
          rp1_target,
          auth0_user,
          workspace_id,
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_counter}_', '{dil}x'),
          CONCAT('plate_', '{plate_counter}'),
          {dil},
          n_wells
        FROM madi_results.xmap_header
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}';
      ")

     dbExecute(conn, new_dilution_header)

      # === BUFFER ===
      new_dilution_blank <- glue::glue("
        INSERT INTO madi_results.xmap_buffer(
          study_accession, experiment_accession, plate_id, well, stype, pctaggbeads,
          samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, dilution, feature)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_counter}_', '{dil}x'),
          well, stype, pctaggbeads, samplingerrors, antigen, antibody_mfi,
          antibody_n, antibody_name, dilution, feature
        FROM madi_results.xmap_buffer
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}'
           AND antigen NOT IN ({exclude_antigens_sql});
      ") #{new_plate_id}',

     dbExecute(conn, new_dilution_blank)

      # === STANDARD ===
      new_dilution_standard <- glue::glue("
        INSERT INTO madi_results.xmap_standard(
          study_accession, experiment_accession, plate_id, well, stype, sampleid, source, dilution,
          pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, predicted_mfi)
        SELECT
          study_accession,
          CONCAT('{diluted_experiment}', '_d'),
          CONCAT(study_accession, '_{diluted_experiment}_pt{plate_counter}_', '{dil}x'),
          well, stype, sampleid, source, dilution, pctaggbeads, samplingerrors,
          antigen, antibody_mfi, antibody_n, antibody_name, feature, predicted_mfi
        FROM madi_results.xmap_standard
        WHERE study_accession = '{study_accession}'
          AND experiment_accession = '{experiment_accession}'
          AND antigen NOT IN ({exclude_antigens_sql});
      ")

     dbExecute(conn, new_dilution_standard)

     insert_sample <- glue::glue("
     INSERT INTO madi_results.xmap_sample (
  study_accession, experiment_accession, plate_id, well, stype,
  sampleid, dilution, pctaggbeads, samplingerrors,
  antigen, antibody_mfi, antibody_n, antibody_name, feature,
  timeperiod, patientid, id_imi, agroup, gate_class, antibody_au,
  antibody_au_se, reference_dilution, gate_class_dil, norm_mfi,
  in_linear_region, gate_class_loq, in_quantifiable_range,
  gate_class_linear_region, quality_score
)
SELECT
  study_accession,
  CONCAT('{diluted_experiment}_d') AS experiment_accession,
  CONCAT(study_accession, '_{diluted_experiment}_pt{plate_counter}_', '{dil}x') AS plate_id,
  well, stype, sampleid, dilution,
  pctaggbeads, samplingerrors, antigen,
  antibody_mfi, antibody_n, antibody_name, feature,
  timeperiod, patientid, id_imi, agroup, gate_class, antibody_au,
  antibody_au_se, reference_dilution, gate_class_dil, norm_mfi,
  in_linear_region, gate_class_loq, in_quantifiable_range,
  gate_class_linear_region, quality_score
FROM madi_results.xmap_sample
WHERE study_accession = '{study_accession}'
  AND experiment_accession = '{experiment_accession}'
  AND plate_id = '{plate}'
  AND dilution = {dil}
  AND antigen NOT IN ({exclude_antigens_sql});")

    # INSERT INTO madi_results.xmap_sample (
    #   study_accession, experiment_accession, plate_id, well, stype,
    #   sampleid, dilution, pctaggbeads, samplingerrors,
    #   antigen, antibody_mfi, antibody_n, antibody_name, feature
    # )
    # SELECT
    #   study_accession,
    #   CONCAT('{diluted_experiment}_d') AS experiment_accession,
    #   '{new_plate_id}' AS plate_id,
    #   well, stype, sampleid, dilution,
    #   pctaggbeads, samplingerrors, antigen,
    #   antibody_mfi, antibody_n, antibody_name, feature
    # FROM madi_results.xmap_sample
    # WHERE study_accession = '{study_accession}'
    #   AND experiment_accession = '{experiment_accession}'
    #   AND plate_id = '{plate}'
    #   AND dilution = {dil};
    # ")
     dbExecute(conn, insert_sample)
    }
    created_plates <- c(created_plates, new_plate_id)
    original_plates <- c(original_plates, plate)

    plate_counter <- plate_counter + 1



#   if (length(created_plates) > 0) {
#     plates_list_sql <- paste0("'", paste(original_plates, collapse = "','"), "'")
#   #   update_sample <- glue::glue("
#   #   UPDATE madi_results.xmap_sample
#   #   SET experiment_accession = CONCAT(experiment_accession, '_dilut'),
#   #       plate_id = CONCAT(plate_id, '_', dilution)
#   #   WHERE study_accession = '{study_accession}'
#   #     AND experiment_accession = '{experiment_accession}'
#   #     AND plate_id IN ({plates_list_sql});
#   # ")
#     update_sample <- glue::glue("
# INSERT INTO madi_results.xmap_sample (
#   study_accession, experiment_accession, plate_id, well, stype,
#   sampleid, dilution, pctaggbeads, samplingerrors,
#   antigen, antibody_mfi, antibody_n, antibody_name, feature
# )
# SELECT
#   study_accession,
#   CONCAT('{diluted_experiment}_d') AS experiment_accession,
#   '{new_plate_id}' AS plate_id,
#   well, stype, sampleid, dilution,
#   pctaggbeads, samplingerrors, antigen,
#   antibody_mfi, antibody_n, antibody_name, feature
# FROM madi_results.xmap_sample
# WHERE study_accession = '{study_accession}'
#   AND experiment_accession = '{experiment_accession}'
#   AND plate_id IN ({plates_list_sql});
# ")
#
#  #print(update_sample)
#   dbExecute(conn, update_sample)
}



  cat("Split optimization completed.\n")
  showNotification("Split optimization completed")
}

# split_optimization_plates <- function(study_accession, experiment_accession) {
#     query_sample_plates <- glue::glue("SELECT DISTINCT  plate_id, dilution
#     	FROM madi_results.xmap_sample
#     	WHERE study_accession = '{study_accession}'
#     	AND experiment_accession = '{experiment_accession}'")
#
#      sample_data <<- dbGetQuery(conn, query_sample_plates)
#      # Step 1: Count unique dilutions per plate_id
#      dilution_counts <- tapply(sample_data$dilution, sample_data$plate_id, function(x) length(unique(x)))
#
#      # Step 2: Get plate_ids with at least 2 dilutions
#      plate_ids_with_2_or_more_dilutions <- names(dilution_counts[dilution_counts >= 2])
#
#      # Step 3: (Optional) Get unique plate IDs from that list
#     # unique_plate_ids <<- unique(plate_ids_with_2_or_more_dilutions)
#      #unique_plate_id_list <- unique(sample_data$plate_id)
#
#      for (plate in plate_ids_with_2_or_more_dilutions) {
#        cat("Plate ID with multiple dilutions:", plate, "\n")
#
#        # Optionally: get dilutions for this plate
#        plate_dilutions <- unique(sample_data$dilution[sample_data$plate_id == plate])
#
#        for (dil in plate_dilutions) {
#          cat("  - Dilution:", dil, "\n")
#          # Optional: Call another function to generate SQL or process the plate/dilution
#          # Create new header for the dilutions
#             new_dilution_header <-  glue::glue("INSERT INTO madi_results.xmap_header(
#       study_accession, experiment_accession, plate_id, file_name, acquisition_date,
#       reader_serial_number, rp1_pmt_volts, rp1_target, auth0_user, workspace_id,
#       plateid, plate, sample_dilution_factor, n_wells)
#     SELECT
#       study_accession,
#       CONCAT(experiment_accession, '_dilut'),
#       CONCAT(plate_id, '_{dil}'),
#       CONCAT(file_name, '{dil}'),
#       acquisition_date,
#       reader_serial_number,
#       rp1_pmt_volts,
#       rp1_target,
#       auth0_user,
#       workspace_id,
#       CONCAT(plateid, '{dil}'),
#       plate,
#       {dil},
#       n_wells
#     FROM madi_results.xmap_header
#     WHERE study_accession = '{study_accession}'
#       AND experiment_accession IN ('{experiment_accession}');
#     ")
#
#       new_dilution_blank <- glue::glue("
#     INSERT INTO madi_results.xmap_buffer(
#       study_accession, experiment_accession, plate_id, well, stype, pctaggbeads,
#       samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, dilution, feature)
#     SELECT
#       study_accession,
#       CONCAT(experiment_accession, '_dilut'),
#       CONCAT(plate_id, '_{dil}'),
#       well,
#       stype,
#       pctaggbeads,
#       samplingerrors,
#       antigen,
#       antibody_mfi,
#       antibody_n,
#       antibody_name,
#       dilution,
#       feature
#     FROM madi_results.xmap_buffer
#     WHERE study_accession = '{study_accession}'
#       AND experiment_accession IN ('{experiment_accession}');
#     ")
#
#       new_dilution_standard <- glue::glue("
# INSERT INTO madi_results.xmap_standard(
#   study_accession, experiment_accession, plate_id, well, stype, sampleid, source, dilution,
#   pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, predicted_mfi)
# SELECT
#   study_accession,
#   CONCAT(experiment_accession, '_dilut'),
#   CONCAT(plate_id, '_{dil}'),
#   well,
#   stype,
#   sampleid,
#   source,
#   dilution,
#   pctaggbeads,
#   samplingerrors,
#   antigen,
#   antibody_mfi,
#   antibody_n,
#   antibody_name,
#   feature,
#   predicted_mfi
# FROM madi_results.xmap_standard
# WHERE study_accession = '{study_accession}'
#   AND experiment_accession IN ('{experiment_accession}');
# ")
#
#        }
#      }
#
#
# }

prepare_lm_sample_data <- function(study_accession, experiment_accession, is_log_mfi_axis, response_type) {
  # query_samples <- glue::glue("SELECT xmap_sample_id, study_accession, experiment_accession, plate_id, timeperiod, patientid, well, stype, sampleid, id_imi, agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, gate_class, antibody_au, antibody_au_se, norm_mfi, in_linear_region, gate_class_loq, in_quantifiable_range, gate_class_linear_region, quality_score
  #   	FROM madi_results.xmap_sample
  #   	WHERE study_accession = '{study_accession}'
  #   	AND experiment_accession = '{experiment_accession}';")
  query_samples <- glue::glue("
                  SELECT se.best_sample_se_all_id, se.raw_predicted_concentration, se.study_accession, se.experiment_accession,
                  se.timeperiod, se.patientid, se.well, se.stype, se.sampleid, se.agroup, se.pctaggbeads, se.samplingerrors, se.antigen,
                  se.antibody_n, se.plateid, se.plate, se.sample_dilution_factor,
                  se.assay_response_variable, se.assay_independent_variable, se.dilution, se.overall_se, se.assay_response, se.norm_assay_response, se.se_concentration,
                  se.final_predicted_concentration as au, se.pcov, se.source, se.gate_class_loq, se.gate_class_lod, se.gate_class_pcov,
                  se.best_glance_all_id, g.is_log_response
                  FROM madi_results.best_sample_se_all se
                  LEFT JOIN madi_results.best_glance_all g
                      ON se.best_glance_all_id = g.best_glance_all_id
                  WHERE se.study_accession = '{study_accession}'
                  AND se.experiment_accession = '{experiment_accession}';")

  sample_data <- dbGetQuery(conn, query_samples)

  # sample_data <- sample_data[sample_data$gate_class != "Not Evaluated" & sample_data$gate_class_linear_region != "Not Evaluated"
  #                            & sample_data$gate_class_loq != "Not Evaluated",]

  sample_data <- sample_data[!is.na(sample_data$gate_class_lod) & !is.na(sample_data$gate_class_loq),]

  sample_data <- sample_data[sample_data$gate_class_lod == "Acceptable",]

  # sample_data$low_plate <- tolower(sample_data$plate_id)  # Lowercase
  #
  # sample_data$plateid <- str_split_i(sample_data$low_plate, "\\\\",-1)
  # #table(sample_data$plateid)
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed(" "),"_")
  #
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed(".."),"_")
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("."),"_")
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plate_"),"plate")
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plaque"),"plate")
  # sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plate_"),"plate")
  # # table(sample_data$plateid)
  #
  #
  # sample_data$plate  <- ifelse(regexpr('plate', sample_data$plateid) > 0,
  #                              str_replace_all(sample_data$plateid, fixed("plate_"),"plate"),
  #                              ifelse(regexpr('_pt', sample_data$plateid) > 0,
  #                                     str_replace_all(sample_data$plateid, fixed("_pt"),"_plate"),
  #                                     sample_data$plateid
  #                              )
  # )
  # # table(sample_data$plate)
  # #  new_filenames <- sub("(?<=pt\\d+)_\\d+x", "", filenames, perl = TRUE)
  # sample_data$plate <- sub("(plate\\d+).*", "\\1", sample_data$plate)
  # sample_data$plate <- str_split_i(sample_data$plate, "_",-1)
  # table(sample_data$plate)
  #table(sample_data$dilution, sample_data$plate)

  unique_plate_dilution_combination <- unique(sample_data[, c("plate", "dilution", "antigen")])

  distinct_samples <- distinct(sample_data, plate, dilution, antigen, patientid, timeperiod, .keep_all = TRUE)

#   if (is_log_mfi_axis)  {
#     distinct_samples$antibody_mfi <- log10(distinct_samples$antibody_mfi)
#     # distinct_samples$antibody_au <- log10(distinct_samples$antibody_au)
#   }
  if (response_type == "raw_assay_response") {
    t_sample_data <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"),values_from = "assay_response")
  } else if (response_type == "au") {
    t_sample_data <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"),values_from = "au")
  }

  t_sample_data_gc <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"), names_prefix = "gc_",values_from = "gate_class_lod")

  t_sample_data_gc_linear <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"), names_prefix = "gc_linear_",values_from = "gate_class_loq") # gate_class_linear region

  t_sample_data <- merge(t_sample_data, t_sample_data_gc, all.x = T, by = c("antigen", "patientid", "timeperiod"))
  t_sample_data <-  merge(t_sample_data, t_sample_data_gc_linear, all.x = T, by = c("antigen", "patientid", "timeperiod"))


  distinct_samples$plate <- sub(".*?(plate[0-9]+).*", "\\1", distinct_samples$plate)

  distinct_samples <- distinct_samples[!is.na(distinct_samples$dilution), ]

    return(distinct_samples)

}

## Fit lm with AU outcome
fit_model <- function(sub_df) {
  mod <- lm(y_au ~ x_au, data = sub_df[sub_df$ecs_group == "usual",])
  tidy_out <- tidy(mod)
  glance_out <- glance(mod)
  new_x <- seq(min(sub_df$x_au, na.rm = TRUE), max(sub_df$x_au, na.rm = TRUE), length.out = 100)
  pred_df <- data.frame(x_au = new_x)
  pred <- predict(mod, newdata = pred_df, interval = "confidence")
  pred_df <- cbind(pred_df, as.data.frame(pred))


  est_intercept <- as.numeric(tidy_out[tidy_out$term == "(Intercept)", "estimate"])
  est_slope <- as.numeric(tidy_out[tidy_out$term == "x_au", "estimate"])

  sub_df$new_y <- (sub_df$y_au - est_intercept) / est_slope
  fit_data <- data.frame(new_y = sub_df$new_y, x_au = sub_df$x_au, ecs_group = sub_df$ecs_group)
  fit_data <- na.omit(fit_data)
  # model_corr <- lm(new_y ~ sub_df$x_au)
  model_corr <- lm(new_y ~ x_au, data = fit_data[fit_data$ecs_group == "usual",])
  model_corr_tidy <- tidy(model_corr)
  model_corr_glance <- glance(model_corr)
  pred_corr_df <- data.frame(x_au = new_x)
  pred_corr <- predict(model_corr, newdata = pred_corr_df, interval = "confidence")
  pred_corr_df <- cbind(pred_corr_df, as.data.frame(pred_corr))


  return(list(
    tidy = tidy_out,
    glance = glance_out,
    predict = pred_df,
    model_corr_tidy = model_corr_tidy,
    model_corr_glance = model_corr_glance,
    predict_corr = pred_corr_df,
    data_with_new_y = sub_df
    # has_corrected_model = has_corrected_model

  ))

}
## Fit lm with AU outcome
fit_model_xy <- function(sub_df, x, y) {
  sub_df$x <- sub_df[[x]]
  sub_df$y <- sub_df[[y]]

  mod <- lm(y ~ x, data = sub_df[sub_df$ecs_group == "usual",])
  tidy_out <- tidy(mod)
  glance_out <- glance(mod)
  new_x <- seq(min(sub_df$x, na.rm = TRUE), max(sub_df$x, na.rm = TRUE), length.out = 100)
  pred_df <- data.frame(x_au = new_x)
  pred <- predict(mod, newdata = pred_df, interval = "confidence")
  pred_df <- cbind(pred_df, as.data.frame(pred))


  est_intercept <- as.numeric(tidy_out[tidy_out$term == "(Intercept)", "estimate"])
  est_slope <- as.numeric(tidy_out[tidy_out$term == "x_au", "estimate"])

  fit_data <- data.frame(new_y = sub_df$new_y, x = sub_df$x, ecs_group = sub_df$ecs_group)
  fit_data <- na.omit(fit_data)
  # model_corr <- lm(new_y ~ sub_df$x_au)
  model_corr <- lm(new_y ~ x, data = fit_data[fit_data$ecs_group == "usual",])
  model_corr_tidy <- tidy(model_corr)
  model_corr_glance <- glance(model_corr)
  pred_corr_df <- data.frame(x = new_x)
  pred_corr <- predict(model_corr, newdata = pred_corr_df, interval = "confidence")
  pred_corr_df <- cbind(pred_corr_df, as.data.frame(pred_corr))


  return(list(
    tidy = tidy_out,
    glance = glance_out,
    predict = pred_df,
    model_corr_tidy = model_corr_tidy,
    model_corr_glance = model_corr_glance,
    predict_corr = pred_corr_df
    # has_corrected_model = has_corrected_model

  ))

}
#
# fit_mfi_model <- function(sub_df) {
#   mod <- lm(y_mfi ~ x_mfi, data = sub_df[sub_df$ecs_group == "usual",])
#   tidy_out <- tidy(mod)
#   glance_out <- glance(mod)
#   new_x <- seq(min(sub_df$x_mfi, na.rm = TRUE), max(sub_df$x_mfi, na.rm = TRUE), length.out = 100)
#   pred_df <- data.frame(x_mfi = new_x)
#   pred <- predict(mod, newdata = pred_df, interval = "confidence")
#   pred_df <- cbind(pred_df, as.data.frame(pred))
#
#
#   est_intercept <- as.numeric(tidy_out[tidy_out$term == "(Intercept)", "estimate"])
#   est_slope <- as.numeric(tidy_out[tidy_out$term == "x_mfi", "estimate"])
#
#   # if (!is.na(est_slope)) {
#   sub_df$new_y <- (sub_df$y_mfi - est_intercept) / est_slope
#
#   fit_data <- data.frame(new_y = sub_df$new_y, x_mfi = sub_df$x_mfi, ecs_group = sub_df$ecs_group)
#   fit_data <- na.omit(fit_data)
#   # model_corr <- lm(new_y ~ sub_df$x_au)
#   model_corr <- lm(new_y ~ x_mfi, data = fit_data[fit_data$ecs_group == "usual",])
#   model_corr_tidy <- tidy(model_corr)
#   model_corr_glance <- glance(model_corr)
#   pred_corr_df <- data.frame(x_mfi = new_x)
#   pred_corr <- predict(model_corr, newdata = pred_corr_df, interval = "confidence")
#   pred_corr_df <- cbind(pred_corr_df, as.data.frame(pred_corr))
#   #  has_corrected_model <- TRUE
#   # }
#
#   return(list(
#     tidy = tidy_out,
#     glance = glance_out,
#     predict = pred_df,
#     model_corr_tidy = model_corr_tidy,
#     model_corr_glance = model_corr_glance,
#     predict_corr = pred_corr_df,
#     data_with_new_y = sub_df
#     # has_corrected_model = has_corrected_model
#
#   ))
#
# }
fit_raw_assay_response_model <- function(sub_df) {

  mod <- lm(y_assay_response ~ x_assay_response,
            data = sub_df[sub_df$ecs_group == "usual",])

  tidy_out <- tidy(mod)
  glance_out <- glance(mod)

  new_x <- seq(min(sub_df$x_assay_response, na.rm = TRUE),
               max(sub_df$x_assay_response, na.rm = TRUE),
               length.out = 100)

  pred_df <- data.frame(x_assay_response = new_x)
  pred <- predict(mod, newdata = pred_df, interval = "confidence")
  pred_df <- cbind(pred_df, as.data.frame(pred))

  est_intercept <- tidy_out$estimate[tidy_out$term == "(Intercept)"]
  est_slope     <- tidy_out$estimate[tidy_out$term == "x_assay_response"]

  sub_df$new_y <- (sub_df$y_assay_response - est_intercept) / est_slope

  fit_data <- na.omit(data.frame(
    new_y = sub_df$new_y,
    x_assay_response = sub_df$x_assay_response,
    ecs_group = sub_df$ecs_group
  ))

  model_corr <- lm(new_y ~ x_assay_response,
                   data = fit_data[fit_data$ecs_group == "usual",])

  model_corr_tidy <- tidy(model_corr)
  model_corr_glance <- glance(model_corr)

  pred_corr_df <- data.frame(x_assay_response = new_x)
  pred_corr <- predict(model_corr, newdata = pred_corr_df, interval = "confidence")
  pred_corr_df <- cbind(pred_corr_df, as.data.frame(pred_corr))

  return(list(
    tidy = tidy_out,
    glance = glance_out,
    predict = pred_df,
    model_corr_tidy = model_corr_tidy,
    model_corr_glance = model_corr_glance,
    predict_corr = pred_corr_df,
    data_with_new_y = sub_df
  ))
}

# returns antigens valid or length
#  check_unique_dilutions_per_antigen <- function(selected_study, selected_experiment, is_log_mfi_axis) {
#
#    ds <- prepare_lm_sample_data(
#      study_accession = selected_study,
#      experiment_accession = selected_experiment,
#      is_log_mfi_axis = is_log_mfi_axis,
#      response_type = "raw_assay_response"
#    )
#
#    dilution_counts_multi <- ds %>%
#      dplyr::group_by(
#        study_accession,
#        experiment_accession,
#        antigen,
#        plate,
#        patientid,
#        timeperiod
#      ) %>%
#      dplyr::summarise(
#        n_dilutions = dplyr::n_distinct(dilution)
#      )
#
#    antigen_validity <- dilution_counts_multi %>%
#      dplyr::group_by(study_accession, experiment_accession, antigen, plate) %>%
#      dplyr::summarise(
#        has_multidilution = any(n_dilutions > 1),
#        .groups = "drop"
#      )
#
#    antigen_validity <<- antigen_validity[antigen_validity$has_multidilution == T,]$antigen
#
#    cat("end of call of antigen validity")
#    return(antigen_validity)
#
# }
# Preform dilutional linearity regression and checks if there are more than 1 unique dilution
dil_lin_regress <- function(distinct_samples, response_type, exclude_conc_samples) {
  # cat("NAMES of Distinct Samples")
  # print(names(distinct_samples))
  if (length(unique(distinct_samples$dilution)) > 1) {
 # distinct_samples <- distinct_samples[, c("study_accession", "experiment_accession", "antigen", "plate", "dilution", "antibody_mfi", "antibody_au", "patientid", "timeperiod", "gate_class_linear_region", "gate_class", "gate_class_loq", "quality_score")]
  distinct_samples <- distinct_samples[, c(
    "study_accession",
    "experiment_accession",
    "antigen",
    "plate",
    "dilution",
    "au",
    "assay_response",
    "assay_response_variable",
    "pcov",
    "patientid",
    "timeperiod",
    "gate_class_loq",
    "gate_class_lod",
    "gate_class_pcov",
    "is_log_response"
  )]


  dilutions <- sort(unique(distinct_samples$dilution))
  middle_dilution <- dilutions[ceiling(length(dilutions) / 2)]
  cat("Middle dilution")
  print(middle_dilution)

  x_dilution_df <- distinct_samples[distinct_samples$dilution == middle_dilution,]
  cat("x dilution df")
  print(x_dilution_df)

  y_dilution_df <- distinct_samples[distinct_samples$dilution != middle_dilution,]
  print("y_ddilution_df")
  print(y_dilution_df)

  # cat("x dilution df structure")
  # print(str(x_dilution_df))
  # cat("y_dilution_df structure")
  # print(str(y_dilution_df))

  # colnames(x_dilution_df) <- c("study_accession", "experiment_accession", "antigen", "plate", "x_dilution",
  #                              "x_mfi", "x_au", "patientid", "timeperiod", "x_gate_class_linear_region",
  #                              "x_gate_class", "x_gate_class_loq", "x_quality_score")
  colnames(x_dilution_df) <- c("study_accession", "experiment_accession", "antigen", "plate", "x_dilution",
                               "x_au", "x_assay_response", "x_assay_response_variable", "x_pcov", "patientid", "timeperiod", "x_gate_class_loq",
                               "x_gate_class_lod", "x_gate_class_pcov", "x_is_log_response")

  # colnames(y_dilution_df) <- c("study_accession", "experiment_accession", "antigen",
  #                              "plate", "y_dilution", "y_mfi", "y_au", "patientid",
  #                              "timeperiod", "y_gate_class_linear_region", "y_gate_class", "y_gate_class_loq", "y_quality_score")

  colnames(y_dilution_df)  <- c("study_accession", "experiment_accession", "antigen", "plate", "y_dilution",
       "y_au", "y_assay_response", "y_assay_response_variable", "y_pcov", "patientid", "timeperiod", "y_gate_class_loq",
       "y_gate_class_lod", "y_gate_class_pcov", "y_is_log_response")

 # print(head(y_dilution_df))
  dilution_df <- merge(x_dilution_df, y_dilution_df, by = c("study_accession", "experiment_accession", "antigen", "plate", "patientid", "timeperiod"), all.x = T)
  # # Handle when patients are not across dilutions.
  # if (all(is.na(dilution_df$y_dilution))) {
  #   dilution_df <- merge(x_dilution_df, y_dilution_df, by = c("study_accession", "experiment_accession", "antigen", "plate"), all.x = T)
  # }
  # Keep all original data including too concentrated samples
  # cat("Excluding concentrated samples")
  # print(exclude_conc_samples)

 # dilution_df_full <<- dilution_df

  dilution_df_modeling <- dilution_df
  print("first df modeling ")
   print(head(dilution_df_modeling))

  if (exclude_conc_samples) {
    dilution_df_modeling$ecs_group <- case_when(
      dilution_df_modeling$x_gate_class_loq == "Too Concentrated" ~ "Too Concentrated",
      dilution_df_modeling$y_gate_class_loq == "Too Concentrated" ~ "Too Concentrated",
      .default = "usual"
    )
    # dilution_df_modeling <- dilution_df_full[dilution_df_full$x_gate_class_linear_region != "Too Concentrated", ]
    # dilution_df_modeling <- dilution_df_modeling[dilution_df_modeling$y_gate_class_linear_region != "Too Concentrated", ]
  } else {
    dilution_df_modeling$ecs_group <- "usual"
    #dilution_df_modeling <- dilution_df_full
  }
  #dilution_df_modeling <- dilution_df_full

  # filtered data
  if (response_type == "au") {
    dilution_df_modeling <-dilution_df_modeling[!is.na(dilution_df_modeling$x_au) & !is.na(dilution_df_modeling$y_au), ]
  } else if (response_type == "raw_assay_response") {
    dilution_df_modeling <-dilution_df_modeling[!is.na(dilution_df_modeling$x_assay_response) & !is.na(dilution_df_modeling$y_assay_response), ]
  }

  #x_dilution_df <- x_dilution_df[, c("study_accession", "experiment_accession", "antigen", "plate", "patientid", "timeperiod", "")]
  # by_plate_dil_antigen <- group_by(dilution_df, plate, y_dilution, antigen)
 # if (nrow(dilution_df) > 0) {

  #  Pre-calculate adjusted y
  # if (response_type == "au") {
  #   dilution_df_modeling <- dilution_df_modeling %>%
  #     group_by(plate, y_dilution, antigen) %>%
  #     group_modify(~ {
  #       mod <- lm(y_au ~ x_au, data = .x[.x$esc_group == "usual",])
  #       intercept <- coef(mod)[["(Intercept)"]]
  #       slope <- coef(mod)[["x_au"]]
  #       .x$new_y <- (.x$y_au - intercept) / slope
  #       .x
  #     }) %>%
  #     ungroup()
  # } else if (response_type == "mfi") {
  #   dilution_df_modeling <- dilution_df_modeling %>%
  #     group_by(plate, y_dilution, antigen) %>%
  #     group_modify(~ {
  #       mod <- lm(y_mfi ~ x_mfi, data = .x[.x$esc_group == "usual",])
  #       intercept <- coef(mod)[["(Intercept)"]]
  #       slope <- coef(mod)[["x_mfi"]]
  #       .x$new_y <- (.x$y_mfi - intercept) / slope
  #       .x
  #     }) %>%
  #     ungroup()
  # }

  # dilution_df_with_newy <- dilution_df_full %>%
  #   left_join(model_params, by = c("plate", "y_dilution", "antigen")) %>%
  #   mutate(
  #     new_y = case_when(
  #       response_type == "au"  ~ (y_au  - intercept) / slope,
  #       response_type == "mfi" ~ (y_mfi - intercept) / slope,
  #       TRUE ~ NA_real_
  #     )
  #   )

 # fit_model_xy <- function(sub_df, x, y)
  if (response_type == "au") {
    #print(head(dilution_df_modeling))
    # x <- "x_au"
    # y <- "y_au"
    safe_fit_model <- safely(fit_model)
    cat("safe fit model")
    print(safe_fit_model)
    dilution_df_modeling_v <- dilution_df_modeling
    print(dilution_df_modeling_v)
    results <- dilution_df_modeling %>%
      group_by(plate, y_dilution, antigen) %>%
      nest() %>%
      # mutate(
      #   model_results = map(data, fit_model)
      # )
      mutate(
        model_out = map(data, safe_fit_model),          # catch errors safely
        model_results = map(model_out, "result")       # extract result
      ) %>%
      filter(!map_lgl(model_results, is.null))

  } else if (response_type == "raw_assay_response") {
    # x <- "x_mfi"
    # y <- "y_mfi"
    safe_fit_raw_assay_response_model <- safely(fit_raw_assay_response_model)
    results <- dilution_df_modeling %>%
      group_by(plate, y_dilution, antigen) %>%
      nest() %>%
      mutate(
        model_out = map(data, safe_fit_raw_assay_response_model),          # catch errors safely
        model_results = map(model_out, "result")       # extract result
      ) %>%
      filter(!map_lgl(model_results, is.null))
      # mutate(
      #   model_results = map(data, fit_mfi_model)
      # )
  }

  by_plate_dil_antigen <- group_by(dilution_df_modeling, plate, y_dilution, antigen)
  by_plate_dil_antigen <- results %>%
    mutate(data_with_new_y = map(model_results, "data_with_new_y")) %>%
    select(plate, y_dilution, antigen, data_with_new_y) %>%
    unnest(data_with_new_y)

  #print(summary(by_plate_dil_antigen))
  #print(head(by_plate_dil_antigen))
  # by_plate_dil_antigen <- by_plate_dil_antigen %>%
  #   left_join(dilution_df_modeling, by = c("plate", "y_dilution", "antigen")) %>%
  #   mutate(
  #     new_y = if_else(
  #       !is.na(y_au) & !is.na(intercept) & !is.na(slope),
  #       (y_au - intercept) / slope,
  #       NA_real_
  #     )
  #   )

   cat("results")
   print(results)
 # cat("model_out\n")
  #print(results$model_out[[1]])
 # cat("model_results\n")
 # print(results$model_results[[1]])

  tidy_results <- results %>%
     select(plate, y_dilution, antigen, model_results) %>%
     unnest_wider(model_results)

   tidy_results_2 <- tidy_results %>%
     unnest_wider(tidy)

  predict_df <- tidy_results_2[, c("plate", "y_dilution", "antigen", "predict")] %>% unnest(predict)
  glance_df <- tidy_results_2[, c("plate", "y_dilution", "antigen", "glance")] %>% unnest(glance)
  tidy_df <- tidy_results[, c("plate", "y_dilution", "antigen", "tidy")] %>% unnest(tidy)

  model_corr_tidy_df <- tidy_results %>%
    select(plate, y_dilution, antigen, model_corr_tidy) %>%
    unnest(model_corr_tidy)

  model_corr_glance_df <- tidy_results %>%
    select(plate, y_dilution, antigen, model_corr_glance) %>%
    unnest(model_corr_glance)

  predict_corr_df <- tidy_results %>%
    select(plate, y_dilution, antigen, predict_corr) %>%
    unnest(predict_corr)



  return(list(
    tidy_uncorrect_df = tidy_df,
    glance_uncorrect_df = glance_df,
    predict_uncorect_df = predict_df,
    model_corr_tidy_df = model_corr_tidy_df,
    model_corr_glance_df = model_corr_glance_df,
    predict_corr_df = predict_corr_df,
    observed_data = by_plate_dil_antigen
    # has_corrected_flags = tidy_results[, c("plate", "y_dilution", "antigen", "has_corrected_model")]
  ))

  }

   else {
 return(NULL)
 }
}
# Plot one regression in the facet
plot_single_regres <- function(distinct_samples,
                               dil_lin_regress_list,
                               plate,
                               antigen,
                               y_dil,
                               is_dil_lin_corr,
                               response_type,
                               is_log_mfi_axis,
                               legend_tracker = list()) {
  
  concentration_colors <- c(
    "Acceptable / Acceptable" = "#6699cc",
    "Acceptable / Too Concentrated" = "#fc8d62",
    "Acceptable / Too Diluted" = "#8da0cb",
    "Too Concentrated / Acceptable" = "#e78ac3",
    "Too Concentrated / Too Concentrated" = "#e6b800",
    "Too Concentrated / Too Diluted" = "#ffd92f",
    "Too Diluted / Acceptable" = "#5e4fa2",
    "Too Diluted / Too Concentrated" = "#b3b3b3",
    "Too Diluted / Too Diluted" = "#d46a6a"
  )
  
  ## -------------------------
  ## Select prediction object
  ## -------------------------
  pred <- if (is_dil_lin_corr) {
    dil_lin_regress_list$predict_corr_df
  } else {
    dil_lin_regress_list$predict_uncorect_df
  }
  
  pred <- pred[pred$plate == plate &
                 pred$antigen == antigen &
                 pred$y_dilution == y_dil, ]
  
  observed_data <- dil_lin_regress_list$observed_data
  observed_data <- observed_data[
    observed_data$plate == plate &
      observed_data$antigen == antigen &
      observed_data$y_dilution == y_dil, ]
  
  if (nrow(observed_data) == 0 || nrow(pred) == 0) {
    return(list(plot = NULL, legend_tracker = legend_tracker))
  }
  
  observed_data$xy_status <- factor(
    paste(observed_data$x_gate_class_loq,
          observed_data$y_gate_class_loq,
          sep = " / ")
  )
  
  ## -------------------------
  ## Axis + response variables
  ## -------------------------
  is_log_response <- isTRUE(unique(observed_data$y_is_log_response)[1])
  
  assay_term <- unique(trimws(observed_data$y_assay_response_variable))
  assay_term <- assay_term[!is.na(assay_term) & assay_term != ""]
  assay_term <- if (length(assay_term) == 0) "Assay Response" else assay_term[1]
  assay_term <- format_assay_terms(assay_term)
  
  if (response_type == "au") {
    x_var <- "x_au"
    y_var <- if (is_dil_lin_corr) "new_y" else "y_au"
    x_label <- paste0("Concentration (Serum Dilution ", observed_data$x_dilution, ")")
    y_label <- paste0("Concentration (Serum Dilution ", observed_data$y_dilution, ")")
  } else {
    x_var <- "x_assay_response"
    y_var <- if (is_dil_lin_corr) "new_y" else "y_assay_response"
    
    prefix <- if (is_log_response) "log<sub>10 </sub>" else ""
    x_label <- paste0(prefix, assay_term, " (Serum Dilution ", observed_data$x_dilution, ")")
    y_label <- paste0(prefix, assay_term, " (Serum Dilution ", observed_data$y_dilution, ")")
  }
  
  ## -------------------------
  ## Hover text
  ## -------------------------
  observed_data$hover_text <- paste0(
    "Subject Accession: ", observed_data$patientid, "<br>",
    "Timepoint: ", observed_data$timeperiod, "<br>",
    x_label, ": ", observed_data[[x_var]], "<br>",
    y_label, ": ", observed_data[[y_var]], "<br>",
    "Concentration Status at ", observed_data$x_dilution, ": ",
    observed_data$x_gate_class_loq, "<br>",
    "Concentration Status at ", observed_data$y_dilution, ": ",
    observed_data$y_gate_class_loq
  )
  
  ## -------------------------
  ## Start plot
  ## -------------------------
  p <- plot_ly()
  
  ## ---- CI ribbon
  show_ci <- is.null(legend_tracker[["ci"]])
  legend_tracker[["ci"]] <- TRUE
  
  p <- p %>%
    add_ribbons(
      data = pred,
      x = as.formula(paste0("~", x_var)),
      ymin = ~lwr,
      ymax = ~upr,
      fillcolor = "lightgrey",
      line = list(color = "transparent"),
      name = "95% CI",
      legendgroup = "ci",
      legendrank = 2,
      showlegend = show_ci
    )
  
  ## ---- Linear fit
  show_fit <- is.null(legend_tracker[["fit"]])
  legend_tracker[["fit"]] <- TRUE
  
  p <- p %>%
    add_lines(
      data = pred,
      x = as.formula(paste0("~", x_var)),
      y = ~fit,
      line = list(color = "darkred"),
      name = "Linear Fit",
      legendgroup = "fit",
      legendrank = 1,
      showlegend = show_fit
    )
  
  
  
  ## ---- Identity line
  rng <- range(c(pred[[x_var]], pred$fit), na.rm = TRUE)
  
  show_id <- is.null(legend_tracker[["identity"]])
  legend_tracker[["identity"]] <- TRUE
  
  p <- p %>%
    add_trace(
      x = rng,
      y = rng,
      type = "scatter",
      mode = "lines",
      line = list(color = "black", dash = "dash"),
      name = "Identity Line",
      legendgroup = "identity",
      legendrank = 3,
      showlegend = show_id
    )
  
  
  ## ---- Points by gate class
  for (gc in levels(observed_data$xy_status)) {
    
    group_data <- observed_data[observed_data$xy_status == gc, ]
    if (nrow(group_data) == 0) next
    
    show_gc <- is.null(legend_tracker[[gc]])
    legend_tracker[[gc]] <- TRUE
    
    p <- p %>%
      add_trace(
        data = group_data,
        x = as.formula(paste0("~", x_var)),
        y = as.formula(paste0("~", y_var)),
        type = "scatter",
        mode = "markers",
        marker = list(color = concentration_colors[[gc]], size = 6),
        text = ~hover_text,
        hoverinfo = "text",
        name = gc,
        legendgroup = gc,
        showlegend = show_gc
      )
  }
  
  ## ---- Layout
  p <- p %>%
    layout(
      title = paste0(plate, " : ", antigen),
      xaxis = list(title = unique(x_label)),
      yaxis = list(title = unique(y_label)),
      showlegend = TRUE
    )
  
  return(list(
    plot = p,
    legend_tracker = legend_tracker
  ))
}


# Plot a single plate facet
dilution_lm_facet <- function(distinct_samples,
                              dil_lin_regress_list,
                              plate,
                              antigen,
                              is_dil_lin_corr,
                              response_type,
                              is_log_mfi_axis) {
  
  observed_dat <- dil_lin_regress_list$observed_data
  middle_dilution <- unique(observed_dat$x_dilution)
  y_dilutions <- sort(unique(observed_dat$y_dilution))
  
  dilution_pairs <- data.frame(
    x_dilution = middle_dilution,
    y_dilutions = y_dilutions,
    stringsAsFactors = FALSE
  )
  
  plot_plate_list <- list()
  legend_tracker <- list()   # <-- NEW
  
  for (y_dil in dilution_pairs$y_dilutions) {
    
    res <- plot_single_regres(
      distinct_samples,
      dil_lin_regress_list = dil_lin_regress_list,
      plate = plate,
      antigen = antigen,
      y_dil = y_dil,
      is_dil_lin_corr = is_dil_lin_corr,
      response_type = response_type,
      is_log_mfi_axis = is_log_mfi_axis,
      legend_tracker = legend_tracker   # <-- pass forward
    )
    
    if (!is.null(res$plot)) {
      plot_plate_list[[as.character(y_dil)]] <- res$plot
      legend_tracker <- res$legend_tracker  # <-- update tracker
    }
  }
  
  if (length(plot_plate_list) == 0) {
    return(NULL)
  }
  
  n_plots <- length(plot_plate_list)
  n_cols <- 3
  n_rows <- ceiling(n_plots / n_cols)
  
  if (response_type == "au") {
    title_subtext <- "Interpolated Sample Concentrations" 
  } else {
    title_subtext <- "Raw Assay Responses"
  }
  plate_facet_plot <- subplot(
    plot_plate_list,
    nrows = n_rows,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.1
  ) %>%
    layout(title = paste0("Linearity of ", title_subtext, " (",plate, ", ", antigen, ")"),
           margin = list(t = 60, b = 60))
  
  return(plate_facet_plot)
}

# Produce all plate facets
produce_all_plate_facets <- function(distinct_samples,
                                     dil_lin_regress_list,
                                     selected_antigen,
                                     is_dil_lin_corr,
                                     response_type,
                                     is_log_mfi_axis) {
  
  if (is.null(dil_lin_regress_list)) {
    return(NULL)
  }
  
  available_plates <- sort(unique(distinct_samples$plate))
  available_plates <- sub(".*?(plate[0-9]+).*", "\\1", available_plates)
  
  nested_results <- lapply(available_plates, function(pl) {
    dilution_lm_facet(
      distinct_samples,
      dil_lin_regress_list = dil_lin_regress_list,
      plate = pl,
      antigen = selected_antigen,
      is_dil_lin_corr = is_dil_lin_corr,
      response_type = response_type,
      is_log_mfi_axis = is_log_mfi_axis
    )
  })
  
  names(nested_results) <- available_plates
  
  nested_results <- nested_results[
    order(as.numeric(gsub("\\D+", "", names(nested_results))))
  ]
  
  return(nested_results)
}

# prepare_lm_sample_data <- function(study_accession, experiment_accession, is_log_mfi_axis, response_type) {
#   query_samples <- glue::glue("SELECT xmap_sample_id, study_accession, experiment_accession, plate_id, timeperiod, patientid, well, stype, sampleid, id_imi, agroup, dilution, pctaggbeads, samplingerrors, antigen, antibody_mfi, antibody_n, antibody_name, feature, gate_class, antibody_au, antibody_au_se, reference_dilution, gate_class_dil, norm_mfi, in_linear_region, gate_class_loq, in_quantifiable_range, gate_class_linear_region, quality_score
#     	FROM madi_results.xmap_sample
#     	WHERE study_accession = '{study_accession}'
#     	AND experiment_accession = '{experiment_accession}';")
#
#   sample_data <- dbGetQuery(conn, query_samples)
#
#   sample_data <- sample_data[sample_data$gate_class != "Not Evaluated" & sample_data$gate_class_linear_region != "Not Evaluated"
#                              & sample_data$gate_class_loq != "Not Evaluated",]
#
#   sample_data <- sample_data[sample_data$gate_class == "Acceptable",]
#
#   #sample_data$plateid <- gsub("[[:punct:][:blank:]]+", ".", basename(gsub("\\", "/", sample_data$plate_id, fixed=TRUE)))
#
#   #sample_data_v <<- sample_data
#
#
#   # sample_data$plate <- str_extract(sample_data$plateid, "(?i)(plate|plaque)[\\.]*\\d+[a-zA-Z]*")
#   #
#   sample_data$low_plate <- tolower(sample_data$plate_id)                        # Lowercase
#   # standardized_plate<- gsub("plaque", "plate", standardized_plate)  # Replace 'plaque' with 'plate'
#   # standardized_plate <- gsub("[^a-z0-9]", "", standardized_plate)     # Remove punctuation
#   # standardized_plate <- gsub("^(plate[0-9]+)[a-z]*$", "\\1", standardized_plate)  # Remove trailing letters
#   #
#   #  sample_data$plate <- standardized_plate
#
#   sample_data$plateid <- str_split_i(sample_data$low_plate, "\\\\",-1)
#  # table(sample_data$plateid)
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed(" "),"_")
#
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed(".."),"_")
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("."),"_")
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plate_"),"plate")
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plaque"),"plate")
#   sample_data$plateid <- str_replace_all(sample_data$plateid, fixed("plate_"),"plate")
#   #table(sample_data$plateid)
#
#
#   sample_data$plate  <- ifelse(regexpr('plate', sample_data$plateid) > 0,
#                                str_replace_all(sample_data$plateid, fixed("plate_"),"plate"),
#                                ifelse(regexpr('_pt', sample_data$plateid) > 0,
#                                       str_replace_all(sample_data$plateid, fixed("_pt"),"_plate"),
#                                       sample_data$plateid
#                                )
#   )
#  # table(sample_data$plate)
#   sample_data$plate <- sub("(plate\\d+).*", "\\1", sample_data$plate)
#   sample_data$plate <- str_split_i(sample_data$plate, "_",-1)
#   # table(sample_data$plate)
#   # table(sample_data$dilution, sample_data$plate)
#   # sample_data$plate <- ifelse(regexpr('plate', sample_data$plate) < str_locate_last(sample_data$plate,"_")[ , 1],
#   #                            substr(sample_data$plate,
#   #                                   regexpr('plate', sample_data$plate),
#   #                                   str_locate_last(sample_data$plate,"_")[ , 1]-1
#   #                            ), substr(sample_data$plate,
#   #                                      regexpr('plate', sample_data$plate),str_length(sample_data$plate)-regexpr('plate', sample_data$plate)+7
#   #                            )
#   #  )
#   #  table(sample_data$plate)
#
#   unique_plate_dilution_combination <- unique(sample_data[, c("plate", "dilution", "antigen")])
#
#   distinct_samples <- distinct(sample_data, plate, dilution, antigen, patientid, timeperiod, .keep_all = TRUE)
#
#   if (is_log_mfi_axis)  {
#     distinct_samples$antibody_mfi <- log10(distinct_samples$antibody_mfi)
#     #distinct_samples$antibody_au <- log10(distinct_samples$antibody_au)
#   }
#   if (response_type == "mfi") {
#     t_sample_data <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"),values_from = "antibody_mfi")
#   } else if (response_type == "au") {
#     t_sample_data <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"),values_from = "antibody_au")
#   }
#
#   t_sample_data_gc <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"), names_prefix = "gc_",values_from = "gate_class")
#
#   t_sample_data_gc_linear <- pivot_wider(data = distinct_samples, id_cols = c("antigen","patientid", "timeperiod"), names_from = c("plate", "dilution"), names_prefix = "gc_linear_",values_from = "gate_class_linear_region")
#
#   t_sample_data <- merge(t_sample_data, t_sample_data_gc, all.x = T, by = c("antigen", "patientid", "timeperiod"))
#   t_sample_data <-  merge(t_sample_data, t_sample_data_gc_linear, all.x = T, by = c("antigen", "patientid", "timeperiod"))
#
#   # find avaliable plates
#   avaliable_plates <- unique(distinct_samples$plate)
#   avaliable_plates <- avaliable_plates[!is.na(avaliable_plates)]
#
#   return(list(t_sample_data, avaliable_plates))
#
# }
#
# dil_lin_regress <- function(t_sample_data, selected_plate, selected_antigen, x_dilution, y_dilution, response_type, is_log_mfi_axis) {
#   x_var <- paste(selected_plate, x_dilution, sep = "_")
#   y_var <- paste(selected_plate, y_dilution, sep = "_")
#
#
#   t_sample_antigen_data <- t_sample_data[t_sample_data$antigen == selected_antigen,]
#   # Filter data for non-NA values on x and y
#   filtered_data <- t_sample_antigen_data[
#     !is.na(t_sample_antigen_data[[x_var]]) & !is.na(t_sample_antigen_data[[y_var]]), ]
#
#  # lims <- range(c(filtered_data[[x_var]], filtered_data[[y_var]]), na.rm = TRUE)
#   by_plate_antigen <- group_by(t_sample_data, plate,antigen)
#   model <- do(by_plate_antigen, lm(y_vals ~ x_vals))
#
#   # Extract vectors
#   x_vals <- filtered_data[[x_var]]
#   y_vals <- filtered_data[[y_var]]
#
#   gc_var <- paste("gc_linear", selected_plate, x_dilution, sep = "_")
#   gc_y <- paste("gc_linear", selected_plate, y_dilution, sep = "_")
#
#   #filtered_data$gc_linear_col <- filtered_data[[gc_var]]
#
#   # Fit linear model
#   model <- lm(y_vals ~ x_vals)
#
#   # target_predictor <- 1
#   # target_intercept <- 0
#
#   model_tidy <- tidy(model)
#   est_intercept <- as.numeric(model_tidy[model_tidy$term == "(Intercept)", "estimate"])
#   est_slope <- as.numeric(model_tidy[model_tidy$term == "x_vals", "estimate"])
#
#   new_y <- (y_vals - est_intercept) / est_slope
#
#   model_corr1 <- lm(new_y ~ x_vals)
#   model_corr1_tidy <- tidy(model_corr1)
#   model_corr1_glance <- glance(model_corr1)
#
#   filtered_data$x_vals <- x_vals
#   filtered_data$new_y <- new_y
#
#   filtered_data$gc_linear_col <- factor(filtered_data[[gc_var]],
#                                         levels = c("Too Diluted", "Acceptable", "Too Concentrated"))
#
#   filtered_data$xy_status <- as.factor(paste(filtered_data[[gc_var]], filtered_data[[gc_y]], sep = " / "))
#
#   # filtered_data$gc_shape <- factor(filtered_data[[gc_y]],
#   #                              levels = c("Too Diluted", "Acceptable", "Too Concentrated"))
#
#   filtered_data_v <- filtered_data
#   concentration_colors <- c(
#     "Acceptable / Acceptable" = "#6699cc",
#     "Acceptable / Too Concentrated" = "#fc8d62",
#     "Acceptable / Too Diluted" = "#8da0cb",
#     "Too Concentrated / Acceptable" = "#e78ac3",
#     "Too Concentrated / Too Concentrated" = "#e6b800",
#     "Too Concentrated / Too Diluted" = "#ffd92f",
#     "Too Diluted / Acceptable" = "#e5c494",
#     "Too Diluted / Too Concentrated" = "#b3b3b3",
#     "Too Diluted / Too Diluted" = "#d46a6a"
#   )
#   # Create a sequence of x values for smooth curve
#   x_seq <- seq(min(x_vals), max(x_vals), length.out = 100)
#
#   # Predict fit and confidence intervals on x_seq
#   pred_corr <- predict(model_corr1, newdata = data.frame(x_vals = x_seq), interval = "confidence")
#   pred <- predict(model, newdata = data.frame(x_vals = x_seq), interval = "confidence")
#
#   x_label <- if (response_type == "mfi") {
#     if (is_log_mfi_axis) {
#       paste0("log10 MFI (Dilution ", x_dilution, ")")
#     } else {
#       paste0("MFI (Dilution ", x_dilution, ")")
#     }
#   } else if (response_type == "au") {
#     paste0("AU (Dilution ", x_dilution, ")")
#   }
#   y_label <- if (response_type == "mfi") {
#     if (is_log_mfi_axis) {
#       paste0("log10 MFI (Dilution ", y_dilution, ")")
#     } else {
#       paste0("MFI (Dilution ", y_dilution, ")")
#     }
#   } else if (response_type == "au") {
#     paste0("AU (Dilution ", y_dilution, ")")
#   }
#
#   return(list(x_seq, pred, pred_corr, x_dilution, y_dilution, x_label, y_label, filtered_data))
# }
#
# #plot to be called when making facet
# produce_plotly_regression_plot <- function(t_sample_data, selected_plate, selected_antigen, x_dilution, y_dilution,response_type, is_log_mfi_axis, is_dil_lin_cor) {
#
#   dilin <- dil_lin_regress(t_sample_data, selected_plate, selected_antigen, x_dilution, y_dilution, response_type, is_log_mfi_axis)
#   if (is_dil_lin_cor) {
#     x_seq <- dilin[[1]]
#     pred <- dilin[[3]]
#     x_dilution <- dilin[[4]]
#     y_dilution <- dilin[[5]]
#     x_label <- dilin[[6]]
#     y_label <- dilin[[7]]
#     filtered_data <- dilin[[8]]
#   } else {
#     x_seq <- dilin[[1]]
#     pred <- dilin[[2]]
#     x_dilution <- dilin[[4]]
#     y_dilution <- dilin[[5]]
#     x_label <- dilin[[6]]
#     y_label <- dilin[[7]]
#     filtered_data <- dilin[[8]]
#   }
#
#   # Build plotly plot
#   p <- plot_ly() %>%
#
#     # Confidence ribbon (fill between lwr and upr)
#     add_trace(
#       x = c(x_seq, rev(x_seq)),
#       y = c(pred[, "lwr"], rev(pred[, "upr"])),
#       type = 'scatter',
#       mode = 'lines',
#       fill = 'toself',
#       fillcolor = 'lightgray', #'rgba220, 20, 60, 0.2',  # transparent darkred
#       line = list(color = 'transparent'),
#       #showlegend = FALSE,
#       name = 'Confidence Interval'
#     )
#
#   # shapes <- c("circle", "square", "diamond")
#   # names(shapes) <- levels(filtered_data$gc_y)
#   filtered_data$hover_text <- paste0(
#     x_label, ": ", x_vals, "<br>",
#     y_label, ": ", new_y, "<br>",
#     "Concentration Status at ", x_dilution, ": ", filtered_data[[gc_var]], "<br>",
#     "Concentration Status at ", y_dilution, ": ", filtered_data[[gc_y]]
#   )
#
#   for (gc in levels(filtered_data$xy_status)) {
#     group_data <- filtered_data[filtered_data$xy_status == gc, ]
#     p <- p %>%
#       add_trace(
#         data = group_data,
#         x = ~x_vals,
#         y = ~new_y,
#         type = 'scatter',
#         mode = 'markers',
#         marker = list(color = concentration_colors[[gc]], size = 6),
#         text = ~hover_text,
#         # text = ~paste("Arbritary Units at Dilution ", x_dilution, ":", x_vals, "<br>",
#         #               "Arbritary Units at Dilution ", y_dilution, ":", new_y, "<br>",
#         #               "Concentration Status at Dilution ", x_dilution, ":", .data[[gc_var]], "<br>",
#         #                "Concentration Status at Dilution ", y_dilution, ":", .data[[gc_y]]
#         #               ),
#         hoverinfo = "text",
#         #shape = shapes[gc_shape],
#         #showlegend = F,
#         name = gc
#       )
#   }
#   #points
#   # add_trace(
#   #   data = filtered_data,
#   #   x = ~x_vals,
#   #   y = ~new_y,
#   #   type = 'scatter',
#   #   mode = 'markers',
#   # #  marker = list(color = filtered_data$gc_linear_col, size = 6),
#   #    color = ~gc_linear_col,
#   #   colors = c("Too Diluted" = "blue", "Acceptable" = "green", "Too Concentrated" = "red"),
#   #   name = ~paste('Data Points', gc_linear_col)) %>%
#
#
#
#
#   #  add_trace(
#   #   data = filtered_data,
#   #   x = ~x_vals,
#   #   y = ~y_vals,
#   #   type = 'scatter',
#   #   mode = 'markers',
#   #   marker = list(size = 6),
#   #   color = ~gc_linear,
#   #   colors = c("Acceptable" = "green", "Too Diluted" = "blue", "Too Concentrated" = "red"),
#   #   name = 'Data Points',
#   #   showlegend = TRUE
#   # ) %>%
#   # Regression line
#   p <- p %>%   add_trace(
#     x = x_seq,
#     y = pred[, "fit"],
#     type = 'scatter',
#     mode = 'lines',
#     line = list(color = 'darkred', width = 2),
#     name = 'Linear Fit'
#   ) %>%
#     # Identity line (1-to-1)
#     add_trace(
#       x = x_seq,
#       y = x_seq,
#       type = 'scatter',
#       mode = 'lines',
#       line = list(color = 'black', dash = 'dash'),
#       name = 'Identity Line'
#     ) %>%
#     layout(
#       title = paste0("Linear Regression for ", selected_antigen),
#       xaxis = list(title = x_label, range = x_seq),
#       yaxis = list(title = y_label, range = x_seq),
#       showlegend = TRUE
#     )
#
#
#   return(list(p, model_tidy, model_corr1_tidy,model_corr1_glance, filtered_data, x_label, y_label))
# }
#
# produce_plotly_regression_facet <- function(t_sample_data, selected_plate, selected_antigen, response_type, is_log_mfi_axis) {
#   t_sample_antigen_data <- t_sample_data[t_sample_data$antigen == selected_antigen, ]
#   plate_dilutions <- names(t_sample_antigen_data[grep(paste0("^", selected_plate), names(t_sample_antigen_data))])
#   dilutions <- sort(as.numeric(gsub(".*_", "", plate_dilutions)))
#
#   # Find the middle dilution
#   middle_dilution <- dilutions[ceiling(length(dilutions) / 2)]
#
#   # Create dilution pairs with middle_dilution as x-axis
#   dilution_pairs <- data.frame(
#     x_dilution = middle_dilution,
#     y_dilutions = dilutions[dilutions != middle_dilution],
#     stringsAsFactors = FALSE
#   )
#
#   plot_list <- list()
#   glance_list <- list()
#   processed_data_list <- list()
#
#   for(i in seq_len(nrow(dilution_pairs))) {
#     x_dilution <- dilution_pairs$x_dilution[i]
#     y_dilution <- dilution_pairs$y_dilutions[i]
#
#     # Call once to get both plot and glance
#     result <- produce_plotly_regression_plot(
#       t_sample_data,
#       selected_plate,
#       selected_antigen,
#       x_dilution,
#       y_dilution,
#       response_type,
#       is_log_mfi_axis
#     )
#
#     plot_list[[i]] <- result[[1]]
#
#     glance_fit <- result[[4]]
#     glance_fit$plate <- selected_plate
#     glance_fit$antigen <- selected_antigen
#     glance_fit$x_dilution <- x_dilution
#     glance_fit$y_dilution <- y_dilution
#     glance_fit$response_type <- response_type
#     glance_fit$is_log_mfi_axis <- is_log_mfi_axis
#    # glance_fit$model_id <- paste(selected_plate, selected_antigen, x_dilution, y_dilution, response_type, is_log_mfi_axis, sep = "_")
#     glance_list[[i]] <- glance_fit
#
#     ## store the updated data with newy
#     processed_data_list[[i]] <- result[[5]]
#
#   }
#
#   glance_model_df <- do.call(rbind, glance_list)
#   processed_data_df <- do.call(rbind, processed_data_list)
#
#   n_plots <- length(plot_list)
#   n_cols <- 3
#   n_rows <- ceiling(n_plots / n_cols)
#
#
#   plot <- subplot(
#     plot_list,
#     nrows = n_rows,
#     shareX = FALSE,
#     shareY = FALSE,
#     titleX = TRUE,
#     titleY = TRUE,
#     margin = 0.1
#   ) %>% layout(title = paste0("Regression Facet Plot: ", selected_plate, ", ", selected_antigen),
#                margin = list(t = 120))
#
#   return(list(plot, glance_model_df, processed_data_df))
# }
#
#
#

