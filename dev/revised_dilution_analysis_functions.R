
compute_classified_merged_update <- function(classified_sample, selectedDilutions, study_configuration) {

  # classified_sample <<- classified_sample
  # selectedDilutionss <<- selectedDilutions
  # study_configuration <<- study_configuration

 # classified_sample <- classified_sample[classified_sample$antigen %in% selectedAntigens,]
  classified_sample <- classified_sample[!is.na(classified_sample$dilution) &
                                           classified_sample$dilution %in% selectedDilutions,]

  #classified_sample <<- classified_sample
  print(names(classified_sample))

  if (nrow(classified_sample) == 0) {
    cat("classified_sample is empty, skipping further processing\n")
    return(NULL)
  }


  classified_sample <- classified_sample[!duplicated(classified_sample[c("patientid", "Classification", "timeperiod", "antigen", "dilution")]), ]

  cat("before classifed_by timeperiod \n")
  print(head(classified_sample))

  classified_by_timeperiod <- as.data.frame(table(classified_sample$patientid,
                                                  classified_sample$Classification,
                                                  classified_sample$timeperiod,
                                                  classified_sample$antigen))

  cat("timeperiod class\n")
  print(names(classified_by_timeperiod))
  print(head(classified_by_timeperiod))
  print(unique(classified_by_timeperiod$Var2))


  classified_by_timeperiod <- classified_by_timeperiod[classified_by_timeperiod$Var2 == "Pass Classification", c("Var1", "Var3", "Var4", "Freq")]

  names(classified_by_timeperiod) <- c("patientid", "timeperiod", "antigen", "n_pass_dilutions")

  cat("after classified by timeperiod before merge")
  print(head(classified_by_timeperiod))
  print(summary(classified_by_timeperiod$n_pass_dilutions))

  merged_classification <- merge(classified_sample, classified_by_timeperiod, by = c("patientid","timeperiod", "antigen"), all.x = TRUE)

  cat("\nrow merged classification:")
  print(nrow(merged_classification))
  print(head(merged_classification))

  # For RAU
  #merged_classification$n_pass_dilutions[is.na(merged_classification$n_pass_dilutions)] <- 0
 # classified_merged$n_pass_dilutions[classified_merged$Classification != "Pass Classification"] <- 0

  no_pass_filtered <- merged_classification[merged_classification$n_pass_dilutions == 0,]
  cat("\nno pass filtered row")
  print(nrow(no_pass_filtered))
  print(head(no_pass_filtered))
  if (nrow(no_pass_filtered) > 0 ) {

  # find subjects with mixed concentration statuses that do not pass
  no_pass <- aggregate(concentration_status ~ patientid + timeperiod + antigen, data = no_pass_filtered,#merged_classification[merged_classification$n_pass_dilutions == 0,],
            FUN = function(x) paste(sort(unique(x)), collapse = "_"))

  no_pass_mixed <- no_pass[no_pass$concentration_status == "Too Concentrated_Too Diluted",]
  if(nrow(no_pass_mixed) > 0) {
      merged_classification$original_status <- merged_classification$concentration_status
      no_pass_mixed$n_pass_dilutions <- 0
      names(no_pass_mixed)[names(no_pass_mixed) == "concentration_status"] <- "mixed_status"

    merged_classification <- merge(merged_classification, no_pass_mixed, by = c("patientid", "timeperiod", "antigen", "n_pass_dilutions"), all.x = TRUE)

    merged_classification$concentration_status <- ifelse(!is.na(merged_classification$mixed_status),
                                                         "Too Concentrated_Too Diluted",
                                                         merged_classification$concentration_status)
    merged_classification <- merged_classification[ , setdiff(names(merged_classification), c("original_status", "mixed_status"))]
  }
  }



  desired_params <- c("zero_pass_diluted_Tx", "zero_pass_concentrated_Tx", "zero_pass_concentrated_diluted_Tx", "one_pass_acceptable_Tx", "two_plus_pass_acceptable_Tx")
  au_treats <- study_configuration[study_configuration$param_name %in% desired_params, c("param_name", "param_character_value")]
   au_treats$n_pass_d <- ifelse(grepl("^zero", au_treats$param_name), 0,
                                ifelse(grepl("^one", au_treats$param_name), 1, 2))

   concentration_status_map <- c(
     "zero_pass_diluted_Tx" = "Too Diluted",
     "zero_pass_concentrated_Tx" = "Too Concentrated",
     "zero_pass_concentrated_diluted_Tx" = "Too Concentrated_Too Diluted",
     "one_pass_acceptable_Tx" = "Acceptable",
     "two_plus_pass_acceptable_Tx" = "Acceptable"
   )

   au_treats$concentration_status <- concentration_status_map[au_treats$param_name]

  #au_treats$concentration_status <- c("Too Diluted", "Too Concentrated", "Too Concentrated_Too Diluted", "Acceptable", "Acceptable")

  names(au_treats)[names(au_treats) == "param_character_value"] <- "au_treatment"


 new_treatments <- data.frame(param_name = c("None","None", "None", "None"), au_treatment = c("exclude_au", "exclude_au", "exclude_au", "exclude_au"),
              n_pass_d = c(1,1,2,2), concentration_status = c("Too Concentrated", "Too Diluted", "Too Concentrated", "Too Diluted"))


  au_treats <- rbind(au_treats, new_treatments)
 # au_treats_v <<- au_treats

  merged_classification$n_pass_d <- ifelse(merged_classification$n_pass_dilutions > 1, 2,
                                           merged_classification$n_pass_dilutions)

  merged_classification <- merge(merged_classification, au_treats[, c("au_treatment", "concentration_status", "n_pass_d", "param_name")],
                                      by = c("n_pass_d", "concentration_status"), all.x = TRUE)
  #mc_v <<- merged_classification

  cat("\nMerged classification - names\n")
  print(names(merged_classification))
 # table(merged_classification$n_pass_d, merged_classification$concentration_status, merged_classification$au_treatment, useNA = "ifany")

  return(merged_classification)
}

produce_margin_table <- function(classified_merged, selectedAntigen, selectedDilutions, time_order) {

  margin_merged <- classified_merged[classified_merged$param_name != "None",]
  margin_merged <- margin_merged[margin_merged$dilution %in% selectedDilutions,]
  margin_merged <- margin_merged[margin_merged$antigen == selectedAntigen,]
  cat("Names of classified merged antigen")
  print(names(margin_merged))

  margin_merged <- distinct(margin_merged[,c("patientid", "timeperiod", "n_pass_dilutions", "concentration_status", "au_treatment")])

  cat("\nafter distinct\n")
  print(names(margin_merged))
  # tab <- table(
  #   classified_merged_antigen$n_pass_dilutions,
  #   classified_merged_antigen$concentration_status,
  #   classified_merged_antigen$timeperiod,
  #   classified_merged_antigen$au_treatment
  # )
  #
  # # Optional: Convert to a tidy data.frame
  # tab_df <- as.data.frame(tab)
  tab_df <- aggregate(
    patientid ~ n_pass_dilutions + concentration_status + timeperiod + au_treatment,
    data = margin_merged,
    FUN = function(x) length(unique(x))
  )

  #names(tab_df) <- c("n_pass_dilutions", "concentration_status", "timeperiod", "au_treatment", "n_patients")
  names(tab_df)[names(tab_df) == "patientid"] <- "n_patients"

 # tab_df <<- tab_df

  wide_df <- reshape(
    tab_df,
    timevar = "timeperiod",
    idvar = c("n_pass_dilutions", "concentration_status", "au_treatment"),
    direction = "wide"
  )

  # Clean column names (optional)
  names(wide_df) <- sub("^n_patients\\.", "", names(wide_df))

  exclude_cols <- c("n_pass_dilutions", "concentration_status", "au_treatment")

  # Select only the columns to check (all except exclude_cols)
  timepoint_cols <- setdiff(names(wide_df), exclude_cols)

  for (col in timepoint_cols) {
      wide_df[[col]][is.na(wide_df[[col]])] <- 0
  }

  # Order by time point
  if (!is.null(time_order) && length(time_order) > 0 && !all(is.na(time_order))) {
    timepoint_cols <- intersect(time_order, timepoint_cols)
    wide_df <- wide_df[, c(exclude_cols, timepoint_cols)]
  }


  # Keep rows where at least one time point column is non-zero
  # wide_df_clean <- wide_df[rowSums(wide_df[, timepoint_cols] != 0) > 0, ]
  #
  # wide_df_clean <- wide_df_clean[order(wide_df_clean$n_pass_dilutions),]
  #sum_row <- colSums(wide_df_clean[, timepoint_cols])
  sum_row <- colSums(wide_df[, timepoint_cols])

  new_row <- c(
    n_pass_dilutions = "Total",
    concentration_status = "Total",
    au_treatment = "Total",
    sum_row
  )

  new_row_df <- as.data.frame(t(new_row), stringsAsFactors = FALSE)

  # Make sure numeric columns are numeric, because everything is character now
  for(col in timepoint_cols) {
    new_row_df[[col]] <- as.numeric(new_row_df[[col]])
  }

  # Bind the new row to the existing data frame
 # margin_table <- rbind(wide_df_clean, new_row_df)
  margin_table <- rbind(wide_df, new_row_df)

  i <- sapply(margin_table, is.factor)
  margin_table[i] <- lapply(margin_table[i], as.character)

  # Assign AU treatment colors
  for (tp in timepoint_cols) {
    color_col <- paste0(tp, "_color")
    margin_table[[color_col]] <- ifelse(margin_table[[tp]] > 0, margin_table[["au_treatment"]], "Blank")
  }

  margin_table <- margin_table[, (!names(margin_table) == "au_treatment")]

  names(margin_table)[names(margin_table) == "n_pass_dilutions"] <- "Number of Acceptable Dilutions"
  names(margin_table)[names(margin_table) == "concentration_status"] <- "Concentration Status"
  return(margin_table)

}


