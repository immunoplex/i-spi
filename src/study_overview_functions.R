## study overview functions

gmean <- function(x) {
  return(exp(mean(log(x))))
}
# Function to calculate geometric std deviation
gsd <- function(x) {
  return(exp(sd(log(x))))
}

summarise_data <- function(df) {
  grp_vars <- c("analyte",
                "antigen",
                "plateid",
                "plate",
                "nominal_sample_dilution")

  if ("std_source" %in% names(df)) {
    grp_vars <- c(grp_vars, "std_source")
  }

  if (nrow(df) > 2)
    {  dfsum <- df %>%
        group_by(across(all_of(grp_vars))) %>%
        #group_by(analyte, antigen, plateid, plate, nominal_sample_dilution) %>%
        dplyr::summarise(
          gmean = gmean(mfi),
          gsd = gsd(mfi),
          n = dplyr::n(),
          intraplate_cv_mfi = (sd(mfi)/mean(mfi)) * 100,
          mp_mfi = mean(mfi),
          .groups = "drop"
    )} else {
      dfsum <- data.frame()
    }
  return(dfsum)
}

summarise_by_fit_category_plate <- function(df) {
  if (nrow(df) > 2)
  {
  df <- df %>%
    group_by(analyte, antigen, fit_category) %>%
    dplyr::summarise(
      count = sum(count),
      .groups = "drop"
    ) %>%
    mutate(
      plate = "plates_all"
    )

  df_tot <- df %>%
     group_by(analyte, antigen, plate) %>%
     dplyr::summarise(
       total = sum(count),
       .groups = "drop"
     )

  df <- merge(df, df_tot, by = c("analyte", "antigen", "plate"), all.x = T)
  df$proportion <- df$count/df$total
  df$crit = "Model"
  df$model_class <- "Model"

  #%>%
    # mutate(
    #   proportion = count / total
    # )

  df <- df[,!names(df) %in% ("total")]

} else {
  df <- data.frame()
}
return(df)
}

summarise_by_timeperiod <- function(df) {
  if (nrow(df) > 2)
  {
  dfsum <- df %>%
    dplyr::group_by(analyte, plate, timeperiod) %>%
    dplyr::summarise(
      gmean = gmean(mfi),
      gsd = gsd(mfi),
      n = dplyr::n(),
      intratime_cv_mfi = (sd(mfi)/mean(mfi)) * 100,
      mp_mfi = mean(mfi),
      .groups = "drop"
    )} else {
      dfsum <- data.frame()
    }
  return(dfsum)
}

# mp = mean plate
get_condition_counts <- function(data, condition_col, condition_val, count_col_name, sample_summ) {
  grp_vars <- c("analyte",
                "antigen",
                "plateid")

  if ("std_source" %in% names(data)) {
    grp_vars <- c(grp_vars, "std_source")
  }

  if (nrow(data) > 2)
  {
  filtered <- data %>% dplyr::filter((!!sym(condition_col)) == condition_val)
    if (nrow(filtered) > 0) {
      dfsum <- filtered %>%
        dplyr::group_by(across(all_of(grp_vars))) %>%
        #dplyr::group_by(analyte, antigen, plateid) %>%
        dplyr::summarise(!!count_col_name := dplyr::n(), .groups = "drop")
    } else {
      # if no rows match, create empty data.frame with zeros for all groups in sample_summ
      dfsum <- sample_summ %>%
        dplyr::select(all_of(grp_vars)) %>%
       # dplyr::select(analyte, antigen, plateid) %>%
        dplyr::mutate(!!count_col_name := 0)
    }
  } else {
    dfsum <- data.frame()
  }
  return(dfsum)
}
#
# get_condition_counts <- function(data, condition_col, condition_val, count_col_name, sample_summ) {
#   if (nrow(data) > 2) {
#     filtered <- data %>%
#       dplyr::filter((!!sym(condition_col)) == condition_val)
#
#     if (nrow(filtered) > 0) {
#       # Normal summarise case
#       dfsum <- filtered %>%
#         dplyr::group_by(analyte, antigen, plateid) %>%
#         dplyr::summarise(!!count_col_name := dplyr::n(), .groups = "drop")
#
#     } else {
#       # No filtered rows
#       if (is.data.frame(sample_summ) && nrow(sample_summ) > 0) {
#
#         dfsum <- sample_summ %>%
#           dplyr::select(analyte, antigen, plateid) %>%
#           dplyr::mutate(!!count_col_name := 0)
#
#       } else {
#         # sample_summ empty or missing necessary columns → return empty with count col
#         dfsum <- tibble::tibble(!!count_col_name := numeric(0))
#       }
#     }
#
#   } else {
#     # data too small, return empty
#     dfsum <- data.frame()
#   }
#
#   return(dfsum)
# }

# check_plate <- function(conn, selected_study){
#  # conn <- get_db_connection()
#   query_nominal_sample_dilution <- glue::glue_sql("SELECT experiment_accession, plate_id, feature, dilution AS nominal_sample_dilutiond
#   	FROM madi_results.xmap_sample
#   	WHERE study_accession = {selected_study};", .con = conn)
#   dilutions <- distinct(dbGetQuery(conn, query_nominal_sample_dilution))
#   query_plates <- glue::glue_sql("SELECT xmap_header_id, experiment_accession, plate_id, plateid,
#   plate, nominal_sample_dilution
#   	FROM madi_results.xmap_header
#   	WHERE study_accession = {selected_study};", .con = conn)
#   plates <- dbGetQuery(conn, query_plates)
#   plates <- merge(plates, dilutions, by = c("plate_id","experiment_accession"), all.x = TRUE)
#   #rm(dilutions)
#   plates$needs_update <- ifelse(is.na(plates$nominal_sample_dilutiond), 1, 0)
#   plates$nominal_sample_dilution <- ifelse(is.na(plates$nominal_sample_dilutiond),
#                                           plates$nominal_sample_dilution,
#                                           plates$nominal_sample_dilutiond)
#   plates$plateidr <- str_trim(str_replace_all(str_split_i(plates$plate_id, "\\\\", -1), " ", ""), side = "both")
#   plates$needs_update <- ifelse(is.na(plates$plateid), 1, plates$needs_update)
#   plates$plateid <- ifelse(is.na(plates$plateid),
#                            plates$plateidr,
#                            plates$plateid)
#   plates$plateid <- str_replace_all(plates$plateid, fixed(".."),"_")
#   plates$plateid <- str_replace_all(plates$plateid, fixed("."),"_")
#   plates$plateid <- str_replace_all(plates$plateid, fixed("plate_"),"plate")
#   plates$plate_id <- str_trim(str_replace_all(plates$plate_id, "\\s", ""), side = "both")
#   if (nrow(plates) > 0) {
#     plates$needs_update <- ifelse(is.na(plates$plate), 1, plates$needs_update)
#     plates$plateids <- tolower(plates$plateid)
#     plates$plateids <- str_trim(str_replace_all(plates$plateids, "\\s", ""), side = "both")
#     plates$plateids <- stringr::str_replace_all(plates$plateids, "plaque", "plate")
#     plates$plateids <- stringr::str_replace_all(plates$plateids, "_pt", "_plate")
#     plates$plate <- str_split_i(plates$plateids, "plate",-1)
#     plates$plate <- paste("plate",str_split_i(plates$plate, "_",1),sep = "_")
#
#
#     plates$plate <- str_extract(plates$plate, "plate_\\d+")
#
#     plates <- distinct(plates[ , c("xmap_header_id","experiment_accession","plate_id","plateid","plate","nominal_sample_dilution","needs_update")])
#   }
#
#
#   # does it need updating?
#   plates_update <- plates[plates$needs_update == 1, c("xmap_header_id","experiment_accession","plate_id","plateid","plate","nominal_sample_dilution")]
#
#   #update
#   if (nrow(plates_update)>0){
#     for(i in seq_len(nrow(plates_update))) {
#       this_row <- plates_update[i, ]
#       print(this_row$plate)
#       sql <- glue_sql(
#         "UPDATE xmap_header
#      SET plateid = {this_row$plateid}, plate = {this_row$plate},
#          nominal_sample_dilution = {this_row$nominal_sample_dilution}
#      WHERE xmap_header_id = {this_row$xmap_header_id};",
#         .con = conn
#       )
#       dbExecute(conn, sql)
#     }
#   }
#   #dbDisconnect(conn)
#
#   plates$analyte <- paste(plates$experiment_accession,plates$nominal_sample_dilution,sep = "_")
#   plates$feature <- plates$experiment_accession
#   plates$plate_id <- toupper(plates$plate_id)
#   plates <- plates[ , c("plate_id", "plateid", "plate", "feature", "analyte", "nominal_sample_dilution")]
#   return(plates)
# }

make_summspec <- function(standard,
                          blank,
                          control,
                          raw,                # <- new argument (raw_samples)
                          low_bead,
                          high_agg,
                          plates,
                          active_samples) {

  ## -----------------------------------------------------------------
  ## 1.  Summarise the *raw* samples (if they exist)
  ## -----------------------------------------------------------------
  #raw_v<<- raw
  if (nrow(raw) > 0) {
    raw_summ <- summarise_data(raw) %>%
      mutate(specimen_type = "raw_sample")

    raw_lowbead <- get_condition_counts(
      data          = raw,
      condition_col = "lowbeadn",
      condition_val = "LowBeadN",
      count_col   = "nlowbead",
      sample_summ = raw_summ
    )

    raw_highagg <- get_condition_counts(
      data          = raw,
      condition_col = "highbeadagg",
      condition_val = "PctAggBeads",
      count_col   = "nhighbeadagg",
      sample_summ = raw_summ
    )

    raw_summ <- raw_summ %>%
      left_join(raw_lowbead,  by = c("analyte", "antigen", "plateid")) %>%
      left_join(raw_highagg,  by = c("analyte", "antigen", "plateid")) %>%
      replace_na(list(nlowbead = 0, nhighbeadagg = 0))
  } else {
    raw_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 2.  Summarise the *standard* samples
  ## -----------------------------------------------------------------
 # standard_v <<- standard
  if (nrow(standard) > 0) {
    cat("STAND")
     print(names(standard))
    # source_std <<- standard %>%
    #   dplyr::select(analyte, antigen, plateid, plate, nominal_sample_dilution, std_source) %>%
    #   dplyr::distinct()

    standard_summ <- summarise_data(standard) %>%
      mutate(specimen_type = "standard")

    std_lowbead <- get_condition_counts(
      data          = standard,
      condition_col = "lowbeadn",
      condition_val = "LowBeadN",
      count_col   = "nlowbead",
      sample_summ = standard_summ
    )

    std_highagg <- get_condition_counts(
      data          = standard,
      condition_col = "highbeadagg",
      condition_val = "Beads",
      count_col   = "nhighbeadagg",
      sample_summ = standard_summ
    )

    standard_summ <- standard_summ %>%
      left_join(std_lowbead,  by = c("analyte", "antigen", "plateid", "std_source")) %>%
      left_join(std_highagg,  by = c("analyte", "antigen", "plateid", "std_source")) %>%
      replace_na(list(nlowbead = 0, nhighbeadagg = 0))

    #standard_summ <<- standard_summ

  } else {
    standard_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 3.  Summarise the *blank* samples
  ## -----------------------------------------------------------------
 # blank_v <<- blank
  if (nrow(blank) > 0) {
    blank_summ <- summarise_data(blank) %>%
      mutate(specimen_type = "blank")

    blk_lowbead <- get_condition_counts(
      data          = blank,
      condition_col = "lowbeadn",
      condition_val = "LowBeadN",
      count_col   = "nlowbead",
      sample_summ = blank_summ
    )

    blk_highagg <- get_condition_counts(
      data          = blank,
      condition_col = "highbeadagg",
      condition_val = "PctAggBeads",
      count_col   = "nhighbeadagg",
      sample_summ = blank_summ
    )

    blank_summ <- blank_summ %>%
      left_join(blk_lowbead, by = c("analyte", "antigen", "plateid")) %>%
      left_join(blk_highagg, by = c("analyte", "antigen", "plateid")) %>%
      replace_na(list(nlowbead = 0, nhighbeadagg = 0))
  } else {
    blank_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 4.  Summarise the *control* samples
  ## -----------------------------------------------------------------
 # control_v <<- control
  if (nrow(control) > 0) {
    control_summ <- summarise_data(control) %>%
      mutate(specimen_type = "control")

    ctl_lowbead <- get_condition_counts(
      data          = control,
      condition_col = "lowbeadn",
      condition_val = "LowBeadN",
      count_col   = "nlowbead",
      sample_summ = control_summ
    )

    ctl_highagg <- get_condition_counts(
      data          = control,
      condition_col = "highbeadagg",
      condition_val = "PctAggBeads",
      count_col   = "nhighbeadagg",
      sample_summ = control_summ
    )

    control_summ <- control_summ %>%
      left_join(ctl_lowbead, by = c("analyte", "antigen", "plateid")) %>%
      left_join(ctl_highagg, by = c("analyte", "antigen", "plateid")) %>%
      replace_na(list(nlowbead = 0, nhighbeadagg = 0))
  } else {
    control_summ <- tibble::tibble()
  }

  #active_samples <<- active_samples

  if (nrow(active_samples) > 0) {
    sample_summ <- summarise_data(active_samples) %>%
      dplyr::mutate(specimen_type = "sample")

    sam_lowbead <- get_condition_counts(
      data          = active_samples,
      condition_col = "lowbeadn",
      condition_val = "LowBeadN",
      count_col     = "nlowbead",
      sample_summ   = sample_summ
    )
    sam_highagg <- get_condition_counts(
      data          = active_samples,
      condition_col = "highbeadagg",
      condition_val = "PctAggBeads",
      count_col     = "nhighbeadagg",
      sample_summ   = sample_summ
    )

   # active_samples_v  <<- active_samples
    sam_above_lod <- get_condition_counts(
      data = active_samples,
      condition_col = "gclod",
      condition_val = "Too Concentrated",
      count_col = "nabovelod",
      sample_summ   = sample_summ
    )

    sam_below_lod <- get_condition_counts(
     data = active_samples,
     condition_col = "gclod",
     condition_val = "Too Diluted",
     count_col = "nbelowlod",
     sample_summ   = sample_summ
    )

    sam_above_loq <- get_condition_counts(
      data = active_samples,
      condition_col = "gcloq",
      condition_val = "Too Concentrated",
      count_col = "naboveloq",
      sample_summ = sample_summ
    )

    sam_below_loq <- get_condition_counts(
      data = active_samples,
      condition_col = "gcloq",
      condition_val = "Too Diluted",
      count_col = "nbelowloq",
      sample_summ   = sample_summ
    )

    sam_in_loq <- get_condition_counts(
      data = active_samples,
      condition_col = "gcloq",
      condition_val = "Acceptable",
      count_col = "ninloq",
      sample_summ = sample_summ
    )

    sample_summ <- sample_summ %>%
      dplyr::left_join(sam_lowbead,  by = c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_highagg,  by = c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_below_lod, by =  c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_above_lod, by =  c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_below_loq, by =  c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_above_loq, by =  c("analyte", "antigen", "plateid")) %>%
      dplyr::left_join(sam_in_loq, by =  c("analyte", "antigen", "plateid")) %>%
      tidyr::replace_na(list(nlowbead = 0, nhighbeadagg = 0, nbelowlod = 0,
                             nabovelod = 0, nbelowloq = 0,  naboveloq = 0,
                             ninloq = 0))
  } else {
    sample_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 5.  Low‑bead “problem” data (already pre‑aggregated by make_problem_sets)
  ## -----------------------------------------------------------------
 # low_bead <<- low_bead

  if (nrow(low_bead) > 0) {
    low_bead_summ <- summarise_data(low_bead) %>%
      mutate(specimen_type = "low_bead_count")
  } else {
    low_bead_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 6.  High‑aggregate “problem” data
  ## -----------------------------------------------------------------
 # high_agg <<- high_agg
  if (nrow(high_agg) > 0) {
    high_agg_summ <- summarise_data(high_agg) %>%
      mutate(specimen_type = "high_aggregate_beads")
  } else {
    high_agg_summ <- tibble::tibble()
  }

  ## -----------------------------------------------------------------
  ## 7.  Combine everything into one master tibble
  ## -----------------------------------------------------------------
  tables <- list(
    raw_summ,
    standard_summ,
    blank_summ,
    control_summ,
    sample_summ,
    low_bead_summ,
    high_agg_summ
  )

  #tables_v <<- tables

  # Keep only those tibbles that have at least one row
  tables_to_bind <- purrr::keep(tables, ~ nrow(.x) > 0)

  master <- dplyr::bind_rows(tables_to_bind)
  print(master)

  # master <- dplyr::bind_rows(
  #   raw_summ,        # raw samples (if any)
  #   standard_summ,
  #   blank_summ,
  #   control_summ,
  #   sample_summ,
  #   low_bead_summ,
  #   high_agg_summ
  # )

  # ## -----------------------------------------------------------------
  # ## 8.  Attach the plate‑level metadata (the `plates` table)
  # ## -----------------------------------------------------------------
  # # `plates` must contain a column called `plateid`.  We do a left join so
  # # that every row in `master` keeps its values even if a plate is missing
  # # from the metadata table.
  # # cat("before left join plates")
  # # print(str(plates))
  # # print(names(master))
  #
  # master <- master %>%
  #   left_join(plates, by = "plateid")
  #

  ## -----------------------------------------------------------------
  ## 9.  Clean‑up column name clashes that can appear after the join
  ## -----------------------------------------------------------------
  # Occasionally a join creates `analyte.x` / `analyte.y`.  We keep the
  # “.x” version (the one that came from the specimen data) and drop the
  # duplicate.
  # if ("analyte.y" %in% names(master)) {
  #   names(master)[names(master) == "analyte.y"] <- "analyte_tmp"
  #   master <- master %>%
  #     rename(analyte = analyte.x) %>%
  #     select(-analyte_tmp)
  # }

  ## -----------------------------------------------------------------
  ## 10.  Return the final summary table
  ## -----------------------------------------------------------------
  return(master)
}

# make_summspec <- function(standard_data, blank_data, control_data, active_samples, low_bead_data, high_agg_bead_data, plates) {
#   # Summarize active_samples (sample data)
#   if(nrow(active_samples) > 2)
#   {
#     sample_summ <- summarise_data(active_samples) %>%
#       mutate(specimen_type = "sample")
#     sample_lowbead <- get_condition_counts(active_samples, "lowbeadn", "LowBeadN", "nlowbead", sample_summ)
#     sample_highbeadagg <- get_condition_counts(active_samples, "highbeadagg", "PctAggBeads", "nhighbeadagg", sample_summ)
#     # sample_gclin <- get_condition_counts(active_samples, "gclin", "Acceptable", "nlinear", sample_summ)
#     # sample_gcconc <- get_condition_counts(active_samples, "gclin", "Too Concentrated", "ntooconc", sample_summ)
#     # sample_gcdilut <- get_condition_counts(active_samples, "gclin", "Too Diluted", "ntoodilut", sample_summ)
#     # sample_gcaulod <- get_condition_counts(active_samples, "gclod", "Too Concentrated", "nabovelod", sample_summ)
#     # sample_gcbllod <- get_condition_counts(active_samples, "gclod", "Too Diluted", "nbelowlod", sample_summ)
#
#     sample_summ <- sample_summ %>%
#      # left_join(sample_gclin, by = c("analyte", "antigen", "plateid")) %>%
#       left_join(sample_highbeadagg, by = c("analyte", "antigen", "plateid")) %>%
#       left_join(sample_lowbead, by = c("analyte", "antigen", "plateid")) %>%
#       # left_join(sample_gcconc, by = c("analyte", "antigen", "plateid")) %>%
#       # left_join(sample_gcdilut, by = c("analyte", "antigen", "plateid")) %>%
#       # left_join(sample_gcaulod, by = c("analyte", "antigen", "plateid")) %>%
#       # left_join(sample_gcbllod, by = c("analyte", "antigen", "plateid")) %>%
#       # Replace NAs in the new count columns with zeros
#       replace_na(list(
#        # nlinear = 0,
#         nhighbeadagg = 0,
#         nlowbead = 0
#         # ntooconc = 0,
#         # ntoodilut = 0,
#         # nabovelod = 0,
#         # nbelowlod = 0
#       ))
#   } else {
#     sample_summ <- data.frame()
#   }
#   cat("after summarise_data sample")
#
#   # Summarise blank data and add specimen_type
#   if(nrow(blank_data) > 2)
#   {
#     cat("summarizing BLANK data")
#     print(str(blank_data))
#
#     buffer_summ <- summarise_data(blank_data) %>%
#       mutate(specimen_type = "blank")
#
#     blank_lowbead <- get_condition_counts(blank_data, "lowbeadn", "LowBeadN", "nlowbead", sample_summ)
#     blank_highbeadagg <- get_condition_counts(blank_data, "highbeadagg", "PctAggBeads", "nhighbeadagg", sample_summ)
#     buffer_summ <- buffer_summ %>%
#       left_join(blank_highbeadagg, by = c("analyte", "antigen", "plateid")) %>%
#       left_join(blank_lowbead, by = c("analyte", "antigen", "plateid")) %>%
#       # Replace NAs in the new count columns with zeros
#       replace_na(list(
#         nlinear = 0,
#         nhighbeadagg = 0,
#         nlowbead = 0,
#         ntooconc = 0,
#         ntoodilut = 0,
#         nabovelod = 0,
#         nbelowlod = 0
#       ))
#   } else {
#     buffer_summ <- data.frame()
#   }
#
#
#   # low_bead_summ <<- summarise_data(low_bead_data) %>%
#   #   mutate(specimen_type = "low_bead_count")
#   #
#   # high_agg_bead_summ <<- summarise_data(high_agg_bead_data) %>%
#   #   mutate(specimen_type = "high_aggregate_beads")
#
#   cat("aftr summarise_data blank")
#   # Summarize control data and add specimen_type
#   if(nrow(control_data) > 2)
#   {
#   control_summ <- summarise_data(control_data) %>%
#     mutate(specimen_type = "control")
#   cat("After Control Sum")
#   print(names(control_summ))
#
#   cat("Sample SUM")
#   print(head(sample_summ))
#   print(names(sample_summ))
#   cat("control data\n")
#   print(head(control_data))
#   print(names(control_data))
#
#
#
#   control_lowbead <- get_condition_counts(control_data, "lowbeadn", "LowBeadN", "nlowbead", sample_summ)
#   control_highbeadagg <- get_condition_counts(control_data, "highbeadagg", "PctAggBeads", "nhighbeadagg", sample_summ)
#   control_summ <- control_summ %>%
#     left_join(control_highbeadagg, by = c("analyte", "antigen", "plateid")) %>%
#     left_join(control_lowbead, by = c("analyte", "antigen", "plateid")) %>%
#     # Replace NAs in the new count columns with zeros
#     replace_na(list(
#       nlinear = 0,
#       nhighbeadagg = 0,
#       nlowbead = 0,
#       ntooconc = 0,
#       ntoodilut = 0,
#       nabovelod = 0,
#       nbelowlod = 0
#     ))
#   } else {
#     control_summ <- data.frame()
#   }
#
#   cat("aftr summarise_data control")
#   print(names(standard_data))
#   # Summarize standard data and add specimen_type
#   if(nrow(standard_data) > 2)
#   {
#     standard_summ <- summarise_data(standard_data) %>%
#       mutate(specimen_type = "standard")
#     standard_lowbead <- get_condition_counts(standard_data, "lowbeadn", "LowBeadN", "nlowbead", sample_summ)
#     standard_highbeadagg <- get_condition_counts(standard_data, "highbeadagg", "PctAggBeads", "nhighbeadagg", sample_summ)
#     standard_summ <- standard_summ %>%
#       left_join(standard_highbeadagg, by = c("analyte", "antigen", "plateid")) %>%
#       left_join(standard_lowbead, by = c("analyte", "antigen", "plateid")) %>%
#       # Replace NAs in the new count columns with zeros
#       replace_na(list(
#         nlinear = 0,
#         nhighbeadagg = 0,
#         nlowbead = 0,
#         ntooconc = 0,
#         ntoodilut = 0,
#         nabovelod = 0,
#         nbelowlod = 0
#       ))
#   } else {
#     standard_summ <- data.frame()
#   }
#
#   cat("aftr summarise_data standard")
#
#   summ_spec <- bind_rows(buffer_summ, control_summ, standard_summ, sample_summ) # low_bead_summ, high_agg_bead_summ)
#
#   # summ_spec$plate_id <- toupper(summ_spec$plate_id)
#   # plates$plate_id <- toupper(plates$plate_id)
#   summ_spec <- merge(summ_spec, plates, by="plateid", all.x = TRUE)
#
#   cat("Sum Spec:\n")
#   print(head(summ_spec))
#   if ("analyte.y" %in% names(summ_spec)) {
#     names(summ_spec)[names(summ_spec) == "analyte.y"] <- "analyte"
#   }
#   return(summ_spec)
# }

plot_study_arm_distribution <- function(patients_arm) {
  p <- plot_ly(patients_arm, x = ~experiment_accession, y = ~num_patients, color = ~agroup, type = 'bar',
               #barmode = 'group',
               text = ~paste0(
                 "Experiment: ", experiment_accession, "<br>",
                 "Arm: ", agroup, "<br>",
                 "Number of Patients: ", num_patients
               ),
               hoverinfo = "text") %>%
    layout(title = "Number of Patients by Experiment and Arm",
           xaxis = list(title = "Experiment Accession"),
           yaxis = list(title = "Number of Patients"))

  return(p)

}

# Convert string to CamelCase
camel_case_converter <- function(x) {
  # Replace non-alphanumeric characters and capitalize the following letter
  gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
}

make_timeperiod_grid <- function(df, x_var, y_var, time_var, count_var, title_var, time_var_order, time_var_palette){

  p <- ggplot(df, aes(x = reorder(get(time_var), -get(time_var_order)), y = get(count_var), fill = reorder(get(time_var), get(time_var_order)))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_grid(rows = vars(get(y_var)), cols = vars(get(x_var))) +
    # geom_text(aes(label = get(count_var)),
    #           position = position_dodge(width = 0.9),
    #           hjust = -0.5) +
    coord_flip() +
    labs(x = camel_case_converter(y_var), y = camel_case_converter(x_var), fill = camel_case_converter(time_var),
         title = title_var) +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(face = "bold"),
          strip.text.y = element_text(angle = 0, hjust = 0),
          #axis.title.y= element_text(hjust = 0),
          axis.text.y =element_blank(),
          axis.ticks.y = element_blank()) +
    scale_fill_manual(values = time_var_palette)

  # legend.title = element_text())
  return(p)
}

make_timeperiod_grid_stacked <- function(df, x_var, y_var, time_var, count_var,
                                         title_var, time_var_order, time_var_palette) {

  names(df)[names(df) == "agroup"] <- "arm"

  p <- ggplot(df, aes(
    x = 1,  # Single bar per facet
    y = get(count_var),
    fill = reorder(get(time_var), get(time_var_order))
  )) +
    geom_bar(stat = "identity") +
    facet_grid(rows = vars(get(y_var)), cols = vars(get(x_var))) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    coord_flip() +
    labs(
      x = camel_case_converter(y_var),
      y = "Proportion",
      fill = camel_case_converter(time_var),
      title = title_var
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      strip.text.y = element_text(angle = 0, hjust = 0),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_fill_manual(values = time_var_palette)

  return(p)
}

prep_analyte_fit_summary <- function(summ_spec_in, standard_fit_res) {
  # standard_fit_res <<- standard_fit_res
  # summ_spec_in <<- summ_spec_in
  merged_df <- merge(summ_spec_in,
                     standard_fit_res[, c("plateid", "antigen", "analyte", "crit", "source")],
                     by = c("plateid", "antigen", "analyte"),
                     all.x = TRUE)

  merged_df$crit[is.na(merged_df$crit)] <- "No Model"

  merged_df$model_class <- merged_df$crit
  # # group 5 param and 4 param models together
  # merged_df$crit[merged_df$crit %in% c("nls_5", "drda_5")] <- "5-parameter"
  # merged_df$crit[merged_df$crit %in% c("nls_4", "nlslm_4")] <- "4-parameter"
  # merged_df$crit[merged_df$crit %in% c("nls_exp")] <- "Exponential"

  # group all models together
  # Group all the model types
  model_types <-  c("Y5", "Yd5", "Y4", "Yd4", "Ygomp4")
  merged_df$crit[merged_df$crit %in% model_types] <- "Model"
  # merged_df$crit[merged_df$crit %in% c("nls_5", "drda_5",
  #                                      "nls_4", "nlslm_4",
  #                                      "nls_exp")] <- "Model"

  return(merged_df)
}

plot_preped_analyte_fit_summary <- function(preped_data, analyte_selector) {

  #preped_data <<- preped_data
  #analyte_selector <<- analyte_selector

  failed_plates <- preped_data %>%
    filter(specimen_type == "standard", crit == "No Model", analyte == analyte_selector) %>%
    pull(plate) %>%
    unique()

  failed_model_count <- preped_data %>%
    filter(specimen_type == "sample", analyte == analyte_selector, plate %in% failed_plates) %>%
    nrow()

  # failed_model_count <- preped_data %>%
  #   filter(specimen_type == "standard", analyte == analyte_selector, crit == "No Model") %>%
  #  distinct(plateid, antigen, analyte) %>%
  #   nrow()
  # failed_model_count <- preped_data %>%
  #     filter(specimen_type == "sample", crit == "No Model", analyte == analyte_selector) %>%
  #        dplyr::summarise(count = dplyr::n()) %>%
  #       dplyr::pull(count)

  #preped_data <<- preped_data
  #long_df <<- preped_data
  #analyte_selector <<- analyte_selector

  # long_df <- preped_data[preped_data$specimen_type == "sample" & preped_data$analyte == analyte_selector,] %>%
  #   pivot_longer(
  #     #cols = c(nlinear, nhighbeadagg, nlowbead, ntooconc, ntoodilut, nabovelod, nbelowlod),
  #     cols = c(ninloq,
  #              nhighbeadagg,
  #              nlowbead,
  #              naboveloq,
  #              nbelowloq,
  #              nabovelod,
  #              nbelowlod),
  #     names_to = "fit_category",
  #     values_to = "count"
  #   ) %>%
  #   # Define a readable and ordered category
  #   mutate(
  #     fit_category = factor(fit_category,
  #                           # levels = c("nlinear", "nhighbeadagg", "nlowbead", "ntooconc", "ntoodilut", "nabovelod", "nbelowlod"),
  #                           levels = c(
  #                             "ninloq",
  #                             "nhighbeadagg",
  #                             "nlowbead",
  #                             "naboveloq",
  #                             "nbelowloq",
  #                             "nabovelod",
  #                             "nbelowlod"
  #                           ),
  #                           # labels = c("In Quantifiable Range", "High Bead Aggregation", "Low Bead Count",
  #                           #            "Too Concentrated", "Too Diluted", "Above ULOD", "Below LLOD")
  #                           labels = c(
  #                             "In Quantifiable Range",
  #                             "High Bead Aggregation",
  #                             "Low Bead Count",
  #                             "Above LOQ",
  #                             "Below LOQ",
  #                             "Above LOD",
  #                             "Below LOD"
  #                           )
  #     )
  #   ) %>%
  #   mutate(
  #     fit_category = if_else(crit == "No Model", "No Model", as.character(fit_category)),
  #     count = if_else(fit_category == "No Model", failed_model_count, count),
  #     fit_category = factor(fit_category,
  #                           # levels = c("No Model", "In Quantifiable Range", "High Bead Aggregation",
  #                           #            "Low Bead Count", "Too Concentrated", "Too Diluted",
  #                           #            "Above ULOD", "Below LLOD")
  #                           levels = c(
  #                             "No Model",
  #                             "In Quantifiable Range",
  #                             "High Bead Aggregation",
  #                             "Low Bead Count",
  #                             "Above LOQ",
  #                             "Below LOQ",
  #                             "Above LOD",
  #                             "Below LOD"
  #                           ))
  #   )

  long_df <- preped_data[
    preped_data$specimen_type == "sample" &
      !is.na(preped_data$analyte) &
      preped_data$analyte == analyte_selector,
  ] %>%
    pivot_longer(
      cols = c(
        ninloq,
        nhighbeadagg,
        nlowbead,
        naboveloq,
        nbelowloq,
        nabovelod,
        nbelowlod
      ),
      names_to  = "fit_category",
      values_to = "count"
    ) %>%
    # map column names → human labels (CHARACTER, not factor)
    mutate(
      fit_category = dplyr::recode(
        fit_category,
        ninloq        = "In Quantifiable Range",
        nhighbeadagg  = "High Bead Aggregation",
        nlowbead      = "Low Bead Count",
        naboveloq     = "Above LOQ",
        nbelowloq     = "Below LOQ",
        nabovelod     = "Above LOD",
        nbelowlod     = "Below LOD"
      )
    ) %>%
    # safely override to No Model
    mutate(
      fit_category = case_when(
        crit == "No Model" ~ "No Model",
        TRUE               ~ fit_category
      ),
      count = if_else(crit == "No Model", failed_model_count, count)
    ) %>%
    # factor ONCE, at the end
    mutate(
      fit_category = factor(
        fit_category,
        levels = c(
          "No Model",
          "In Quantifiable Range",
          "High Bead Aggregation",
          "Low Bead Count",
          "Above LOQ",
          "Below LOQ",
          "Above LOD",
          "Below LOD"
        )
      )
    )

  # filter out fit category of samples
  long_df <- long_df[!(long_df$fit_category %in% c("High Bead Aggregation", "Low Bead Count")), ]
   long_df_group <- long_df %>%
         group_by(plate, antigen, crit) %>%
         mutate(proportion = count / sum(count)) %>%
         ungroup()

   long_df_group$fit_category <- factor(
     long_df_group$fit_category,
     # levels = rev(c(
     #   "Below LLOD",
     #   "Low Bead Count",
     #   "Too Diluted",
     #   "In Quantifiable Range",
     #   "Too Concentrated",
     #   "High Bead Aggregation",
     #   "Above ULOD",
     #   "No Model"
     # )
     levels = rev(c(
       "Below LOD",
       "Above LOD",
       "Below LOQ",
       "Above LOQ",
       "Low Bead Count",
       "High Bead Aggregation",
       "In Quantifiable Range",
       "No Model"
     ))
   )

   long_df_group <- long_df_group
   long_df_group <- long_df_group[, c("analyte", "plate", "antigen", "model_class", "crit", "fit_category", "count", "proportion")]
   plates_all <- summarise_by_fit_category_plate(long_df_group)[, c("analyte", "plate", "antigen", "model_class", "crit", "fit_category", "count", "proportion")]
   long_df_group <- rbind(long_df_group, plates_all)

   #long_df_group <<- long_df_group
   # long_df_group <- long_df_group[long_df_group$proportion > 0,]
   # long_df_group <- droplevels(long_df_group)

   # summsdary <<- long_df_group %>%
   #   summarise(zero_prop = sum(proportion == 0))

   # long_df_group <- long_df_group %>%
   #   mutate(fit_category = droplevels(fit_category))

  #  fit_levels <- rev(c(
  #    "Below LLOD",
  #    "Low Bead Count",
  #    "Too Diluted",
  #    "In Linear Range",
  #    "Too Concentrated",
  #    "High Bead Aggregation",
  #    "Above ULOD",
  #    "No Model"
  #  )
  #  )
  #
  #  long_df_group <- long_df_group %>%
  #    group_by(plate, antigen) %>%
  #    mutate(proportion_norm = count / sum(count)) %>%
  #    ungroup() %>%
  #    mutate(fit_category = factor(fit_category, levels = fit_levels))
  #
  #
  # fit_colors <-  c(
  #        "Below LLOD"            = "#313695",
  #        "Low Bead Count"        = "#4575b4",
  #        "Too Diluted"           = "#91bfdb",
  #        "In Linear Range"       = "#1a9850",  # green (center)
  #        "Too Concentrated"      = "#fee08b",
  #        "High Bead Aggregation" = "#fc8d59",
  #        "Above ULOD"            = "#d73027",
  #        "No Model"              = "black"
  #  )
  #  plots <- lapply(split(long_df_group, long_df_group$antigen), function(df) {
  #    plot_ly(
  #      data = df,
  #      x = ~plate,
  #      y = ~proportion_norm,
  #      color = ~fit_category,
  #      colors = fit_colors,
  #      type = "bar"
  #    ) %>%
  #      layout(
  #        barmode = "stack",
  #        title = unique(df$antigen),
  #        xaxis = list(title = "Plate", tickangle = 90),
  #        yaxis = list(title = "Proportion")
  #      )
  #  })

   # arrange subplots vertically
   # plot <- subplot(plots, nrows = length(plots), shareX = TRUE) %>%
   #   layout(
   #     title = paste(input$analyte_selector,
   #                   "- Sample Estimate Quality by Plate and Antigen (Proportion)."),
   #     legend = list(title = list(text = "Quality"))
   #   )
   # col_map <- c(
   #   "Below LLOD"            = "#313695",
   #   "Low Bead Count"        = "#4575b4",
   #   "Too Diluted"           = "#91bfdb",
   #   "In Quantifiable Range" = "#1a9850",
   #   "Too Concentrated"      = "#fee08b",
   #   "High Bead Aggregation" = "#fc8d59",
   #   "Above ULOD"            = "#d73027",
   #   "No Model"              = "black"
   # )
   # fill_levels <- names(col_map)
   # # same setup of long_df_group as before, but plate can remain character if different per antigen
   # antigens <- unique(long_df_group$antigen)
   # subplot_list <- vector("list", length(antigens))
   #
   # for (i in seq_along(antigens)) {
   #   ag <- antigens[i]
   #   df_ag <- filter(long_df_group, antigen == ag)
   #   plates <- unique(df_ag$plate)
   #
   #   p <- plot_ly()
   #   for (cat in fill_levels) {
   #     df_cat <- df_ag %>% filter(fit_category == cat)
   #     vals <- sapply(plates, function(pn) {
   #       v <- df_cat$proportion[df_cat$plate == pn]
   #       if (length(v) == 0) 0 else v
   #     })
   #     p <- add_trace(p,
   #                    x = plates, y = vals, type = "bar", name = cat,
   #                    marker = list(color = col_map[cat], line = list(color = "black", width = 0.3)),
   #                    showlegend = (i == 1),
   #                    hoverinfo = "text",
   #                    text = paste0("Antigen: ", ag, "<br>Plate: ", plates, "<br>Quality: ", cat, "<br>Proportion: ", vals)
   #     )
   #   }
   #
   #   show_xticks <- i == length(antigens)  # only bottom subplot shows x tick labels
   #
   #   p <- layout(p,
   #               barmode = "stack",
   #               xaxis = list(title = "Plate", tickangle = 90, showticklabels = show_xticks),
   #               yaxis = list(title = "Proportion"),
   #               title = list(text = ag, x = 0, xanchor = "left"))
   #   subplot_list[[i]] <- p
   # }
   #
   # plot <- subplot(subplot_list, nrows = length(subplot_list), shareX = FALSE, shareY = TRUE) %>%
   #   layout(title = paste0(input$analyte_selector, " - Sample Estimate Quality by Plate and Antigen (Proportion)"),
   #          legend = list(orientation = "v", x = 1.02, y = 1),
   #          margin = list(l = 60, r = 150, t = 80, b = 160))
   # plot

   # plots <- long_df_group %>%
   #   split(.$antigen) %>%
   #   lapply(function(df) {
   #     plot_ly(
   #       data = df,
   #       x = ~plate,
   #       y = ~proportion,
   #       color = ~fit_category,
   #       colors = col_map,
   #       type = "bar"
   #     ) %>%
   #       layout(
   #         barmode = "stack",
   #         xaxis = list(title = "Plate", tickangle = 90),
   #         yaxis = list(title = "Proportion"),
   #         legend = list(title = list(text = "Quality")),
   #         title = unique(df$antigen)
   #       )
   #   })
   #
   # # arrange vertically like facet_grid(rows = vars(antigen))
   # plot <- subplot(plots, nrows = length(plots), shareX = TRUE, shareY = FALSE, titleY = TRUE) %>%
   #   layout(
   #     title = paste(input$analyte_selector,"- Sample Estimate Quality by Plate and Antigen (Proportion)")
   #   )

 #long_df_group_v <<- long_df_group
   #print(table(long_df_group$fit_category, useNA = "ifany"))

  plot <- ggplot(long_df_group, aes(x = plate, y = proportion, fill = fit_category)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
    facet_grid(rows = vars(antigen), scales = "free_x", space = "free_x", drop = TRUE) + #cols = vars(crit),
    scale_fill_manual(values = c(
      "Below LOD"             = "#313695",
      "Below LOQ"             = "#91bfdb",
      "In Quantifiable Range" = "#1a9850",  # green (center)
      "Above LOQ"             = "#fee08b",
      "Above LOD"             = "#f46d43", #4575b4",
      "High Bead Aggregation" = "#fc8d59",
      "Low Bead Count"        = "#d73027",
      "No Model"              = "black"
    )) +

    # scale_fill_manual(values = c(
    #   "Below LLOD"            = "#313695",
    #   "Low Bead Count"        = "#4575b4",
    #   "Too Diluted"           = "#91bfdb",
    #   "In Quantifiable Range" = "#1a9850",  # green (center)
    #   "Too Concentrated"      = "#fee08b",
    #   "High Bead Aggregation" = "#fc8d59",
    #   "Above ULOD"            = "#d73027",
    #   "No Model"              = "black"
    # )) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          strip.text.y = element_text(angle = 0, hjust = 0),
          strip.text = element_text(face = "bold")) +
    labs(
      x = "Plate",
      y = "Proportion",
      fill = "Quality",
      title = paste(input$analyte_selector,"- Sample Estimate Quality by Plate and Antigen (Proportion)")
    )

  return(list(plot, long_df_group))

}

# Produce table with number of samples by analyte, antigen, time period table
create_timeperiod_table <- function(sample_spec_timeperiod) {
sample_spec_timeperiod_v1 <- sample_spec_timeperiod[, c("analyte", "plate", "timeperiod", "n", "timeperiod_order")]
sample_spec_timeperiod_v1 <- sample_spec_timeperiod_v1[order(sample_spec_timeperiod_v1$timeperiod_order),]
sample_spec_timeperiod_v1 <- sample_spec_timeperiod_v1[, c("analyte", "plate", "timeperiod", "n")]

return(sample_spec_timeperiod_v1)
}

prepare_arm_balance_data <- function(sample_specimen, sorted_arms) {
  long_df_group <- sample_specimen %>%
      dplyr::distinct(plate, analyte, agroup, patientid) %>%  # ensure 1 row per patient
       dplyr::group_by(plate, analyte, agroup) %>%
      dplyr::summarise(patient_count = dplyr::n(), .groups = "drop")

    long_df_group <- long_df_group %>%
      group_by(plate, analyte) %>%
       mutate(proportion = patient_count / sum(patient_count),
              median_proportion = median(proportion)) %>%
       ungroup()

     long_df_group$agroup_order <- match(long_df_group$agroup, sorted_arms)

  return(long_df_group)
}

# prep_plate_content_summary <- function(summ_spec_df) {
#   summ_spec_dup <- distinct(summ_spec_df, analyte, antigen, plate, specimen_type, .keep_all = TRUE)
#
#   summ_spec_dup$nlowbead <- ifelse(is.na(summ_spec_dup$nlowbead),0,summ_spec_dup$nlowbead)
#
#   summ_spec_dup$specimen_type <- ifelse(
#     summ_spec_dup$nlowbead > 0,
#     paste0(summ_spec_dup$specimen_type, "_low_bead_count"),
#     summ_spec_dup$specimen_type
#   )
#
#   summ_spec_dup$nhighbeadagg <- ifelse(is.na(summ_spec_dup$nhighbeadagg),0,summ_spec_dup$nhighbeadagg)
#
#   summ_spec_dup$specimen_type <- ifelse(
#     summ_spec_dup$nhighbeadagg > 0,
#     paste0(summ_spec_dup$specimen_type, "_high_bead_agg_count"),
#     summ_spec_dup$specimen_type
#   )
#
#   return(summ_spec_dup)
# }
make_antigen_plate_bead <- function(data, specimen_type, analyte, title,
                                    axis_text_size = 12,
                                    size_range = c(4, 10)) {
  plot_data <- data[data$specimen_type == specimen_type &
                      data$analyte == analyte & data$N_wells > 0, ]
  if (nrow(plot_data) == 0) {
    return(NULL)
  } else {

    type_levels <- levels(factor(plot_data$Type))
    pal <- RColorBrewer::brewer.pal(n = max(3, length(type_levels)), name = "Set1")
    type_colors <- setNames(pal[seq_along(type_levels)], type_levels)

    # sort plates (ex: plate_1, plate_2b, plate_10a, plate_10b) optional letter
    pat <- ".*_(\\d+)([A-Za-z]*)$"
    plate_levels <- unique(plot_data$plate)[
      order(
        as.numeric( sub(pat, "\\1", unique(plot_data$plate)) ),
        sub(pat, "\\2", unique(plot_data$plate))
      )
    ]

    plot_data$plate_factor <- factor(plot_data$plate, levels = plate_levels)
    plot_data$plate_pos <- as.numeric(plot_data$plate_factor)
    plate_positions <- seq_along(levels(plot_data$plate_factor))
    plate_labels <- levels(plot_data$plate_factor)

    p <- ggplot(plot_data, aes(x = plate_pos, y = antigen, group = Type)) +
      geom_text_repel(aes(label = N_wells, color = Type, size = N_wells),
                      min.segment.length = 0,
                      box.padding = 0.25,
                      point.padding = 0.35,
                      max.overlaps = 20,
                      force = 1,
                      show.legend = FALSE) +
      scale_color_manual(values = type_colors) +
      scale_size_continuous(range = size_range) +
      theme_minimal() +
      guides(color = "none", size = "none") +
      labs(x = "Plate", y = "Antigen", title = title) +
      theme(
        axis.text.x = element_text(size = axis_text_size),
        axis.text.y = element_text(size = axis_text_size),
        axis.title.x = element_text(size = axis_text_size + 1),
        axis.title.y = element_text(size = axis_text_size + 1),
        plot.title = element_text(size = axis_text_size + 2, hjust = 0.5)
      ) +
      scale_x_continuous(
        breaks = plate_positions,
        labels = plate_labels,
        expand = expansion(mult = c(0.02, 0.02)),
        sec.axis = dup_axis(name = NULL, breaks = plate_positions, labels = plate_labels)
      ) +
      scale_y_discrete(expand = expansion(mult = c(0.2, 0.2)))

    # Build the subtitle legend row showing Type colored labels (unchanged from original)
    n_types <- length(type_levels)
    if (n_types == 1) {
      xpos <- 0.5
    } else {
      xpos <- seq(0, 1, length.out = n_types + 1)[-1] - (0.5 / n_types)
    }

    subtitle_items <- lapply(seq_along(type_levels), function(i) {
      textGrob(
        label = type_levels[i],
        x = unit(xpos[i], "npc"),
        y = unit(0.5, "npc"),
        just = "center",
        gp = gpar(col = type_colors[type_levels[i]], fontsize = 11)
      )
    })
    subtitle_row <- gTree(children = do.call(gList, subtitle_items))
    prefix_grob <- textGrob("", x = unit(0.02, "npc"), y = unit(0.5, "npc"),
                            just = "left", gp = gpar(col = "black", fontsize = 11))
    combined_subtitle <- gTree(children = gList(
      editGrob(prefix_grob, x = unit(0.02, "npc"), y = unit(0.5, "npc"), just = c("left", "center")),
      editGrob(subtitle_row, x = unit(0.16, "npc"), y = unit(0.5, "npc"), just = c("left", "center"))
    ))

    # Use ggdraw to leave space at bottom for subtitle. The top sec_axis sits within the ggplot area.
    main_plot <- ggdraw() +
      draw_plot(p, x = 0, y = 0.08, width = 1, height = 0.92) +   # leave bottom margin for subtitle
      draw_grob(combined_subtitle, x = 0, y = 0, width = 1, height = 0.08)
    return(main_plot)
  }
}
# make_antigen_plate_bead <- function(data, specimen_type, analyte, title) {
#   plot_data <- data[data$specimen_type==specimen_type &
#                       data$analyte==analyte & data$N_wells > 0,]
#   if(nrow(plot_data) == 0) {
#     return(NULL)
#    # stop("No failing bead count for this combination of specimen type and analyte.")
#     } else {
#
#   type_levels <- levels(factor(plot_data$Type))
#   pal <- RColorBrewer::brewer.pal(n = max(3, length(type_levels)), name = "Set1")
#   type_colors <- setNames(pal[seq_along(type_levels)], type_levels)
#   p <- ggplot(plot_data, aes(x = plate, y = antigen, group = Type)) +
#     geom_text_repel(aes(label = N_wells, color = Type),
#                     min.segment.length = 0,
#                     box.padding = 0.25,
#                     point.padding = 0.35,
#                     max.overlaps = 20,
#                     force = 1) +
#     scale_color_manual(values = type_colors) +
#     theme_minimal() +
#     guides(color = "none", size = "none") +
#     labs(x = "Plate", y = "Antigen", title = title) +
#     scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +
#     scale_y_discrete(expand = expansion(mult = c(0.2, 0.2)))
#   n_types <- length(type_levels)
#   if (n_types == 1) {
#     xpos <- 0.5
#   } else {
#     xpos <- seq(0, 1, length.out = n_types + 1)[-1] - (0.5 / n_types)  # spread evenly but not flush to edges
#   }
#
#   subtitle_items <- lapply(seq_along(type_levels), function(i) {
#     textGrob(
#       label = type_levels[i],
#       x = unit(xpos[i], "npc"),
#       y = unit(0.5, "npc"),
#       just = "center",
#       gp = gpar(col = type_colors[type_levels[i]], fontsize = 11)
#     )
#   })
#   subtitle_row <- gTree(children = do.call(gList, subtitle_items))
#   prefix_grob <- textGrob("Types: ", x = unit(0.02, "npc"), y = unit(0.5, "npc"),
#                           just = "left", gp = gpar(col = "black", fontsize = 11))
#   combined_subtitle <- gTree(children = gList(
#     editGrob(prefix_grob, x = unit(0.02, "npc"), y = unit(0.5, "npc"), just = c("left", "center")),
#     editGrob(subtitle_row, x = unit(0.16, "npc"), y = unit(0.5, "npc"), just = c("left", "center"))
#   ))
#   main_plot <- ggdraw() +
#     draw_plot(p, x = 0, y = 0.08, width = 1, height = 0.92) +   # leave bottom margin for subtitle
#     draw_grob(combined_subtitle, x = 0, y = 0, width = 1, height = 0.08)
#   return(main_plot)
#
#     }
# }
