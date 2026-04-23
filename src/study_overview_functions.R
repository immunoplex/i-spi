## study overview functions

gmean <- function(x) {
  return(exp(mean(log(x))))
}
# Function to calculate geometric std deviation
gsd <- function(x) {
  return(exp(sd(log(x))))
}

gn <- function(x) {
  return(nrow(x))
}

# pull_standard <- function(conn, selected_study, current_user) {
#   standard_query <- glue::glue_sql("
#   SELECT DISTINCT study_accession, experiment_accession AS Analyte, plate_id, well, antigen,
#         		antibody_mfi AS MFI, antibody_n AS bead_count, pctaggbeads
#   	FROM madi_results.xmap_standard
#   	WHERE study_accession = {selected_study}
#   	ORDER BY experiment_accession, antigen, plate_id",
#                                    .con = conn)
#   standard_data <- dbGetQuery(conn, standard_query)
#   standard_data$plate_id <- str_trim(str_replace_all(standard_data$plate_id, "\\s", ""), side = "both")
#
#   return(standard_data)
# }
# pull_standard <- function(conn, selected_study, current_user, plates) {
#   standard_query <- glue::glue_sql("
#   SELECT DISTINCT s.study_accession, s.experiment_accession AS Analyte, s.plate_id, s.well, s.antigen,
#         		s.antibody_mfi AS MFI, s.antibody_n AS bead_count, s.pctaggbeads,
#         		CASE WHEN s.antibody_n < lower_bc_threshold THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
#             CASE WHEN s.pctaggbeads > pct_agg_threshold THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
#   	FROM madi_results.xmap_standard AS s
#   	     INNER JOIN (
#             SELECT study_accession, param_integer_value AS lower_bc_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold'
#   		    ) AS bct ON bct.study_accession = s.study_accession
#           INNER JOIN (
#             SELECT study_accession, param_integer_value AS pct_agg_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold'
#   		    ) AS pab ON pab.study_accession = s.study_accession
#   	WHERE s.study_accession = {selected_study}
#   	ORDER BY s.experiment_accession, s.antigen, s.plate_id",
#                                    .con = conn)
#   standard_data <- dbGetQuery(conn, standard_query)
#   standard_data$plate_id <- str_trim(str_replace_all(standard_data$plate_id, "\\s", ""), side = "both")
#
#   # new
#   standard_data$plate_id <- toupper(standard_data$plate_id)
#   standard_data <- merge(standard_data[ , ! names(standard_data) %in% c("analyte")], plates, by="plate_id", all.x = TRUE)
#   standard_data <- distinct(standard_data)
#
#   cat("NAMES from pulled standard")
#   print(names(standard_data))
#   return(standard_data)
# }

pull_standard <- function(conn, selected_study, current_user, plates) {
  standard_query <- glue::glue_sql("SELECT DISTINCT
         s.study_accession,
         s.experiment_accession,
		 (s.experiment_accession || '_' || h.nominal_sample_dilution) AS Analyte,
         --s.plate_id,
         h.xmap_header_id,
         s.source as std_source,
         h.plateid,
         h.plate,
         h.nominal_sample_dilution,
         s.feature,
         s.well,
         s.antigen,
         s.antibody_mfi                 AS MFI,
         s.antibody_n                   AS bead_count,
         s.pctaggbeads,
         /* QC flags that use the per‑study thresholds                */
         CASE WHEN s.antibody_n < bct.lower_bc_threshold
              THEN 'LowBeadN' ELSE 'Acceptable' END           AS lowbeadn,
         CASE WHEN s.pctaggbeads > pab.pct_agg_threshold
              THEN 'PctAggBeads' ELSE 'Acceptable' END       AS highbeadagg
  FROM   madi_results.xmap_standard AS s
  /* ----- lower‑bead‑count threshold (user‑specific) ----- */
  INNER JOIN (
          SELECT study_accession,
                 param_integer_value AS lower_bc_threshold
          FROM   madi_results.xmap_study_config
          WHERE  study_accession = {selected_study}
            AND  param_user      = {current_user}
            AND  param_name      = 'lower_bc_threshold'
        ) AS bct
          ON bct.study_accession = s.study_accession
  /* ----- %‑agg‑beads threshold (user‑specific) ----- */
  INNER JOIN (
          SELECT study_accession,
                 param_integer_value AS pct_agg_threshold
          FROM   madi_results.xmap_study_config
          WHERE  study_accession = {selected_study}
            AND  param_user      = {current_user}
            AND  param_name      = 'pct_agg_threshold'
        ) AS pab
          ON pab.study_accession = s.study_accession
  /* ----- Bring in the plate‑level header info ----- */
  INNER JOIN madi_results.xmap_header AS h
          ON h.study_accession      = s.study_accession
         AND h.experiment_accession = s.experiment_accession
         AND TRIM(h.plate_id)            = TRIM(s.plate_id)
  WHERE  s.study_accession = {selected_study}
  ORDER BY s.experiment_accession,
           s.antigen;", .con = conn)

  standard_data <- dbGetQuery(conn, standard_query)

  return(standard_data)

}

# pull_blank <- function(conn, selected_study, current_user, plates) {
#   buffer_query <- glue::glue_sql("
#   SELECT DISTINCT s.study_accession, s.experiment_accession AS Analyte, s.plate_id, s.well, s.antigen,
#         		s.antibody_mfi AS MFI, s.antibody_n AS bead_count, s.pctaggbeads,
#         		 CASE WHEN s.antibody_n < lower_bc_threshold THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
#             CASE WHEN s.pctaggbeads > pct_agg_threshold THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
#   	FROM madi_results.xmap_buffer as s
#   	     INNER JOIN (
#             SELECT study_accession, param_integer_value AS lower_bc_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold'
#   		    ) AS bct ON bct.study_accession = s.study_accession
#           INNER JOIN (
#             SELECT study_accession, param_integer_value AS pct_agg_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold'
#   		    ) AS pab ON pab.study_accession = s.study_accession
#   	WHERE s.study_accession = {selected_study}
#   	ORDER BY s.experiment_accession, s.antigen, s.plate_id",
#                                  .con = conn)
#   blank_data <- dbGetQuery(conn, buffer_query)
#   blank_data$plate_id <- str_trim(str_replace_all(blank_data$plate_id, "\\s", ""), side = "both")
#
#   # new
#   blank_data$plate_id <- toupper(blank_data$plate_id)
#   blank_data <- merge(blank_data[ , ! names(blank_data) %in% c("analyte")], plates, by="plate_id", all.x = TRUE)
#   blank_data <- distinct(blank_data)
#
#   return(blank_data)
# }

pull_blank <- function(conn, selected_study, current_user, plates) {
  buffer_query <- glue::glue_sql("SELECT DISTINCT
    b.study_accession,
    b.experiment_accession,
	(b.experiment_accession || '_' || h.nominal_sample_dilution) AS Analyte,
    --s.plate_id,
   -- h.xmap_header_id,
    h.plateid,
    h.plate,
    h.nominal_sample_dilution,
    b.well,
    b.experiment_accession as feature,
    b.antigen,
    b.antibody_mfi                  AS mfi,
    b.antibody_n                    AS bead_count,
    b.pctaggbeads,
    /* QC flags based on the per‑study thresholds */
    CASE WHEN b.antibody_n < bct.lower_bc_threshold
         THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
    CASE WHEN b.pctaggbeads > pab.pct_agg_threshold
         THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
FROM madi_results.xmap_buffer               AS b
/* ----- lower‑bead‑count threshold (user‑specific) ----- */
INNER JOIN (
    SELECT study_accession,
           param_integer_value AS lower_bc_threshold
    FROM   madi_results.xmap_study_config
    WHERE  study_accession = {selected_study}
      AND  param_user      = {current_user}
      AND  param_name      = 'lower_bc_threshold'
) AS bct
    ON bct.study_accession = b.study_accession
/* ----- %‑agg‑beads threshold (user‑specific) ----- */
INNER JOIN (
    SELECT study_accession,
           param_integer_value AS pct_agg_threshold
    FROM   madi_results.xmap_study_config
    WHERE  study_accession = {selected_study}
      AND  param_user      = {current_user}
      AND  param_name      = 'pct_agg_threshold'
) AS pab
    ON pab.study_accession = b.study_accession
/* ----- Header information for the plate ----- */
INNER JOIN madi_results.xmap_header AS h
    ON h.study_accession      = b.study_accession
   AND h.experiment_accession = b.experiment_accession
   AND TRIM(h.plate_id)            = TRIM(b.plate_id)
WHERE b.study_accession = {selected_study}
ORDER BY b.experiment_accession,
         b.antigen;
",.con = conn)

  blank_data <- dbGetQuery(conn, buffer_query)
  return(blank_data)

}
# pull_control <- function(conn, selected_study, current_user, plates){
#   control_query <- glue::glue_sql("
#   SELECT DISTINCT s.study_accession, s.experiment_accession AS Analyte, s.plate_id, s.well, s.antigen,
#         		s.antibody_mfi AS MFI, s.antibody_n AS bead_count, s.pctaggbeads,
#         		CASE WHEN s.antibody_n < lower_bc_threshold THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
#             CASE WHEN s.pctaggbeads > pct_agg_threshold THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
#   	FROM madi_results.xmap_control as s
#   	     INNER JOIN (
#             SELECT study_accession, param_integer_value AS lower_bc_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold'
#   		    ) AS bct ON bct.study_accession = s.study_accession
#           INNER JOIN (
#             SELECT study_accession, param_integer_value AS pct_agg_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold'
#   		    ) AS pab ON pab.study_accession = s.study_accession
#   	WHERE s.study_accession = {selected_study}
#   	ORDER BY s.experiment_accession, s.antigen, s.plate_id",
#                                   .con = conn)
#   control_data <- dbGetQuery(conn, control_query)
#   #control_data$plate_id <- str_trim(control_data$plate_id, side = "both")
#   control_data$plate_id <- str_trim(str_replace_all(control_data$plate_id, "\\s", ""), side = "both")
#
#   # new
#   control_data$plate_id <- toupper(control_data$plate_id)
#   control_data <- merge(control_data[ , ! names(control_data) %in% c("analyte")], plates, by="plate_id", all.x = TRUE)
#   control_data <- distinct(control_data)
#
#   return(control_data)
# }

pull_control <- function(conn, selected_study, current_user, plates) {
  control_query <- glue::glue_sql("SELECT DISTINCT
         c.study_accession,
		 c.experiment_accession,
         (c.experiment_accession || '_' || h.nominal_sample_dilution) AS Analyte,
         h.plateid,
         h.plate,
         h.nominal_sample_dilution,
         c.feature,
         c.well,
         c.antigen,
         c.antibody_mfi                 AS MFI,
         c.antibody_n                   AS bead_count,
         c.pctaggbeads,
         CASE WHEN c.antibody_n < bct.lower_bc_threshold
              THEN 'LowBeadN' ELSE 'Acceptable' END           AS lowbeadn,
         CASE WHEN c.pctaggbeads > pab.pct_agg_threshold
              THEN 'PctAggBeads' ELSE 'Acceptable' END       AS highbeadagg
  FROM   madi_results.xmap_control AS c
  /* lower‑bead‑count threshold (user‑specific) */
  INNER JOIN (
          SELECT study_accession,
                 param_integer_value AS lower_bc_threshold
          FROM   madi_results.xmap_study_config
          WHERE  study_accession = {selected_study}
            AND  param_user      = {current_user}
            AND  param_name      = 'lower_bc_threshold'
        ) AS bct
        ON bct.study_accession = c.study_accession
  /* %‑agg‑beads threshold (user‑specific) */
  INNER JOIN (
          SELECT study_accession,
                 param_integer_value AS pct_agg_threshold
          FROM   madi_results.xmap_study_config
          WHERE  study_accession = {selected_study}
            AND  param_user      = {current_user}
            AND  param_name      = 'pct_agg_threshold'
        ) AS pab
        ON pab.study_accession = c.study_accession
  /* Bring in the header to get nominal_sample_dilution */
  INNER JOIN madi_results.xmap_header AS h
          ON h.study_accession      = c.study_accession
         AND h.experiment_accession = c.experiment_accession
         AND TRIM(h.plate_id)            = TRIM(c.plate_id)
  WHERE  c.study_accession = {selected_study}
  ORDER BY c.experiment_accession,
           c.antigen;
           --c.plate_id;", .con = conn)

    control_data <- dbGetQuery(conn, control_query)
    return(control_data)

}
#
# pull_samples <- function(conn, selected_study, current_user, plates) {
#   select_query <- glue::glue_sql("
#       		SELECT DISTINCT xmap_sample.study_accession, experiment_accession, plate_id,
#       		  well, antigen, patientid, agroup, timeperiod,
#         		antibody_mfi AS MFI, antibody_au AS AU,
#         		dilution AS nominal_sample_dilution,
#         		CASE
#         		  WHEN gate_class IN ('Between_Limits','Acceptable') THEN 'Acceptable'
#               WHEN gate_class IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
#       		    WHEN gate_class IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
#               WHEN gate_class IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gclod,
#             CASE
#               WHEN gate_class_linear_region IN ('Between_Limits','Acceptable') THEN 'Acceptable'
#               WHEN gate_class_linear_region IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
#               WHEN gate_class_linear_region IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
#               WHEN gate_class_linear_region IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gclin,
#             CASE
#               WHEN gate_class_loq IN ('Between_Limits','Acceptable') THEN 'Acceptable'
#               WHEN gate_class_loq IN ('Below_Lower_Limit','Too Diluted') THEN 'Too Diluted'
#               WHEN gate_class_loq IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
#               WHEN gate_class_loq IN ('Not Evaluated') OR gate_class IS NULL THEN 'Not Evaluated' END AS gcloq,
#             CASE WHEN antibody_n < lower_bc_threshold THEN 'LowBeadN' ELSE 'Acceptable' END AS lowbeadn,
#             CASE WHEN pctaggbeads > pct_agg_threshold THEN 'PctAggBeads' ELSE 'Acceptable' END AS highbeadagg
#       		FROM madi_results.xmap_sample
#           INNER JOIN (
#             SELECT study_accession, param_integer_value AS lower_bc_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'lower_bc_threshold'
#   		    ) AS bct ON bct.study_accession = xmap_sample.study_accession
#           INNER JOIN (
#             SELECT study_accession, param_integer_value AS pct_agg_threshold
#             FROM madi_results.xmap_study_config
#     		    WHERE study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'pct_agg_threshold'
#   		    ) AS pab ON pab.study_accession = xmap_sample.study_accession
#   		    WHERE xmap_sample.study_accession = {selected_study}
#   ",
#                                  .con = conn)
#   # antibody_n AS bead_count, lower_bc_threshold,
#   # pctaggbeads, pct_agg_threshold,
#   active_samples <- dbGetQuery(conn, select_query)
#   active_samples$analyte <- factor(active_samples$experiment_accession)
#   active_samples$plate_id <- str_trim(str_replace_all(active_samples$plate_id, "\\s", ""), side = "both")
#
#   ## new
#   active_samples$plate_id <- toupper(active_samples$plate_id)
#   active_samples <- merge(active_samples[ , ! names(active_samples) %in% c("analyte")], plates[ , ! names(plates) %in% c("nominal_sample_dilution")], by="plate_id", all.x = TRUE)
#   active_samples <- distinct(active_samples)
#   return(active_samples)
# }

### depends on the fitting - gating of samples
pull_samples <- function(conn, selected_study, current_user, plates) {
  sample_query <- glue::glue_sql("/* -------------------------------------------------------------
     1. Pull the study‑specific thresholds once (single scan)
   ------------------------------------------------------------- */
WITH study_config AS (
    SELECT
        study_accession,
        MAX(CASE WHEN param_name = 'lower_bc_threshold' THEN param_integer_value END) AS lower_bc_threshold,
        MAX(CASE WHEN param_name = 'pct_agg_threshold'   THEN param_integer_value END) AS pct_agg_threshold
    FROM madi_results.xmap_study_config
    WHERE param_user = {current_user}
      AND param_name IN ('lower_bc_threshold','pct_agg_threshold')
    GROUP BY study_accession
)

/* -------------------------------------------------------------
   2  Main query – everything you need, plus derived flags
   ------------------------------------------------------------- */
SELECT
    b.best_sample_se_all_id,
   -- b.raw_predicted_concentration as predicted_concentration,
    b.study_accession,
    b.experiment_accession,
    b.timeperiod,
    b.patientid,
    b.well,
    b.stype,
    b.sampleid,
    b.agroup,
    b.pctaggbeads,
    b.samplingerrors,
    b.antigen,
    b.antibody_n,
    b.plateid,
    b.plate,
    b.nominal_sample_dilution,

    /* NEW – columns already in best_sample_se_all */
    b.feature,               -- bead‑/antigen‑level feature
    (b.experiment_accession || '_' || b.nominal_sample_dilution) AS Analyte,

    b.assay_response_variable,
    b.assay_independent_variable,
    b.dilution,
    b.overall_se,
    b.assay_response        AS MFI,
    b.se_concentration,
    --b.final_predicted_concentration AS concentration,
    b.pcov,
    b.source,
    b.gate_class_loq,
    b.gate_class_lod,
    b.gate_class_pcov,
    b.norm_assay_response,
    b.best_glance_all_id,

    /* ---- Derived flag columns ----------------------------------- */
    CASE
        WHEN b.gate_class_lod IN ('Between_Limits','Acceptable')          THEN 'Acceptable'
        WHEN b.gate_class_lod IN ('Below_Lower_Limit','Too Diluted')      THEN 'Too Diluted'
        WHEN b.gate_class_lod IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
        ELSE 'Not Evaluated'
    END AS gclod,

    CASE
        WHEN b.gate_class_loq IN ('Between_Limits','Acceptable')          THEN 'Acceptable'
        WHEN b.gate_class_loq IN ('Below_Lower_Limit','Too Diluted')      THEN 'Too Diluted'
        WHEN b.gate_class_loq IN ('Above_Upper_Limit','Too Concentrated') THEN 'Too Concentrated'
        ELSE 'Not Evaluated'
    END AS gcloq,

    CASE
        WHEN b.antibody_n < cfg.lower_bc_threshold THEN 'LowBeadN'
        ELSE 'Acceptable'
    END AS lowbeadn,

    CASE
        WHEN b.pctaggbeads > cfg.pct_agg_threshold THEN 'PctAggBeads'
        ELSE 'Acceptable'
    END AS highbeadagg

FROM madi_results.best_sample_se_all AS b               -- alias defined here
INNER JOIN study_config AS cfg
        ON cfg.study_accession = b.study_accession
WHERE b.study_accession = {selected_study};", .con = conn)

active_samples <- dbGetQuery(conn,sample_query)

return(active_samples)
}

pull_raw_samples <- function(conn, selected_study, current_user) {
  raw_sample_query <- glue::glue_sql("WITH study_config AS (
    SELECT
        study_accession,
        MAX(CASE WHEN param_name = 'lower_bc_threshold' THEN param_integer_value END) AS lower_bc_threshold,
        MAX(CASE WHEN param_name = 'pct_agg_threshold'   THEN param_integer_value END) AS pct_agg_threshold
    FROM madi_results.xmap_study_config
    WHERE param_user = {current_user}
      AND param_name IN ('lower_bc_threshold','pct_agg_threshold')
    GROUP BY study_accession
)
SELECT DISTINCT
    s.study_accession,
    s.experiment_accession,
    (s.experiment_accession || '_' || h.nominal_sample_dilution) AS analyte,
    h.plateid,
    h.plate,
    h.nominal_sample_dilution,
    s.feature,
    s.well,
    s.antigen,
    s.patientid,
    s.agroup,
    s.timeperiod,
    s.antibody_mfi AS MFI,

    CASE
        WHEN s.antibody_n < cfg.lower_bc_threshold THEN 'LowBeadN'
        ELSE 'Acceptable'
    END AS lowbeadn,

    CASE
        WHEN s.pctaggbeads > cfg.pct_agg_threshold THEN 'PctAggBeads'
        ELSE 'Acceptable'
    END AS highbeadagg

FROM madi_results.xmap_sample AS s

INNER JOIN madi_results.xmap_header AS h
    ON h.study_accession      = s.study_accession
   AND h.experiment_accession = s.experiment_accession
   AND TRIM(h.plate_id)             = TRIM(s.plate_id)

INNER JOIN study_config AS cfg
    ON cfg.study_accession = s.study_accession

WHERE s.study_accession = {selected_study};
",.con = conn)

raw_samples <- dbGetQuery(conn, raw_sample_query)
return(raw_samples)
}


pull_conc <- function(conn, selected_study, current_user){
  query_stdcurve_conc <- glue::glue_sql("SELECT antigen, antigen_family, standard_curve_concentration
	FROM madi_results.xmap_antigen_family
	WHERE study_accession = {selected_study};", .con = conn)
  stdcurve_undiluted_conc <- dbGetQuery(conn, query_stdcurve_conc)
  stdcurve_undiluted_conc$standard_curve_concentration <- as.numeric(stdcurve_undiluted_conc[["standard_curve_concentration"]])
  return(stdcurve_undiluted_conc)
}

# pull_fits <- function(conn, selected_study, current_user, plates) {
# #   fit_query <- glue::glue_sql("
# # SELECT experiment_accession AS analyte, antigen, plateid,
# # bkg_method AS buffer_treatment, is_log_mfi_axis AS logMFI, crit, cv, llod, ulod,
# # bendlower AS llin, bendupper AS ulin, lloq, uloq, l_asy, r_asy, x_mid, scale, g
# # 	FROM madi_results.xmap_standard_fits
# # 	INNER JOIN madi_results.xmap_study_config ON xmap_standard_fits.study_accession = xmap_study_config.study_accession
# # 	WHERE xmap_standard_fits.study_accession = {selected_study} AND param_user = {current_user} AND param_name = 'default_source' AND source = param_character_value
# # 	ORDER BY experiment_accession, antigen, plateid",
# #                               .con = conn)
#  fit_query <- glue::glue_sql("SELECT
#   experiment_accession AS analyte,
#   antigen,
#   plateid,
#   bkg_method AS buffer_treatment,
#   is_log_mfi_axis AS logMFI,
#   crit, cv, llod, ulod,
#   bendlower AS llin,
#   bendupper AS ulin,
#   lloq, uloq,
#   l_asy, r_asy, x_mid, scale, g, source
#   FROM madi_results.xmap_standard_fits sf
#
#   -- Join for default source
#  -- INNER JOIN madi_results.xmap_study_config cfg_source
#   --ON sf.study_accession = cfg_source.study_accession
#   -- AND cfg_source.param_user = {current_user}
#   -- AND cfg_source.param_name = 'default_source'
#   --  AND sf.source = cfg_source.param_character_value
#
#   -- Join for buffer treatment
#   INNER JOIN madi_results.xmap_study_config cfg_buffer
#   ON sf.study_accession = cfg_buffer.study_accession
#   AND cfg_buffer.param_user = {current_user}
#   AND cfg_buffer.param_name = 'blank_option'
#   AND sf.bkg_method = cfg_buffer.param_character_value
#
#   -- Join for log MFI axis (using boolean value)
#   INNER JOIN madi_results.xmap_study_config cfg_logmfi
#   ON sf.study_accession = cfg_logmfi.study_accession
#   AND cfg_logmfi.param_user = {current_user}
#   AND cfg_logmfi.param_name = 'is_log_mfi_axis'
#   AND sf.is_log_mfi_axis = cfg_logmfi.param_boolean_value
#
#   WHERE sf.study_accession = {selected_study}
#
#   ORDER BY experiment_accession, antigen, plateid;", .con = conn)
#
#   standard_fit <- dbGetQuery(conn, fit_query)
#   # standard_fit_1 <<- standard_fit
#   # plates_v <<- plates
#
#   ## If it is in the form plate.num remove the .
#   standard_fit$plateid <- sub("\\.plate\\.(\\d+)", ".plate\\1", standard_fit$plateid)
#
#   standard_fit$plateid <- str_replace_all(standard_fit$plateid, fixed(".."),"_")
#   standard_fit$plateid <- str_replace_all(standard_fit$plateid, fixed("."),"_")
#   #standard_fit_v <<- standard_fit
#
#   standard_fit <- merge(standard_fit[ , ! names(standard_fit) %in% c("analyte")], plates, by = "plateid", all.y = TRUE)
#   standard_fit$plate_id <- toupper(standard_fit$plate_id)
#   names(standard_fit)[names(standard_fit) == "l_asy"] <- "a"
#   names(standard_fit)[names(standard_fit) == "r_asy"] <- "d"
#   names(standard_fit)[names(standard_fit) == "x_mid"] <- "c"
#   names(standard_fit)[names(standard_fit) == "scale"] <- "b"
#   standard_fit$g <- ifelse(is.na(standard_fit$g), 1, standard_fit$g)
#   unique(standard_fit$crit)
#
#   #standard_fit_f <<- standard_fit
#
#   standard_fit <- standard_fit %>% distinct()
#
#   return(standard_fit)
# }

pull_fits <- function(conn, selected_study, current_user, plates) {
  fit_query <- glue::glue_sql("SELECT
  sf.study_accession,
  sf.experiment_accession,
  nominal_sample_dilution,
  (sf.experiment_accession || '_' || nominal_sample_dilution) AS Analyte,
  antigen,
  plateid, plate,
  bkg_method AS buffer_treatment,
  is_log_response AS logMFI,
  crit, cv, llod, ulod,
  lloq_y AS llin,
  uloq_y AS ulin,
  lloq, uloq,
  a, b, c, d, g, source
  FROM madi_results.best_glance_all sf
  -- Join for buffer treatment
  INNER JOIN madi_results.xmap_study_config cfg_buffer
  ON sf.study_accession = cfg_buffer.study_accession
  AND cfg_buffer.param_user = {current_user}
  AND cfg_buffer.param_name = 'blank_option'
  AND sf.bkg_method = cfg_buffer.param_character_value

  -- Join for log MFI axis (using boolean value)
  INNER JOIN madi_results.xmap_study_config cfg_logmfi
  ON sf.study_accession = cfg_logmfi.study_accession
  AND cfg_logmfi.param_user = {current_user}
  AND cfg_logmfi.param_name = 'is_log_mfi_axis'
  AND sf.is_log_response = cfg_logmfi.param_boolean_value

  WHERE sf.study_accession = {selected_study}

  ORDER BY experiment_accession, antigen, plateid;
", .con = conn)
  standard_fits <- dbGetQuery(conn, fit_query)
  return(standard_fits)
}

# function to summarize data with gmean, gsd and count (n)
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

summarise_by_plate_id <- function(df) {
  if (nrow(df) > 2)
{  dfsum <- df %>%
    group_by(analyte, antigen, plateid) %>%
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
interplate_summarize <- function(df) {
  if (nrow(df) > 2)
  {
  dfsum <- df %>%
    group_by(analyte, antigen) %>%
    dplyr::summarise(
      gmean = gmean(mp_mfi),
      gsd = gsd(mp_mfi),
      n = dplyr::n(),
      interplate_cv_mfi = (sd(mp_mfi)/mean(mp_mfi)) * 100,
      .groups = "drop"
    )} else {
      dfsum <- data.frame()
    }
  return(dfsum)
}

# Further summarise counts for specific conditions within active_samples
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

check_plate <- function(conn, selected_study) {
  query_plate <- glue::glue_sql("SELECT best_plate_all_id, study_accession,
  experiment_accession, feature, source, plateid, plate,
  nominal_sample_dilution, assay_response_variable, assay_independent_variable
	FROM madi_results.best_plate_all
  WHERE study_accession = {selected_study};", .con = conn)

  plates <- dbGetQuery(conn, query_plate)
  plates$analyte <- paste(plates$experiment_accession,plates$nominal_sample_dilution,sep = "_")
  #plates$plate_id <- toupper(plates$plate_id)

  plates <- plates[ , c("plateid", "plate", "feature", "analyte", "nominal_sample_dilution")]

  return(plates)
}

# load_specimens <- function(current_user, selected_study) {
#   #conn <- get_db_connection()
#   standard_data <- pull_standard(conn, selected_study, current_user)
#   standard_data$specimen_type <- "standard"
#   blank_data <- pull_blank(conn, selected_study, current_user)
#   blank_data$specimen_type <- "blank"
#   control_data <- pull_control(conn, selected_study, current_user)
#   control_data$specimen_type <- "control"
#   active_samples <- pull_samples(conn, selected_study, current_user)
#   active_samples$specimen_type <- "sample"
#
#   #dbDisconnect(conn)
#   return(list(standard_data, blank_data, control_data, active_samples))
# }

load_specimens <- function(conn, current_user, selected_study) {
  #conn <- get_db_connection()
  plates <- check_plate(conn = conn, selected_study = selected_study)
  standard_data <- pull_standard(conn, selected_study, current_user, plates)
  print(names(standard_data))
  standard_data$specimen_type <- "standard"
  blank_data <- pull_blank(conn, selected_study, current_user, plates)
  if (nrow(blank_data) > 1) {blank_data$specimen_type <- "blank"} else {
    blank_data <- data.frame()
  }
  control_data <- pull_control(conn, selected_study, current_user, plates = plates)
  if (nrow(control_data) > 1) {control_data$specimen_type <- "control"} else {
    control_data <- data.frame()
  }
  sample_data <- pull_samples(conn, selected_study, current_user, plates)
  if (nrow(sample_data) > 1) {sample_data$specimen_type <- "sample"} else {
    sample_data <- data.frame()
  }
 # plates_v <<- plates
  standard_fit <- pull_fits(conn, selected_study, current_user, plates)
  stdcurve_undiluted_conc <- pull_conc(conn, selected_study, current_user)

  raw_samples <- pull_raw_samples(conn, selected_study, current_user)
  if (nrow(raw_samples) > 1) {raw_samples$specimen_type <- "raw_sample"} else {
    raw_samples <- data.frame()
  }
  #dbDisconnect(conn)
  return(list(plates, standard_data, blank_data, sample_data, control_data, standard_fit,
              stdcurve_undiluted_conc,
              raw_samples))
}

# add_condition_counts <- function(data, summ) {
#   lowbead    <- get_condition_counts(data, "lowbeadn",  "LowBeadN",       "nlowbead",   summ)
#   highbead   <- get_condition_counts(data, "highbeadagg","PctAggBeads",   "nhighbeadagg", summ)
#   gclin_lin  <- get_condition_counts(data, "gclin",     "Acceptable",     "nlinear",    summ)
#   gclin_conc <- get_condition_counts(data, "gclin",     "Too Concentrated","ntooconc",  summ)
#   gclin_dil  <- get_condition_counts(data, "gclin",     "Too Diluted",    "ntoodilut",  summ)
#   gclod_conc <- get_condition_counts(data, "gclod",     "Too Concentrated","nabovelod", summ)
#   gclod_dil  <- get_condition_counts(data, "gclod",     "Too Diluted",    "nbelowlod",  summ)
#
#   summ %>%
#     left_join(lowbead,    by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(highbead,   by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(gclin_lin,  by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(gclin_conc, by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(gclin_dil,  by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(gclod_conc, by = c("analyte", "antigen", "plate_id")) %>%
#     left_join(gclod_dil,  by = c("analyte", "antigen", "plate_id")) %>%
#     replace_na(list(
#       nlinear      = 0,
#       nhighbeadagg = 0,
#       nlowbead     = 0,
#       ntooconc     = 0,
#       ntoodilut    = 0,
#       nabovelod    = 0,
#       nbelowlod    = 0
#     ))
# }

# make_summspec <- function(standard_data, blank_data, control_data, active_samples, low_bead_data, high_agg_bead_data, plates) {
#   buffer_summ   <- summarise_data(blank_data)  %>%
#                     mutate(specimen_type = "blank") %>%
#                     add_condition_counts(blank_data, .)
#   control_summ  <- summarise_data(control_data)  %>%
#                     mutate(specimen_type = "control") %>%
#                       add_condition_counts(control_data, .)
#   standard_summ <- summarise_data(standard_data) %>%
#                         mutate(specimen_type = "standard") %>%
#                         add_condition_counts(standard_data, .)
#   sample_summ   <- summarise_data(active_samples) %>%
#                       mutate(specimen_type = "sample") %>%
#                       add_condition_counts(active_samples, .)
#
#   low_bead_summ  <- summarise_data(low_bead_data) %>%
#                           mutate(specimen_type = "low_bead_count")
#   high_agg_bead_summ <- summarise_data(high_agg_bead_data) %>%
#                            mutate(specimen_type = "high_aggregate_beads")
#
#   summ_spec <- bind_rows(buffer_summ, control_summ, standard_summ, sample_summ,
#                          low_bead_summ, high_agg_bead_summ)
#
#   summ_spec$plate_id <- toupper(summ_spec$plate_id)
#     plates$plate_id <- toupper(plates$plate_id)
#     summ_spec <- merge(summ_spec, plates, by="plate_id", all.x = TRUE)
#
#     cat("Sum Spec:\n")
#     print(head(summ_spec))
#     if ("analyte.y" %in% names(summ_spec)) {
#       names(summ_spec)[names(summ_spec) == "analyte.y"] <- "analyte"
#     }
#     return(summ_spec)
#
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

make_interplate_summ_spec <- function(summ_spec) {
  interplate_summ_spec <- interplate_summarize(summ_spec)
  return(interplate_summ_spec)
}

pivot_by_plate <- function(df, value_col) {
  df %>%
    select(analyte, antigen, plateid, all_of(value_col)) %>%
    pivot_wider(names_from = plateid, values_from = all_of(value_col), values_fill = 0)
}

pivot_sample_col <- function(df,colname) {
  df %>%
    select(analyte, antigen, plateid, all_of(colname)) %>%
    pivot_wider(names_from = plateid, values_from = all_of(colname), values_fill = 0)
}

# download report
report_vars <- function(summ_spec) {
  summ_spec <- distinct(summ_spec, analyte, antigen, plate, specimen_type, .keep_all = TRUE)
  summ_spec$nlowbead <- ifelse(is.na(summ_spec$nlowbead),0,summ_spec$nlowbead)
  summ_spec$nlinear <- ifelse(is.na(summ_spec$nlinear),0,summ_spec$nlinear)
  summ_spec$nhighbeadagg <- ifelse(is.na(summ_spec$nhighbeadagg),0,summ_spec$nhighbeadagg)
  summ_spec$ntooconc <- ifelse(is.na(summ_spec$ntooconc),0,summ_spec$ntooconc)
  summ_spec$ntoodilut <- ifelse(is.na(summ_spec$ntoodilut),0,summ_spec$ntoodilut)
  summ_spec$nabovelod <- ifelse(is.na(summ_spec$nabovelod),0,summ_spec$nabovelod)
  summ_spec$nbelowlod <- ifelse(is.na(summ_spec$nbelowlod),0,summ_spec$nbelowlod)
  summ_spec$pct_lin <- ifelse(summ_spec$n > 0, round(summ_spec$nlinear / summ_spec$n * 100, digits = 0), NULL)

  return(summ_spec)
}

convert_vars <- function(summ_spec) {
  x_vals <- seq(-5, 0, length.out = 1000)
  summ_spec_dup <- distinct(summ_spec, analyte, antigen, plate, specimen_type, .keep_all = TRUE)
  cat("summ spec_dup/n")
 print(head(summ_spec_dup))
  # print(unique(summ_spec_dup$plateid))
  sample_spec <- summ_spec_dup[summ_spec_dup$specimen_type=='sample', ]
  cat("sample spec\n")
  sample_spec$nlowbead <- ifelse(is.na(sample_spec$nlowbead),0,sample_spec$nlowbead)
  #sample_spec$nlinear <- ifelse(is.na(sample_spec$nlinear),0,sample_spec$nlinear)
  sample_spec$nhighbeadagg <- ifelse(is.na(sample_spec$nhighbeadagg),0,sample_spec$nhighbeadagg)
  # sample_spec$ntooconc <- ifelse(is.na(sample_spec$ntooconc),0,sample_spec$ntooconc)
  # sample_spec$ntoodilut <- ifelse(is.na(sample_spec$ntoodilut),0,sample_spec$ntoodilut)
  # sample_spec$nabovelod <- ifelse(is.na(sample_spec$nabovelod),0,sample_spec$nabovelod)
  # sample_spec$nbelowlod <- ifelse(is.na(sample_spec$nbelowlod),0,sample_spec$nbelowlod)
  # sample_spec$pct_lin <- ifelse(sample_spec$n > 0, round(sample_spec$nlinear / sample_spec$n * 100, digits = 0), NULL)

 # sample_spec$analyte <- paste(sample_spec$analyte, sample_spec$nominal_sample_dilution, sep = "_")
  print(head(sample_spec))

  # sample_spec$plaque_info <- str_extract(sample_spec$plateid, regex("plaque[_]?\\d+[a-zA-Z]*", ignore_case = TRUE))
  # sample_spec$plate_number <- str_replace(sample_spec$plaque_info, regex("(?i)plaque[_]?", ""), "")
  # if (all(sample_spec$plate == "plate_")) {
  #   sample_spec$plate <- paste0(sample_spec$plate, sample_spec$plate_number)
  # }

 filter_res <- summ_spec_dup %>%
    dplyr::summarise(n = dplyr::n(), .by = c(analyte, antigen, plate, nominal_sample_dilution, specimen_type)) |>
    dplyr::filter(n > 1L)

 cat("duplicated\n")
 print(filter_res)

  tsumm_spec <- pivot_wider(summ_spec_dup, id_cols = c("analyte", "antigen", "plate", "nominal_sample_dilution"), names_from = "specimen_type", values_from = "n")
  cat("after tsum pivot")

  if ("sample" %in% names(tsumm_spec)) {
    tsumm_spec$sample <- ifelse(is.na(tsumm_spec$sample), 0, tsumm_spec$sample)
    tsample <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "sample"))
  } else {tsample <- data.frame()}

  if ("standard" %in% names(tsumm_spec)) {
    tsumm_spec$standard <- ifelse(is.na(tsumm_spec$standard), 0, tsumm_spec$standard)
    tstandard <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "standard"))
  } else {tstandard <- data.frame()}

  if ("control" %in% names(tsumm_spec)) {
    tsumm_spec$control <- ifelse(is.na(tsumm_spec$control), 0, tsumm_spec$control)
    tcontrol <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "control"))
  } else {tcontrol <- data.frame()}

  if ("blank" %in% names(tsumm_spec)) {
    tsumm_spec$blank <- ifelse(is.na(tsumm_spec$blank), 0, tsumm_spec$blank)
    tblank <- as.data.frame(pivot_wider(tsumm_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "blank"))
  } else {tblank <- data.frame()}

  #tsumm_spec$analyte <- paste(tsumm_spec$analyte, tsumm_spec$nominal_sample_dilution, sep = "_")

  tsample_lin <- data.frame()
  #tsample_lin <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "pct_lin"))

  tsample_lobead <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nlowbead"))
  tsample_hiagg <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nhighbeadagg"))
  # tsample_conc <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "ntooconc"))
  # tsample_dilut <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "ntoodilut"))
  # tsample_abovelod <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nabovelod"))
  # tsample_belowlod <- as.data.frame(pivot_wider(sample_spec, id_cols = c("analyte", "antigen"), names_from = "plate", values_from = "nbelowlod"))
  return(list(tsample, tstandard, tblank, tcontrol, tsample_lin, tsample_lobead, tsample_hiagg)) # tsample_conc, tsample_dilut, tsample_abovelod, tsample_belowlod))
}

get_bg_color <- function(pctlin) {
  norm_val <- pctlin / 100
  colors <- viridisLite::viridis(100, begin=0.01, end=0.95, option = "E")
  colors[ceiling(norm_val * 99) + 1]
}

select_safe <- function(df, cols) {
  existing <- intersect(cols, names(df))
  if (length(existing) == 0) {
    tibble::tibble()
  } else {
    df %>% select(all_of(existing))
  }
}

# create low bead and high aggregate data frames for any specimen type
make_problem_sets <- function(df, needed_cols) {
  # -----------------------------------------------------------------
  # 1) Low‑bead rows
  # -----------------------------------------------------------------
  low_bead <- df %>%
    filter(lowbeadn == "LowBeadN") %>%
    #select_safe(needed_cols) %>%
    mutate(problem_type = "low_bead_count")

  # -----------------------------------------------------------------
  # 2) High‑aggregate rows
  # -----------------------------------------------------------------
  high_agg <- df %>%
    filter(highbeadagg == "PctAggBeads") %>%
   # select_safe(needed_cols) %>%
    mutate(problem_type = "high_aggregate_beads")

  # -----------------------------------------------------------------
  # Return a list with the two tables
  # -----------------------------------------------------------------
  return(list(low = low_bead, high = high_agg))
}

make_make_problem_sets_if_not_empty <- function(df, name, needed_cols) {
  if (nrow(df) > 0) {
    cat("-> processing", name, "\n")
    make_problem_sets(df, needed_cols)
  } else {
    cat("->", name, "has 0 rows – skipping\n")
    NULL
  }

}


preprocess_plate_data <- function(conn, current_user, selected_study) {
  cat("\n=== preprocess_plate_data start ===\n")

  ## ------------------------------------------------------------
  ## 1) Load everything (using the unchanged load_specimens())
  ## ------------------------------------------------------------
  loaded <- load_specimens(conn, current_user, selected_study)

  print(str(loaded[[2]]))
  ## ------------------------------------------------------------
  ## 2) Unpack the list (convert each element to a tibble for safety)
  ## ------------------------------------------------------------
#  plates                 <- as_tibble(loaded[[1]])
  plates <- check_plate(conn = conn, selected_study = selected_study)
  standard_data          <- as_tibble(loaded[[2]])
  cat("aftter standard")
  blank_data             <- as_tibble(loaded[[3]])
  cat("after_blank")
  sample_data            <- as_tibble(loaded[[4]])   # may be empty
  cat("after sample")
  control_data           <- as_tibble(loaded[[5]])
  cat("after control")
  #print(str(loaded[[6]]))
  standard_fit_data      <- as_tibble(loaded[[6]])   # not used here but returned later
  cat("after standard fit")
  stdcurve_undiluted_conc <- as_tibble(loaded[[7]]) # not used here but returned later
  cat("after undiluted_conc")
  raw_samples            <- as_tibble(loaded[[8]])   # always present
  cat("after samples")

  cat("after loading specimens\n")

  ## ------------------------------------------------------------
  ## 3) Columns we need for the low‑bead / high‑aggregate extraction
  ## ------------------------------------------------------------
  needed_cols <- c(
    "study_accession","plateid","plate","analyte","antigen","mfi",
    "specimen_type","nominal_sample_dilution","feature",
    "lowbeadn","highbeadagg"
  )

  ## ------------------------------------------------------------
  ## 4) Build low‑bead / high‑aggregate tables for EVERY specimen type
  ## ------------------------------------------------------------
  # problem_lists <- list(
  #   raw_samples   = make_problem_sets(raw_samples,   needed_cols),
  #   standard_data = make_problem_sets(standard_data, needed_cols),
  #   blank_data    = make_problem_sets(blank_data,    needed_cols),
  #   control_data  = make_problem_sets(control_data,  needed_cols),
  #   sample_data   = make_problem_sets(sample_data,   needed_cols)   # may be empty
  # )
  cat("active sample data procccessing row:")
  print(nrow(sample_data))
  print(nrow(control_data))

 # raw_samples <<- raw_samples

  problem_lists <- purrr::compact(list(
    raw_samples   = { cat("-> processing raw_samples\n");   make_make_problem_sets_if_not_empty(raw_samples,  "raw_samples",  needed_cols) },
    standard_data = { cat("-> processing standard_data\n"); make_make_problem_sets_if_not_empty(standard_data, "standard_data", needed_cols) },
    blank_data    = { cat("-> processing blank_data\n");    make_make_problem_sets_if_not_empty(blank_data, "blank_data", needed_cols) },
    control_data  = { cat("-> processing control_data\n");  make_make_problem_sets_if_not_empty(control_data, "control_data", needed_cols) }
   # sample_data   = { cat("-> processing sample_data\n");   make_problem_sets(sample_data,   needed_cols) }
  ))

  print(problem_lists)

 # problem_lists <<- problem_lists

  low_bead_data  <- dplyr::bind_rows(purrr::map(problem_lists, "low"))
  high_agg_data  <- dplyr::bind_rows(purrr::map(problem_lists, "high"))

  cat("Low‑bead rows   :", nrow(low_bead_data), "\n")
  cat("High‑agg rows   :", nrow(high_agg_data), "\n")

  ## ------------------------------------------------------------
  ## 5) Create the master summary table
  ## ------------------------------------------------------------
  summ_spec <- make_summspec(
    standard = standard_data,
    blank    = blank_data,
    control  = control_data,
    raw      = raw_samples, # pre-QC raw samples
    low_bead = low_bead_data,
    high_agg = high_agg_data,
    active_samples = sample_data, # QC samples after curve fit
    plates   = plates
  )

  ## ------------------------------------------------------------
  ## 6) Add an explicit ordering factor (so downstream sorts are deterministic)
  ## ------------------------------------------------------------
  summ_spec$specimen_type_order <- case_when(
    summ_spec$specimen_type == "blank" ~ 1,
    summ_spec$specimen_type == "control" ~ 2,
    summ_spec$specimen_type == "standard" ~ 3,
    summ_spec$specimen_type == "sample" ~ 4,
    summ_spec$specimen_type == "low_bead_count" ~5,
    summ_spec$specimen_type == "high_aggregate_beads" ~6,
    summ_spec$specimen_type == "raw_sample" ~7,
    TRUE ~ 0
  )
  # summ_spec <- summ_spec %>%
  #   mutate(
  #     specimen_type_order = factor(
  #       specimen_type,
  #       levels = c(
  #         "blank", "control", "standard", "sample",
  #         "raw_sample", "low_bead_count", "high_aggregate_beads"
  #       ),
  #       ordered = TRUE
  #     )
  #   )

  ## ------------------------------------------------------------
  ## 7) Convert variables to their final types (you already have a helper)
  ## ------------------------------------------------------------
  cat("before count set")
  print(names(summ_spec))
  count_set <- convert_vars(summ_spec)

  ## ------------------------------------------------------------
  ## 8) Tweak the plates table exactly as you did before
  ## ------------------------------------------------------------
  plates <- plates %>%
    mutate(plate = paste(plate, nominal_sample_dilution, sep = "_"))

  # ## ------------------------------------------------------------
  # ## 9) (Optional) expose the summary globally – keep only if you really need it
  # ## ------------------------------------------------------------
  # assign("summ_spec_v", summ_spec, envir = .GlobalEnv)

  cat("=== preprocess_plate_data finished ===\n\n")

  ## ------------------------------------------------------------
  ## 10) Return the objects you asked for
  ## ------------------------------------------------------------
  return(list(
    count_set          = count_set,
    plates             = plates,
    raw_samples        = raw_samples,
    summ_spec          = summ_spec,
    standard_fit_data = standard_fit_data,
    active_samples    = sample_data          # may be an empty tibble
  ))
}


# preprocess_plate_data <- function(conn, current_user, selected_study){
#   plates <- check_plate(conn = conn, selected_study = selected_study)
#   cat("before load specimens\n")
#   loaded_data <<- load_specimens(conn, current_user, selected_study)
#   #return(list(plates, standard_data, blank_data, sample_data, control_data, standard_fit, stdcurve_undiluted_conc))
#   cat("after loading specimens\n")
#   standard_data <- as.data.frame(loaded_data[[2]]) # shift it up 1 index
#   blank_data <- as.data.frame(loaded_data[[3]])
#   active_samples <- as.data.frame(loaded_data[[4]])
#   control_data <- as.data.frame(loaded_data[[5]])
#   standard_fit_data <- as.data.frame(loaded_data[[6]])
#   # raw samples to analyze up to fit quality
#   raw_samples <- as.data.frame(loaded_data[[8]])
#
#
#   missing_cols <- setdiff(c("study_accession","plateid", "plate","analyte", "antigen", "mfi",
#                             "specimen_type", "nominal_sample_dilution", "feature","lowbeadn", "highbeadagg"), names(raw_samples))
#
#   print("missing cols:\n")
#   print(missing_cols)
#
#   if(nrow(raw_samples) > 1){
#     low_bead_sample <- raw_samples[raw_samples$lowbeadn == "LowBeadN",
#                        c("study_accession","plateid", "plate","analyte", "antigen", "mfi",
#                        "specimen_type", "nominal_sample_dilution", "feature","lowbeadn", "highbeadagg")]
#
#     high_agg_bead_sample <- raw_samples[raw_samples$highbeadagg == "PctAggBeads",
#                             c("study_accession", "plateid", "plate", "analyte", "antigen","mfi",
#                             "specimen_type", "nominal_sample_dilution", "feature","lowbeadn", "highbeadagg")]
#   } else {
#     low_bead_sample <- data.frame()
#     high_agg_bead_sample <- data.frame()
#   }
#
#   cat("after samples")
#   if(nrow(standard_data) > 1){
#     low_bead_standard <- standard_data[standard_data$lowbeadn == "LowBeadN",
#                          c("study_accession", "plateid", "plate", "analyte", "antigen", "mfi", "specimen_type",
#                          "nominal_sample_dilution", "feature", "lowbeadn", "highbeadagg")]
#     high_agg_bead_standard <- standard_data[standard_data$highbeadagg == "PctAggBeads",
#                               c("study_accession", "plateid", "plate", "analyte", "antigen","mfi",
#                               "specimen_type", "nominal_sample_dilution", "feature", "lowbeadn", "highbeadagg")]
#   } else {
#     low_bead_standard <- data.frame()
#     high_agg_bead_standard <- data.frame()
#   }
#   print("after standards")
#   missing_cols2 <- setdiff(c("study_accession","plateid", "plate","analyte", "antigen", "mfi",
#                            "specimen_type", "nominal_sample_dilution", "feature","lowbeadn", "highbeadagg"), names(blank_data))
#
#   print("missing cols blank data :\n")
#   print(missing_cols2)
#
#   if(nrow(blank_data) > 1){
#     blank_data <<- blank_data
#     low_bead_blank <- blank_data[blank_data$lowbeadn == "LowBeadN", c("study_accession",
#                       "plateid", "plate", "analyte", "antigen","mfi", "specimen_type",
#                       "nominal_sample_dilution", "feature", "lowbeadn", "highbeadagg")]
#     high_agg_bead_blank <- blank_data[blank_data$highbeadagg == "PctAggBeads",
#                            c("study_accession", "plateid", "plate", "analyte", "antigen","mfi",
#                           "specimen_type", "nominal_sample_dilution", "feature", "lowbeadn", "highbeadagg")]
#   } else {
#     low_bead_blank <- data.frame()
#     high_agg_bead_blank <- data.frame()
#   }
#   print("after blanks")
#   if(nrow(control_data) > 1){
#     low_bead_control <- control_data[control_data$lowbead == "LowBeadN", c("study_accession",
#                         "plateid", "plate", "analyte", "antigen","mfi", "specimen_type",
#                         "nominal_sample_dilution", "feature","lowbeadn", "highbeadagg")]
#     high_agg_bead_control <- control_data[control_data$highbeadagg == "PctAggBeads",
#                              c("study_accession", "plateid", "plate", "analyte", "antigen","mfi",
#                              "specimen_type", "nominal_sample_dilution", "feature", "lowbeadn", "highbeadagg")]
#   } else {
#     low_bead_control <- data.frame()
#     high_agg_bead_control <- data.frame()
#   }
#
#   cat("\n\nLow Bead Sample \n\n")
#   print(names(low_bead_sample))
#   cat("\n\nLow Bead standard \n\n")
#   print(names(low_bead_standard))
#
#   low_bead_data <- rbind(low_bead_sample, low_bead_standard, low_bead_blank, low_bead_control)
#   high_agg_bead_data <- rbind(high_agg_bead_sample, high_agg_bead_blank, high_agg_bead_control, high_agg_bead_standard)
#   cat("before make summspec\n")
#   print(head(standard_data))
#   summ_spec <- make_summspec(standard_data, blank_data, control_data, raw_samples, low_bead_data, high_agg_bead_data, plates)
#   summ_spec$specimen_type_order <- case_when(
#     summ_spec$specimen_type == "blank" ~ 1,
#     summ_spec$specimen_type == "control" ~ 2,
#     summ_spec$specimen_type == "standard" ~ 3,
#     summ_spec$specimen_type == "sample" ~ 4,
#     summ_spec$specimen_type == "low_bead_count" ~5,
#     summ_spec$specimen_type == "high_aggregate_beads" ~6,
#     TRUE ~ 0
#   )
#   cat("after make spec\n")
#   count_set <- convert_vars(summ_spec)
#   plates$plate <- paste(plates$plate, plates$nominal_sample_dilution, sep = "_")
#   summ_spec_v <<- summ_spec
#   return(list(count_set, plates, raw_samples, summ_spec, standard_fit_data, active_samples))
# }

## Cohort Overview
fetch_study_participant_arms <- function(study_accession) {
  query_participants <- glue::glue_sql("
SELECT DISTINCT experiment_accession, TRIM(agroup) as agroup , COUNT( DISTINCT patientid) as num_patients
FROM madi_results.xmap_sample
WHERE study_accession = {study_accession}
GROUP BY experiment_accession, TRIM(agroup)
ORDER BY experiment_accession", .con = conn)
  paricipant_arms <- dbGetQuery(conn, query_participants)
  return(paricipant_arms)
}

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


make_cv_scatterplot <- function(df, x_var, y_var, facet_var1, facet_var2, color_var, title_var, color_palette) {
  p <- ggplot(df, aes(x = get(x_var), y = get(y_var), color = get(color_var))) +
    geom_point(alpha = 0.7) +
    facet_grid(rows = vars(get(facet_var1)), cols = vars(get(facet_var2))) +
    labs(
      x = camel_case_converter(x_var),
      y = camel_case_converter(y_var),
      color = camel_case_converter(color_var),
      title = title_var
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      strip.text.y = element_text(angle = 0, hjust = 0)
    ) +
    scale_color_manual(values = color_palette)


  return(p)
}


library(dplyr)



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

