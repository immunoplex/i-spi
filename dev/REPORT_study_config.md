# xmap_study_config -- structure + contents dump
working dir = C:/Users/d78039e/Documents/R-git/i-spi-refactor

## 1. Columns and declared types
  xmap_study_config_id         integer          NOT NULL
  study_accession              character varying (15) NOT NULL
  param_group                  character varying (24) 
  param_name                   character varying (50) 
  param_label                  character varying (256) 
  param_data_type              character varying (15) 
  param_char_len               numeric          
  param_control_type           character varying (64) 
  param_choices_list           character varying (256) 
  param_integer_value          integer          
  param_boolean_value          boolean          
  param_character_value        text             
  param_user                   text             
  project_id                   integer          

## 2. Size
  total rows: 11936

## 3. Distinct param names (col 'param_name' if present)
  node_order                               546
  is_binary_gc                             545
  valid_gate_class                         545
  failed_well_criteria                     544
  zero_pass_diluted_Tx                     544
  blank_option                             544
  antigen_family_order                     544
  timeperiod_order                         544
  zero_pass_concentrated_diluted_Tx        544
  zero_pass_concentrated_Tx                544
  one_pass_acceptable_Tx                   544
  primary_timeperiod_comparison            544
  upper_bc_threshold                       544
  mean_mfi                                 544
  two_plus_pass_acceptable_Tx              544
  lower_bc_threshold                       544
  antigen_order                            544
  reference_arm                            544
  default_source                           544
  is_log_mfi_axis                          534
  applyProzone                             534
  pct_agg_threshold                        528

## 4. Distinct param_data_type (if present)
  string               7619
  boolean              2157
  numeric              1616
  categorical          544

## 5. Full contents (all rows, all columns)
  xmap_study_config_id | study_accession | param_group | param_name | param_label | param_data_type | param_char_len | param_control_type | param_choices_list | param_integer_value | param_boolean_value | param_character_value | param_user | project_id
  --------
  465 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein | NA
  466 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | NA
  467 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | NA
  468 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | NA
  469 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | NA
  470 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | NA
  471 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | NA
  472 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | NA
  473 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | NA
  474 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | NA
  475 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | NA
  476 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | NA
  477 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein | NA
  478 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | NA
  479 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | NA
  480 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | NA
  481 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | NA
  482 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | NA
  483 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | NA
  504 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | carolina.argondizoc | NA
  505 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizoc | NA
  506 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizoc | NA
  507 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | NA
  508 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | NA
  509 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | NA
  510 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | NA
  511 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | NA
  512 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | carolina.argondizoc | NA
  513 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizoc | NA
  514 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizoc | NA
  515 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | NA
  516 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | carolina.argondizoc | NA
  517 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | NA
  518 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | NA
  519 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | NA
  520 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | carolina.argondizoc | NA
  521 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | NA
  522 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | NA
  523 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | carolina.argondizoc | 17
  524 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizoc | 17
  525 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizoc | 17
  526 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  527 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  528 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  529 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | 17
  530 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | 17
  531 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 12 | NA | NA | carolina.argondizoc | 17
  532 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizoc | 17
  533 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizoc | 17
  534 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | 17
  535 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | carolina.argondizoc | 17
  536 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | 17
  537 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | 17
  538 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  539 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | carolina.argondizoc | 17
  540 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  541 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  582 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | NA
  583 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | NA
  584 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | 17
  585 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | 17
  586 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | NA
  587 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | NA
  594 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein | 29
  595 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | 29
  596 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | 29
  597 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 29
  598 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 29
  599 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 29
  600 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 29
  601 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 29
  602 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | 29
  603 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | 29
  604 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | 29
  605 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 29
  606 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein | 29
  607 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 29
  608 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 29
  609 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 29
  610 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 29
  611 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 29
  612 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | 29
  613 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 29
  614 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 29
  615 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | cvhomweg | NA
  616 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | cvhomweg | NA
  617 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | cvhomweg | NA
  618 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | NA
  619 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | NA
  620 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | NA
  621 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | cvhomweg | NA
  622 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | cvhomweg | NA
  623 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | cvhomweg | NA
  624 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | cvhomweg | NA
  625 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | cvhomweg | NA
  626 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | cvhomweg | NA
  627 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | cvhomweg | NA
  628 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | cvhomweg | NA
  629 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | cvhomweg | NA
  630 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | cvhomweg | NA
  631 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | cvhomweg | NA
  632 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | cvhomweg | NA
  633 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | cvhomweg | NA
  672 | BK study | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | cvhomweg | 37
  673 | BK study | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | cvhomweg | 37
  674 | BK study | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | cvhomweg | 37
  675 | BK study | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | 37
  676 | BK study | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | 37
  677 | BK study | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | cvhomweg | 37
  678 | BK study | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | cvhomweg | 37
  679 | BK study | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | cvhomweg | 37
  680 | BK study | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | cvhomweg | 37
  681 | BK study | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | cvhomweg | 37
  682 | BK study | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | cvhomweg | 37
  683 | BK study | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | cvhomweg | 37
  684 | BK study | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | cvhomweg | 37
  685 | BK study | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | STD | cvhomweg | 37
  686 | BK study | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | cvhomweg | 37
  687 | BK study | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | cvhomweg | 37
  688 | BK study | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | cvhomweg | 37
  689 | BK study | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | BK virus ,All Antigens | cvhomweg | 37
  690 | BK study | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | vp1,DT,pentamer_cmv_15,pentamer_15,agnoprotein_42,t_ag_22,tt_21,pt_75,s_t_ag_28 | cvhomweg | 37
  691 | GuineBissau | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | carolina.argondizoc | 17
  692 | GuineBissau | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizoc | 17
  693 | GuineBissau | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizoc | 17
  694 | GuineBissau | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  695 | GuineBissau | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  696 | GuineBissau | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizoc | 17
  697 | GuineBissau | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | 17
  698 | GuineBissau | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizoc | 17
  699 | GuineBissau | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | carolina.argondizoc | 17
  700 | GuineBissau | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizoc | 17
  701 | GuineBissau | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizoc | 17
  702 | GuineBissau | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizoc | 17
  703 | GuineBissau | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | carolina.argondizoc | 17
  704 | GuineBissau | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | 17
  705 | GuineBissau | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | carolina.argondizoc | 17
  706 | GuineBissau | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  707 | GuineBissau | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | carolina.argondizoc | 17
  708 | GuineBissau | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  709 | GuineBissau | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizoc | 17
  710 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck | NA
  711 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck | NA
  712 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck | NA
  713 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | NA
  714 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | NA
  715 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | NA
  716 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | NA
  717 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | NA
  718 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck | NA
  719 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck | NA
  720 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck | NA
  721 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck | NA
  722 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck | NA
  723 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck | NA
  724 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck | NA
  725 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | NA
  726 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck | NA
  727 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | NA
  728 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | NA
  729 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck | 17
  730 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck | 17
  731 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck | 17
  732 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  733 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  734 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  735 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  736 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  737 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck | 17
  738 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck | 17
  739 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck | 17
  740 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck | 17
  741 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck | 17
  742 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck | 17
  743 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck | 17
  744 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  745 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck | 17
  746 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  747 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  748 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck | 29
  749 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck | 29
  750 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck | 29
  751 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 29
  752 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 29
  753 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 29
  754 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 29
  755 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 29
  756 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck | 29
  757 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck | 29
  758 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck | 29
  759 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck | 29
  760 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck | 29
  761 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck | 29
  762 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck | 29
  763 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 29
  764 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck | 29
  765 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 29
  766 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 29
  767 | GAPS_mothers | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck | 17
  768 | GAPS_mothers | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck | 17
  769 | GAPS_mothers | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck | 17
  770 | GAPS_mothers | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  771 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  772 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  773 | GAPS_mothers | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  774 | GAPS_mothers | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  775 | GAPS_mothers | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck | 17
  776 | GAPS_mothers | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck | 17
  777 | GAPS_mothers | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck | 17
  778 | GAPS_mothers | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck | 17
  779 | GAPS_mothers | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck | 17
  780 | GAPS_mothers | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck | 17
  781 | GAPS_mothers | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck | 17
  782 | GAPS_mothers | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  783 | GAPS_mothers | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck | 17
  784 | GAPS_mothers | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  785 | GAPS_mothers | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  786 | MADI_P3_GAPS | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_quantification | loicdedonck | 17
  787 | MADI_P3_GAPS | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck | 17
  788 | MADI_P3_GAPS | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck | 17
  789 | MADI_P3_GAPS | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  790 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  791 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck | 17
  792 | MADI_P3_GAPS | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  793 | MADI_P3_GAPS | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck | 17
  794 | MADI_P3_GAPS | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck | 17
  795 | MADI_P3_GAPS | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck | 17
  796 | MADI_P3_GAPS | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck | 17
  797 | MADI_P3_GAPS | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck | 17
  798 | MADI_P3_GAPS | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck | 17
  799 | MADI_P3_GAPS | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | SD | loicdedonck | 17
  800 | MADI_P3_GAPS | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | TdaP_wP | loicdedonck | 17
  801 | MADI_P3_GAPS | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | pre3rd,pre1st,post3rd,post3rd5mo | loicdedonck | 17
  802 | MADI_P3_GAPS | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | pre3rd,pre1st | loicdedonck | 17
  803 | MADI_P3_GAPS | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  804 | MADI_P3_GAPS | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck | 17
  883 | MADI_P3_GAPS | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein | 17
  884 | MADI_P3_GAPS | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | 17
  885 | MADI_P3_GAPS | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | 17
  886 | MADI_P3_GAPS | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  887 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  888 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  889 | MADI_P3_GAPS | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  890 | MADI_P3_GAPS | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  891 | MADI_P3_GAPS | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | 17
  892 | MADI_P3_GAPS | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | 17
  893 | MADI_P3_GAPS | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | 17
  894 | MADI_P3_GAPS | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  895 | MADI_P3_GAPS | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein | 17
  896 | MADI_P3_GAPS | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC06_140 | seamus.owen.stein | 17
  897 | MADI_P3_GAPS | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  898 | MADI_P3_GAPS | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  899 | MADI_P3_GAPS | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 17
  900 | MADI_P3_GAPS | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  901 | MADI_P3_GAPS | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | 17
  902 | MADI_P3_GAPS | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  903 | MADI_P3_GAPS | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  904 | GuineBissau | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein | 17
  905 | GuineBissau | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | 17
  906 | GuineBissau | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | 17
  907 | GuineBissau | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  908 | GuineBissau | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  909 | GuineBissau | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  910 | GuineBissau | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  911 | GuineBissau | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  912 | GuineBissau | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | 17
  913 | GuineBissau | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | 17
  914 | GuineBissau | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | 17
  915 | GuineBissau | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  916 | GuineBissau | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein | 17
  917 | GuineBissau | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 17
  918 | GuineBissau | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  919 | GuineBissau | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  920 | GuineBissau | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 17
  921 | GuineBissau | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  922 | GuineBissau | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | 17
  923 | GuineBissau | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  924 | GuineBissau | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  1155 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_detection | seamus.owen.stein | 17
  1156 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | 17
  1157 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | 17
  1158 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  1159 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  1160 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 17
  1161 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  1162 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 17
  1163 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | 17
  1164 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | 17
  1165 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | 17
  1166 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein | 17
  1167 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  1168 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | seamus.owen.stein | 17
  1169 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | seamus.owen.stein | 17
  1170 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  1171 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 17
  1172 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 17
  1173 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  1174 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | 17
  1175 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  1176 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 17
  1177 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | yiwei.jiang2015 | 29
  1178 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | yiwei.jiang2015 | 29
  1179 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | yiwei.jiang2015 | 29
  1180 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 29
  1181 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 29
  1182 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 29
  1183 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015 | 29
  1184 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015 | 29
  1185 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | yiwei.jiang2015 | 29
  1186 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | yiwei.jiang2015 | 29
  1187 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | yiwei.jiang2015 | 29
  1188 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015 | 29
  1189 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | yiwei.jiang2015 | 29
  1190 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1191 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1192 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1193 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1194 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1195 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 29
  1196 | ELIKYA II | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | yiwei.jiang2015 | 39
  1197 | ELIKYA II | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | yiwei.jiang2015 | 39
  1198 | ELIKYA II | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | yiwei.jiang2015 | 39
  1199 | ELIKYA II | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 39
  1200 | ELIKYA II | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 39
  1201 | ELIKYA II | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015 | 39
  1202 | ELIKYA II | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015 | 39
  1203 | ELIKYA II | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015 | 39
  1204 | ELIKYA II | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | yiwei.jiang2015 | 39
  1205 | ELIKYA II | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | yiwei.jiang2015 | 39
  1206 | ELIKYA II | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | yiwei.jiang2015 | 39
  1207 | ELIKYA II | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015 | 39
  1208 | ELIKYA II | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | yiwei.jiang2015 | 39
  1209 | ELIKYA II | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1210 | ELIKYA II | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1211 | ELIKYA II | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1212 | ELIKYA II | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1213 | ELIKYA II | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1214 | ELIKYA II | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015 | 39
  1215 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta.th | NA
  1216 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta.th | NA
  1217 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta.th | NA
  1218 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | NA
  1219 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | NA
  1220 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | NA
  1221 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | NA
  1222 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | NA
  1223 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta.th | NA
  1224 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta.th | NA
  1225 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta.th | NA
  1226 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta.th | NA
  1227 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | NA
  1228 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta.th | NA
  1229 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | hardik.gupta.th | NA
  1230 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | NA
  1231 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | NA
  1232 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta.th | NA
  1233 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | NA
  1234 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta.th | NA
  1235 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | NA
  1236 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | NA
  1237 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta.th | 17
  1238 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta.th | 17
  1239 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta.th | 17
  1240 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  1241 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  1242 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  1243 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 17
  1244 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 17
  1245 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta.th | 17
  1246 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta.th | 17
  1247 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta.th | 17
  1248 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta.th | 17
  1249 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  1250 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta.th | 17
  1251 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | hardik.gupta.th | 17
  1252 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  1253 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  1254 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta.th | 17
  1255 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  1256 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta.th | 17
  1257 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  1258 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  1259 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | admin@example.com | 29
  1260 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | admin@example.com | 29
  1261 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | admin@example.com | 29
  1262 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | admin@example.com | 29
  1263 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | admin@example.com | 29
  1264 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | admin@example.com | 29
  1265 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | admin@example.com | 29
  1266 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | admin@example.com | 29
  1267 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | admin@example.com | 29
  1268 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | admin@example.com | 29
  1269 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | admin@example.com | 29
  1270 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | admin@example.com | 29
  1271 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | admin@example.com | 29
  1272 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | admin@example.com | 29
  1273 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | admin@example.com | 29
  1274 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | admin@example.com | 29
  1275 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | admin@example.com | 29
  1276 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | admin@example.com | 29
  1277 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | admin@example.com | 29
  1278 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | admin@example.com | 29
  1279 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | admin@example.com | 29
  1280 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | admin@example.com | 29
  1281 | ELIKYA II | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein | 39
  1282 | ELIKYA II | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein | 39
  1283 | ELIKYA II | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein | 39
  1284 | ELIKYA II | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 39
  1285 | ELIKYA II | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 39
  1286 | ELIKYA II | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein | 39
  1287 | ELIKYA II | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 39
  1288 | ELIKYA II | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein | 39
  1289 | ELIKYA II | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein | 39
  1290 | ELIKYA II | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein | 39
  1291 | ELIKYA II | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein | 39
  1292 | ELIKYA II | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein | 39
  1293 | ELIKYA II | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 39
  1294 | ELIKYA II | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein | 39
  1295 | ELIKYA II | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Sandoglobuline | seamus.owen.stein | 39
  1296 | ELIKYA II | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 39
  1297 | ELIKYA II | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein | 39
  1298 | ELIKYA II | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein | 39
  1299 | ELIKYA II | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 39
  1300 | ELIKYA II | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein | 39
  1301 | ELIKYA II | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 39
  1302 | ELIKYA II | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein | 39
  1303 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta@dartmouth.edu | 29
  1304 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta@dartmouth.edu | 29
  1305 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta@dartmouth.edu | 29
  1306 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 29
  1307 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 29
  1308 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 29
  1309 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 29
  1310 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 29
  1311 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta@dartmouth.edu | 29
  1312 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta@dartmouth.edu | 29
  1313 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta@dartmouth.edu | 29
  1314 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta@dartmouth.edu | 29
  1315 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 29
  1316 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta@dartmouth.edu | 29
  1317 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | hardik.gupta@dartmouth.edu | 29
  1318 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 29
  1319 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 29
  1320 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 29
  1321 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 29
  1322 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 29
  1323 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 29
  1324 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 29
  1325 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  1326 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  1327 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  1328 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  1329 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  1330 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  1331 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  1332 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  1333 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1334 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1335 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  1336 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1337 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  1338 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  1339 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1340 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  1341 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  1342 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1343 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1344 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1345 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1346 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  1369 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 29
  1370 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 29
  1371 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 29
  1372 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 29
  1373 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 29
  1374 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 29
  1375 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 29
  1376 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 29
  1377 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1378 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1379 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 29
  1380 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1381 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 29
  1382 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 29
  1383 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | seamus.owen.stein@dartmouth.edu | 29
  1384 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 29
  1385 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 29
  1386 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1387 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1388 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1389 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1390 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 29
  1391 | BK study | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 37
  1392 | BK study | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 37
  1393 | BK study | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 37
  1394 | BK study | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 37
  1395 | BK study | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 37
  1396 | BK study | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 37
  1397 | BK study | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 37
  1398 | BK study | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 37
  1399 | BK study | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 37
  1400 | BK study | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 37
  1401 | BK study | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 37
  1402 | BK study | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 37
  1403 | BK study | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 37
  1404 | BK study | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | seamus.owen.stein@dartmouth.edu | 37
  1405 | BK study | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | STD | seamus.owen.stein@dartmouth.edu | 37
  1406 | BK study | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 37
  1407 | BK study | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 37
  1408 | BK study | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | Viremic P | seamus.owen.stein@dartmouth.edu | 37
  1409 | BK study | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | T0,T3M,T1Y | seamus.owen.stein@dartmouth.edu | 37
  1410 | BK study | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | T0,T1Y | seamus.owen.stein@dartmouth.edu | 37
  1411 | BK study | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | All Antigens,BK virus  | seamus.owen.stein@dartmouth.edu | 37
  1412 | BK study | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | DT,vp1,pentamer_cmv_15,pentamer_15,agnoprotein_42,t_ag_22,tt_21,pt_75,s_t_ag_28 | seamus.owen.stein@dartmouth.edu | 37
  1435 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | carolina.argondizo.correia@ulb.be | NA
  1436 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizo.correia@ulb.be | NA
  1437 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizo.correia@ulb.be | NA
  1438 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | NA
  1439 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | NA
  1440 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | NA
  1441 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizo.correia@ulb.be | NA
  1442 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizo.correia@ulb.be | NA
  1443 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1444 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1445 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizo.correia@ulb.be | NA
  1446 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1447 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | NA
  1448 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | carolina.argondizo.correia@ulb.be | NA
  1449 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1450 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | NA
  1451 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | NA
  1452 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1453 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1454 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1455 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1456 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | NA
  1457 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_detection | carolina.argondizo.correia@ulb.be | 17
  1458 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizo.correia@ulb.be | 17
  1459 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizo.correia@ulb.be | 17
  1460 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 17
  1461 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 17
  1462 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 17
  1463 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 17
  1464 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 17
  1465 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 12 | NA | NA | carolina.argondizo.correia@ulb.be | 17
  1466 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizo.correia@ulb.be | 17
  1467 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizo.correia@ulb.be | 17
  1468 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | carolina.argondizo.correia@ulb.be | 17
  1469 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 17
  1470 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | carolina.argondizo.correia@ulb.be | 17
  1471 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | carolina.argondizo.correia@ulb.be | 17
  1472 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 17
  1473 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 17
  1474 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | nonpregnant | carolina.argondizo.correia@ulb.be | 17
  1475 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | V00,V03,CB1,CB,CB2,V04,NA,V01 | carolina.argondizo.correia@ulb.be | 17
  1476 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | V00,V03 | carolina.argondizo.correia@ulb.be | 17
  1477 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | All Antigens,H3N2,SARS-CoV-2,Polio,B/Victoria,CMV,Pertussis,TT,H1N1,B/Yamagata | carolina.argondizo.correia@ulb.be | 17
  1478 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 17
  1523 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_quantification | mscotzens@gmail.com | 17
  1524 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | 17
  1525 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 17
  1526 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  1527 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  1528 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  1529 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  1530 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  1531 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 12 | NA | NA | mscotzens@gmail.com | 17
  1532 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | 17
  1533 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | 17
  1534 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | 17
  1535 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  1536 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | 17
  1537 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | mscotzens@gmail.com | 17
  1538 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  1539 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  1540 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | nonpregnant | mscotzens@gmail.com | 17
  1541 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | V00,V03,CB1,CB,CB2,V04,NA,V01 | mscotzens@gmail.com | 17
  1542 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | V00,V03 | mscotzens@gmail.com | 17
  1543 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | H3N2,SARS-CoV-2,B/Victoria,CMV,Pertussis,Polio,TT,H1N1,B/Yamagata | mscotzens@gmail.com | 17
  1544 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | a_darwin_14,cambodia_63,b_austria_18,hong_kong_55,cg2_61,fha_27,g_b_12,ipv_12... | mscotzens@gmail.com | 17
  1567 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | sos@dartmouth.edu | 29
  1568 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | sos@dartmouth.edu | 29
  1569 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | sos@dartmouth.edu | 29
  1570 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 29
  1571 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 29
  1572 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 29
  1573 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | 29
  1574 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | 29
  1575 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | sos@dartmouth.edu | 29
  1576 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | sos@dartmouth.edu | 29
  1577 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | sos@dartmouth.edu | 29
  1578 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | sos@dartmouth.edu | 29
  1579 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 29
  1580 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | sos@dartmouth.edu | 29
  1581 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | sos@dartmouth.edu | 29
  1582 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 29
  1583 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 29
  1584 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | sos@dartmouth.edu | 29
  1585 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 29
  1586 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | sos@dartmouth.edu | 29
  1587 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 29
  1588 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 29
  1589 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | sos@dartmouth.edu | NA
  1590 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | sos@dartmouth.edu | NA
  1591 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | sos@dartmouth.edu | NA
  1592 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | NA
  1593 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | NA
  1594 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | NA
  1595 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | NA
  1596 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | NA
  1597 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | sos@dartmouth.edu | NA
  1598 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | sos@dartmouth.edu | NA
  1599 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | sos@dartmouth.edu | NA
  1600 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | sos@dartmouth.edu | NA
  1601 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | NA
  1602 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | sos@dartmouth.edu | NA
  1603 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1604 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | NA
  1605 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | NA
  1606 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1607 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1608 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1609 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1610 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | NA
  1611 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | sos@dartmouth.edu | 17
  1612 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | sos@dartmouth.edu | 17
  1613 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | sos@dartmouth.edu | 17
  1614 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 17
  1615 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 17
  1616 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | sos@dartmouth.edu | 17
  1617 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | 17
  1618 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | sos@dartmouth.edu | 17
  1619 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | sos@dartmouth.edu | 17
  1620 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | sos@dartmouth.edu | 17
  1621 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | sos@dartmouth.edu | 17
  1622 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | sos@dartmouth.edu | 17
  1623 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 17
  1624 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | sos@dartmouth.edu | 17
  1625 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | sos@dartmouth.edu | 17
  1626 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 17
  1627 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | sos@dartmouth.edu | 17
  1628 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | sos@dartmouth.edu | 17
  1629 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 17
  1630 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | sos@dartmouth.edu | 17
  1631 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 17
  1632 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | sos@dartmouth.edu | 17
  1633 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | 29
  1634 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | 29
  1635 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | 29
  1636 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 29
  1637 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 29
  1638 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 29
  1639 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 29
  1640 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 29
  1641 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | 29
  1642 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | 29
  1643 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | 29
  1644 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | 29
  1645 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 29
  1646 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | 29
  1647 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | loicdedonck@gmail.com | 29
  1648 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 29
  1649 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 29
  1650 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | 29
  1651 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 29
  1652 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | 29
  1653 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 29
  1654 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 29
  1655 | ELIKYA II | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 39
  1656 | ELIKYA II | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 39
  1657 | ELIKYA II | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 39
  1658 | ELIKYA II | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 39
  1659 | ELIKYA II | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 39
  1660 | ELIKYA II | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 39
  1661 | ELIKYA II | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 39
  1662 | ELIKYA II | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 39
  1663 | ELIKYA II | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1664 | ELIKYA II | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1665 | ELIKYA II | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 39
  1666 | ELIKYA II | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1667 | ELIKYA II | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 39
  1668 | ELIKYA II | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 39
  1669 | ELIKYA II | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Sandoglobuline | seamus.owen.stein@dartmouth.edu | 39
  1670 | ELIKYA II | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 39
  1671 | ELIKYA II | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 39
  1672 | ELIKYA II | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1673 | ELIKYA II | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1674 | ELIKYA II | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1675 | ELIKYA II | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1676 | ELIKYA II | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 39
  1677 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | tester@secretservice.com | 29
  1678 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | tester@secretservice.com | 29
  1679 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | tester@secretservice.com | 29
  1680 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | tester@secretservice.com | 29
  1681 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | tester@secretservice.com | 29
  1682 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | tester@secretservice.com | 29
  1683 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | tester@secretservice.com | 29
  1684 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | tester@secretservice.com | 29
  1685 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | tester@secretservice.com | 29
  1686 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | tester@secretservice.com | 29
  1687 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | tester@secretservice.com | 29
  1688 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | tester@secretservice.com | 29
  1689 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | tester@secretservice.com | 29
  1690 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | tester@secretservice.com | 29
  1691 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | tester@secretservice.com | 29
  1692 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | tester@secretservice.com | 29
  1693 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | tester@secretservice.com | 29
  1694 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | tester@secretservice.com | 29
  1695 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | tester@secretservice.com | 29
  1696 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | tester@secretservice.com | 29
  1697 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | tester@secretservice.com | 29
  1698 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | tester@secretservice.com | 29
  1699 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | dev_user@dartmouth.edu | 29
  1700 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | dev_user@dartmouth.edu | 29
  1701 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | dev_user@dartmouth.edu | 29
  1702 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 29
  1703 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 29
  1704 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 29
  1705 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | 29
  1706 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | 29
  1707 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | dev_user@dartmouth.edu | 29
  1708 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | dev_user@dartmouth.edu | 29
  1709 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | dev_user@dartmouth.edu | 29
  1710 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | dev_user@dartmouth.edu | 29
  1711 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 29
  1712 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | dev_user@dartmouth.edu | 29
  1713 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | dev_user@dartmouth.edu | 29
  1714 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 29
  1715 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 29
  1716 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | dev_user@dartmouth.edu | 29
  1717 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 29
  1718 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | dev_user@dartmouth.edu | 29
  1719 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 29
  1720 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 29
  1721 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | dev_user@dartmouth.edu | NA
  1722 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | dev_user@dartmouth.edu | NA
  1723 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | dev_user@dartmouth.edu | NA
  1724 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | NA
  1725 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | NA
  1726 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | NA
  1727 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | NA
  1728 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | NA
  1729 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | dev_user@dartmouth.edu | NA
  1730 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | dev_user@dartmouth.edu | NA
  1731 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | dev_user@dartmouth.edu | NA
  1732 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | dev_user@dartmouth.edu | NA
  1733 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | NA
  1734 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | dev_user@dartmouth.edu | NA
  1735 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1736 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | NA
  1737 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | NA
  1738 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1739 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1740 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1741 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1742 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | NA
  1743 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | dev_user@dartmouth.edu | 17
  1744 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | dev_user@dartmouth.edu | 17
  1745 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | dev_user@dartmouth.edu | 17
  1746 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 17
  1747 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 17
  1748 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | dev_user@dartmouth.edu | 17
  1749 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | 17
  1750 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | dev_user@dartmouth.edu | 17
  1751 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | dev_user@dartmouth.edu | 17
  1752 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | dev_user@dartmouth.edu | 17
  1753 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | dev_user@dartmouth.edu | 17
  1754 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | dev_user@dartmouth.edu | 17
  1755 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 17
  1756 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | dev_user@dartmouth.edu | 17
  1757 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | dev_user@dartmouth.edu | 17
  1758 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 17
  1759 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | dev_user@dartmouth.edu | 17
  1760 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | dev_user@dartmouth.edu | 17
  1761 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 17
  1762 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | dev_user@dartmouth.edu | 17
  1763 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 17
  1764 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | dev_user@dartmouth.edu | 17
  1765 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | test@secret.com | 29
  1766 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | test@secret.com | 29
  1767 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | test@secret.com | 29
  1768 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | test@secret.com | 29
  1769 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | test@secret.com | 29
  1770 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | test@secret.com | 29
  1771 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | test@secret.com | 29
  1772 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | test@secret.com | 29
  1773 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | test@secret.com | 29
  1774 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | test@secret.com | 29
  1775 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | test@secret.com | 29
  1776 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | test@secret.com | 29
  1777 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | test@secret.com | 29
  1778 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | test@secret.com | 29
  1779 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | test@secret.com | 29
  1780 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | test@secret.com | 29
  1781 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | test@secret.com | 29
  1782 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | test@secret.com | 29
  1783 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | test@secret.com | 29
  1784 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | test@secret.com | 29
  1785 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | test@secret.com | 29
  1786 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | test@secret.com | 29
  1787 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | yiwei.jiang2015@gmail.com | 29
  1788 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | yiwei.jiang2015@gmail.com | 29
  1789 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | yiwei.jiang2015@gmail.com | 29
  1790 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 29
  1791 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 29
  1792 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 29
  1793 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015@gmail.com | 29
  1794 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | yiwei.jiang2015@gmail.com | 29
  1795 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | yiwei.jiang2015@gmail.com | 29
  1796 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | yiwei.jiang2015@gmail.com | 29
  1797 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | yiwei.jiang2015@gmail.com | 29
  1798 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | yiwei.jiang2015@gmail.com | 29
  1799 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 29
  1800 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | yiwei.jiang2015@gmail.com | 29
  1801 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | yiwei.jiang2015@gmail.com | 29
  1802 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 29
  1803 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 29
  1804 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 29
  1805 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 29
  1806 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 29
  1807 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 29
  1808 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 29
  1809 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta.th | 29
  1810 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta.th | 29
  1811 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta.th | 29
  1812 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 29
  1813 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 29
  1814 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 29
  1815 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 29
  1816 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 29
  1817 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta.th | 29
  1818 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta.th | 29
  1819 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta.th | 29
  1820 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta.th | 29
  1821 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 29
  1822 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta.th | 29
  1823 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | hardik.gupta.th | 29
  1824 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 29
  1825 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 29
  1826 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta.th | 29
  1827 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 29
  1828 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta.th | 29
  1829 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 29
  1830 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 29
  1831 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | mscotzens@gmail.com | 29
  1832 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | 29
  1833 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 29
  1834 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 29
  1835 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 29
  1836 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 29
  1837 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 29
  1838 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 29
  1839 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | mscotzens@gmail.com | 29
  1840 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | 29
  1841 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | 29
  1842 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | 29
  1843 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 29
  1844 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | 29
  1845 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | mscotzens@gmail.com | 29
  1846 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 29
  1847 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 29
  1848 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | 29
  1849 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 29
  1850 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | mscotzens@gmail.com | 29
  1851 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 29
  1852 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 29
  1853 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | mscotzens@gmail.com | NA
  1854 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | NA
  1855 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | NA
  1856 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | NA
  1857 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | NA
  1858 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | NA
  1859 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | NA
  1860 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | NA
  1861 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | mscotzens@gmail.com | NA
  1862 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | NA
  1863 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | NA
  1864 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | NA
  1865 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | NA
  1866 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | NA
  1867 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1868 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | NA
  1869 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | NA
  1870 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1871 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1872 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1873 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1874 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | NA
  1875 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | NA
  1876 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | NA
  1877 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | NA
  1878 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  1879 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  1880 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  1881 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  1882 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  1883 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | NA
  1884 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | NA
  1885 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | NA
  1886 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | NA
  1887 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  1888 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | NA
  1889 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1890 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  1891 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  1892 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1893 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1894 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1895 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1896 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  1897 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | 17
  1898 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | 17
  1899 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | 17
  1900 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  1901 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  1902 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  1903 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  1904 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  1905 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | 17
  1906 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | 17
  1907 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | 17
  1908 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | 17
  1909 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  1910 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | loicdedonck@gmail.com | 17
  1911 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | loicdedonck@gmail.com | 17
  1912 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  1913 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  1914 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  1915 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  1916 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  1917 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  1918 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  1919 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | charlotte.vanhomwegen@ulb.be | 29
  1920 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | charlotte.vanhomwegen@ulb.be | 29
  1921 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | charlotte.vanhomwegen@ulb.be | 29
  1922 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | 29
  1923 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | 29
  1924 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | 29
  1925 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | charlotte.vanhomwegen@ulb.be | 29
  1926 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | charlotte.vanhomwegen@ulb.be | 29
  1927 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1928 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1929 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | charlotte.vanhomwegen@ulb.be | 29
  1930 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1931 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | 29
  1932 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | charlotte.vanhomwegen@ulb.be | 29
  1933 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | charlotte.vanhomwegen@ulb.be | 29
  1934 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | 29
  1935 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | 29
  1936 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1937 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1938 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1939 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1940 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | 29
  1941 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_detection | seamus.owen.stein@dartmouth.edu | 17
  1942 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 17
  1943 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  1944 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  1945 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  1946 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  1947 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  1948 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  1949 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 60 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  1950 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  1951 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 17
  1952 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 9 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  1953 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  1954 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 17
  1955 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | seamus.owen.stein@dartmouth.edu | 17
  1956 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  1957 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  1958 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | nonpregnant | seamus.owen.stein@dartmouth.edu | 17
  1959 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | CB,V03,V04,V01,V00,CB2,CB1,NA | seamus.owen.stein@dartmouth.edu | 17
  1960 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | V00,V03 | seamus.owen.stein@dartmouth.edu | 17
  1961 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | H3N2,SARS-CoV-2,B/Victoria,CMV,Pertussis,Polio,TT,H1N1,B/Yamagata | seamus.owen.stein@dartmouth.edu | 17
  1962 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | a_darwin_14,hong_kong_55,cambodia_63,b_austria_18,cg2_61,fha_27,g_b_12,ipv_12... | seamus.owen.stein@dartmouth.edu | 17
  1963 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta@dartmouth.edu | NA
  1964 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta@dartmouth.edu | NA
  1965 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta@dartmouth.edu | NA
  1966 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | NA
  1967 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | NA
  1968 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | NA
  1969 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | NA
  1970 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | NA
  1971 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta@dartmouth.edu | NA
  1972 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta@dartmouth.edu | NA
  1973 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta@dartmouth.edu | NA
  1974 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta@dartmouth.edu | NA
  1975 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | NA
  1976 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta@dartmouth.edu | NA
  1977 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1978 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | NA
  1979 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | NA
  1980 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1981 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1982 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1983 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1984 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | NA
  1985 | MADI_01 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta@dartmouth.edu | 17
  1986 | MADI_01 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta@dartmouth.edu | 17
  1987 | MADI_01 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta@dartmouth.edu | 17
  1988 | MADI_01 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  1989 | MADI_01 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  1990 | MADI_01 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  1991 | MADI_01 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 17
  1992 | MADI_01 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 17
  1993 | MADI_01 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta@dartmouth.edu | 17
  1994 | MADI_01 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta@dartmouth.edu | 17
  1995 | MADI_01 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta@dartmouth.edu | 17
  1996 | MADI_01 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta@dartmouth.edu | 17
  1997 | MADI_01 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  1998 | MADI_01 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta@dartmouth.edu | 17
  1999 | MADI_01 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Standard | hardik.gupta@dartmouth.edu | 17
  2000 | MADI_01 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  2001 | MADI_01 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  2002 | MADI_01 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2003 | MADI_01 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2004 | MADI_01 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2005 | MADI_01 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | CMV,Pertussis,SARS-CoV-2,TT,B/Victoria,H1N1,B/Yamagata,Polio,H3N2 | hardik.gupta@dartmouth.edu | 17
  2006 | MADI_01 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | s2,tt_21,pt_25,a_darwin_14,cg1_20,cambodia_new_36,victoria_57,ipv2,phuket_78,... | hardik.gupta@dartmouth.edu | 17
  2007 | ELIKYA II | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_detection | yiwei.jiang2015@gmail.com | 39
  2008 | ELIKYA II | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | yiwei.jiang2015@gmail.com | 39
  2009 | ELIKYA II | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | yiwei.jiang2015@gmail.com | 39
  2010 | ELIKYA II | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 39
  2011 | ELIKYA II | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 39
  2012 | ELIKYA II | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 39
  2013 | ELIKYA II | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 39
  2014 | ELIKYA II | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | yiwei.jiang2015@gmail.com | 39
  2015 | ELIKYA II | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | yiwei.jiang2015@gmail.com | 39
  2016 | ELIKYA II | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | yiwei.jiang2015@gmail.com | 39
  2017 | ELIKYA II | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | yiwei.jiang2015@gmail.com | 39
  2018 | ELIKYA II | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | yiwei.jiang2015@gmail.com | 39
  2019 | ELIKYA II | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 39
  2020 | ELIKYA II | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | yiwei.jiang2015@gmail.com | 39
  2021 | ELIKYA II | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | sandoglobulin | yiwei.jiang2015@gmail.com | 39
  2022 | ELIKYA II | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 39
  2023 | ELIKYA II | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | yiwei.jiang2015@gmail.com | 39
  2024 | ELIKYA II | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 39
  2025 | ELIKYA II | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 39
  2026 | ELIKYA II | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 39
  2027 | ELIKYA II | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 39
  2028 | ELIKYA II | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | yiwei.jiang2015@gmail.com | 39
  2029 | MADI_P3_GAPS | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | limits_of_detection | seamus.owen.stein@dartmouth.edu | 17
  2030 | MADI_P3_GAPS | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Above_Upper_Limit_of_Detection | seamus.owen.stein@dartmouth.edu | 17
  2031 | MADI_P3_GAPS | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2032 | MADI_P3_GAPS | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2033 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2034 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2035 | MADI_P3_GAPS | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  2036 | MADI_P3_GAPS | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  2037 | MADI_P3_GAPS | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 34 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2038 | MADI_P3_GAPS | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2039 | MADI_P3_GAPS | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 17
  2040 | MADI_P3_GAPS | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2041 | MADI_P3_GAPS | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2042 | MADI_P3_GAPS | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 17
  2043 | MADI_P3_GAPS | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC06_140 | seamus.owen.stein@dartmouth.edu | 17
  2044 | MADI_P3_GAPS | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2045 | MADI_P3_GAPS | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2046 | MADI_P3_GAPS | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | TdaP_wP | seamus.owen.stein@dartmouth.edu | 17
  2047 | MADI_P3_GAPS | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | pre3rd,pre1st,post3rd,post3rd5mo | seamus.owen.stein@dartmouth.edu | 17
  2048 | MADI_P3_GAPS | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | pre3rd,pre1st | seamus.owen.stein@dartmouth.edu | 17
  2049 | MADI_P3_GAPS | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | Pertussis,polio,All Antigens | seamus.owen.stein@dartmouth.edu | 17
  2050 | MADI_P3_GAPS | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | fim,act,dt,pentamer,tt,prn,pt,ipv1,fha,ipv2,ipv3 | seamus.owen.stein@dartmouth.edu | 17
  2051 | GAPS_mothers | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | mscotzens@gmail.com | 17
  2052 | GAPS_mothers | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | 17
  2053 | GAPS_mothers | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 17
  2054 | GAPS_mothers | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2055 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2056 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2057 | GAPS_mothers | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  2058 | GAPS_mothers | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  2059 | GAPS_mothers | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | mscotzens@gmail.com | 17
  2060 | GAPS_mothers | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | 17
  2061 | GAPS_mothers | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | 17
  2062 | GAPS_mothers | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | 17
  2063 | GAPS_mothers | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  2064 | GAPS_mothers | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | 17
  2065 | GAPS_mothers | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2066 | GAPS_mothers | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  2067 | GAPS_mothers | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  2068 | GAPS_mothers | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2069 | GAPS_mothers | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2070 | GAPS_mothers | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2071 | GAPS_mothers | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2072 | GAPS_mothers | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 17
  2095 | GuineBissau | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta@dartmouth.edu | 17
  2096 | GuineBissau | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta@dartmouth.edu | 17
  2097 | GuineBissau | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta@dartmouth.edu | 17
  2098 | GuineBissau | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  2099 | GuineBissau | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  2100 | GuineBissau | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta@dartmouth.edu | 17
  2101 | GuineBissau | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 17
  2102 | GuineBissau | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta@dartmouth.edu | 17
  2103 | GuineBissau | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta@dartmouth.edu | 17
  2104 | GuineBissau | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta@dartmouth.edu | 17
  2105 | GuineBissau | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta@dartmouth.edu | 17
  2106 | GuineBissau | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta@dartmouth.edu | 17
  2107 | GuineBissau | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  2108 | GuineBissau | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta@dartmouth.edu | 17
  2109 | GuineBissau | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | sandoglobulin | hardik.gupta@dartmouth.edu | 17
  2110 | GuineBissau | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  2111 | GuineBissau | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta@dartmouth.edu | 17
  2112 | GuineBissau | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2113 | GuineBissau | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2114 | GuineBissau | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2115 | GuineBissau | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2116 | GuineBissau | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta@dartmouth.edu | 17
  2117 | GuineBissau | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | hardik.gupta.th | 17
  2118 | GuineBissau | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | hardik.gupta.th | 17
  2119 | GuineBissau | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | hardik.gupta.th | 17
  2120 | GuineBissau | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  2121 | GuineBissau | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  2122 | GuineBissau | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | hardik.gupta.th | 17
  2123 | GuineBissau | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 17
  2124 | GuineBissau | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | hardik.gupta.th | 17
  2125 | GuineBissau | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | hardik.gupta.th | 17
  2126 | GuineBissau | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | hardik.gupta.th | 17
  2127 | GuineBissau | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | hardik.gupta.th | 17
  2128 | GuineBissau | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | hardik.gupta.th | 17
  2129 | GuineBissau | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  2130 | GuineBissau | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | hardik.gupta.th | 17
  2131 | GuineBissau | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | sandoglobulin | hardik.gupta.th | 17
  2132 | GuineBissau | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  2133 | GuineBissau | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | hardik.gupta.th | 17
  2134 | GuineBissau | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | hardik.gupta.th | 17
  2135 | GuineBissau | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  2136 | GuineBissau | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | hardik.gupta.th | 17
  2137 | GuineBissau | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  2138 | GuineBissau | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | hardik.gupta.th | 17
  2139 | Click here | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | carolina.argondizo.correia@ulb.be | 29
  2140 | Click here | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | carolina.argondizo.correia@ulb.be | 29
  2141 | Click here | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | carolina.argondizo.correia@ulb.be | 29
  2142 | Click here | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 29
  2143 | Click here | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 29
  2144 | Click here | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | carolina.argondizo.correia@ulb.be | 29
  2145 | Click here | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizo.correia@ulb.be | 29
  2146 | Click here | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | carolina.argondizo.correia@ulb.be | 29
  2147 | Click here | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2148 | Click here | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2149 | Click here | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | carolina.argondizo.correia@ulb.be | 29
  2150 | Click here | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2151 | Click here | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 29
  2152 | Click here | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | carolina.argondizo.correia@ulb.be | 29
  2153 | Click here | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Megalotect | carolina.argondizo.correia@ulb.be | 29
  2154 | Click here | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 29
  2155 | Click here | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | carolina.argondizo.correia@ulb.be | 29
  2156 | Click here | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2157 | Click here | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2158 | Click here | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2159 | Click here | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2160 | Click here | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | carolina.argondizo.correia@ulb.be | 29
  2161 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | charlotte.vanhomwegen@ulb.be | NA
  2162 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | charlotte.vanhomwegen@ulb.be | NA
  2163 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | charlotte.vanhomwegen@ulb.be | NA
  2164 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | NA
  2165 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | NA
  2166 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | charlotte.vanhomwegen@ulb.be | NA
  2167 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | charlotte.vanhomwegen@ulb.be | NA
  2168 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | charlotte.vanhomwegen@ulb.be | NA
  2169 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2170 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2171 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | charlotte.vanhomwegen@ulb.be | NA
  2172 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2173 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | NA
  2174 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | charlotte.vanhomwegen@ulb.be | NA
  2175 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2176 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | NA
  2177 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | charlotte.vanhomwegen@ulb.be | NA
  2178 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2179 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2180 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2181 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2182 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | charlotte.vanhomwegen@ulb.be | NA
  2205 | GAPS_mothers | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | 17
  2206 | GAPS_mothers | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | 17
  2207 | GAPS_mothers | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | 17
  2208 | GAPS_mothers | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2209 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2210 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2211 | GAPS_mothers | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  2212 | GAPS_mothers | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  2213 | GAPS_mothers | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | 17
  2214 | GAPS_mothers | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | 17
  2215 | GAPS_mothers | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | 17
  2216 | GAPS_mothers | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | 17
  2217 | GAPS_mothers | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2218 | GAPS_mothers | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | 17
  2219 | GAPS_mothers | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2220 | GAPS_mothers | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2221 | GAPS_mothers | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2222 | GAPS_mothers | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2223 | GAPS_mothers | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2224 | GAPS_mothers | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2225 | GAPS_mothers | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2226 | GAPS_mothers | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2227 | MADI_P3_GAPS | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | 17
  2228 | MADI_P3_GAPS | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | 17
  2229 | MADI_P3_GAPS | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | 17
  2230 | MADI_P3_GAPS | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2231 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2232 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | 17
  2233 | MADI_P3_GAPS | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  2234 | MADI_P3_GAPS | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | 17
  2235 | MADI_P3_GAPS | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | 17
  2236 | MADI_P3_GAPS | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | 17
  2237 | MADI_P3_GAPS | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | 17
  2238 | MADI_P3_GAPS | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | 17
  2239 | MADI_P3_GAPS | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2240 | MADI_P3_GAPS | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | 17
  2241 | MADI_P3_GAPS | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | Sando | loicdedonck@gmail.com | 17
  2242 | MADI_P3_GAPS | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2243 | MADI_P3_GAPS | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | 17
  2244 | MADI_P3_GAPS | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2245 | MADI_P3_GAPS | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2246 | MADI_P3_GAPS | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2247 | MADI_P3_GAPS | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2248 | MADI_P3_GAPS | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | 17
  2249 | Opti_test | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | mscotzens@gmail.com | 4
  2250 | Opti_test | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | 4
  2251 | Opti_test | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 4
  2252 | Opti_test | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 4
  2253 | Opti_test | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 4
  2254 | Opti_test | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 4
  2255 | Opti_test | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 4
  2256 | Opti_test | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 4
  2257 | Opti_test | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | mscotzens@gmail.com | 4
  2258 | Opti_test | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | 4
  2259 | Opti_test | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | 4
  2260 | Opti_test | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | 4
  2261 | Opti_test | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 4
  2262 | Opti_test | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | 4
  2263 | Opti_test | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | sandoglobulin | mscotzens@gmail.com | 4
  2264 | Opti_test | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 4
  2265 | Opti_test | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 4
  2266 | Opti_test | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | mscotzens@gmail.com | 4
  2267 | Opti_test | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 4
  2268 | Opti_test | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | mscotzens@gmail.com | 4
  2269 | Opti_test | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 4
  2270 | Opti_test | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | mscotzens@gmail.com | 4
  2293 | MADI_P3_GAPS | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | mscotzens@gmail.com | 17
  2294 | MADI_P3_GAPS | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | mscotzens@gmail.com | 17
  2295 | MADI_P3_GAPS | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 17
  2296 | MADI_P3_GAPS | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2297 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2298 | MADI_P3_GAPS | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | mscotzens@gmail.com | 17
  2299 | MADI_P3_GAPS | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  2300 | MADI_P3_GAPS | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | mscotzens@gmail.com | 17
  2301 | MADI_P3_GAPS | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 20 | NA | NA | mscotzens@gmail.com | 17
  2302 | MADI_P3_GAPS | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | mscotzens@gmail.com | 17
  2303 | MADI_P3_GAPS | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | mscotzens@gmail.com | 17
  2304 | MADI_P3_GAPS | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | mscotzens@gmail.com | 17
  2305 | MADI_P3_GAPS | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | FALSE | NA | mscotzens@gmail.com | 17
  2306 | MADI_P3_GAPS | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | mscotzens@gmail.com | 17
  2307 | MADI_P3_GAPS | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC06_140 | mscotzens@gmail.com | 17
  2308 | MADI_P3_GAPS | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  2309 | MADI_P3_GAPS | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | mscotzens@gmail.com | 17
  2310 | MADI_P3_GAPS | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | TT_wP | mscotzens@gmail.com | 17
  2311 | MADI_P3_GAPS | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | pre1st,pre3rd,post3rd,post3rd5mo | mscotzens@gmail.com | 17
  2312 | MADI_P3_GAPS | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | pre1st,post3rd | mscotzens@gmail.com | 17
  2313 | MADI_P3_GAPS | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | All Antigens,Pertussis,polio | mscotzens@gmail.com | 17
  2314 | MADI_P3_GAPS | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | act,dt,pt,prn,ipv1,fim,ipv3,pentamer,fha,ipv2,tt | mscotzens@gmail.com | 17
  2315 | GAPS_infant | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | NA
  2316 | GAPS_infant | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | NA
  2317 | GAPS_infant | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | NA
  2318 | GAPS_infant | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2319 | GAPS_infant | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2320 | GAPS_infant | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2321 | GAPS_infant | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  2322 | GAPS_infant | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  2323 | GAPS_infant | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | NA
  2324 | GAPS_infant | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | NA
  2325 | GAPS_infant | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | NA
  2326 | GAPS_infant | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | NA
  2327 | GAPS_infant | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2328 | GAPS_infant | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | NA
  2329 | GAPS_infant | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC | loicdedonck@gmail.com | NA
  2330 | GAPS_infant | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2331 | GAPS_infant | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2332 | GAPS_infant | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2333 | GAPS_infant | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2334 | GAPS_infant | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2335 | GAPS_infant | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2336 | GAPS_infant | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2337 | GAPS_infant | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  2338 | GAPS_infant | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  2339 | GAPS_infant | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  2340 | GAPS_infant | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2341 | GAPS_infant | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2342 | GAPS_infant | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2343 | GAPS_infant | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2344 | GAPS_infant | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2345 | GAPS_infant | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2346 | GAPS_infant | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2347 | GAPS_infant | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  2348 | GAPS_infant | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2349 | GAPS_infant | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2350 | GAPS_infant | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  2351 | GAPS_infant | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC | seamus.owen.stein@dartmouth.edu | NA
  2352 | GAPS_infant | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2353 | GAPS_infant | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2354 | GAPS_infant | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2355 | GAPS_infant | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2356 | GAPS_infant | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2357 | GAPS_infant | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2358 | GAPS_infant | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2359 | GAPS_mothers | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 17
  2360 | GAPS_mothers | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 17
  2361 | GAPS_mothers | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 17
  2362 | GAPS_mothers | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2363 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2364 | GAPS_mothers | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 17
  2365 | GAPS_mothers | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  2366 | GAPS_mothers | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 17
  2367 | GAPS_mothers | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2368 | GAPS_mothers | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2369 | GAPS_mothers | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 17
  2370 | GAPS_mothers | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2371 | GAPS_mothers | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2372 | GAPS_mothers | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 17
  2373 | GAPS_mothers | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2374 | GAPS_mothers | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2375 | GAPS_mothers | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 17
  2376 | GAPS_mothers | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2377 | GAPS_mothers | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2378 | GAPS_mothers | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2379 | GAPS_mothers | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2380 | GAPS_mothers | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 17
  2381 | GAPS_infants | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | loicdedonck@gmail.com | NA
  2382 | GAPS_infants | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | loicdedonck@gmail.com | NA
  2383 | GAPS_infants | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | loicdedonck@gmail.com | NA
  2384 | GAPS_infants | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2385 | GAPS_infants | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2386 | GAPS_infants | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | loicdedonck@gmail.com | NA
  2387 | GAPS_infants | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  2388 | GAPS_infants | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | loicdedonck@gmail.com | NA
  2389 | GAPS_infants | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | loicdedonck@gmail.com | NA
  2390 | GAPS_infants | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | loicdedonck@gmail.com | NA
  2391 | GAPS_infants | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | loicdedonck@gmail.com | NA
  2392 | GAPS_infants | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | loicdedonck@gmail.com | NA
  2393 | GAPS_infants | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2394 | GAPS_infants | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | loicdedonck@gmail.com | NA
  2395 | GAPS_infants | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC | loicdedonck@gmail.com | NA
  2396 | GAPS_infants | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2397 | GAPS_infants | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | loicdedonck@gmail.com | NA
  2398 | GAPS_infants | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2399 | GAPS_infants | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2400 | GAPS_infants | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2401 | GAPS_infants | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2402 | GAPS_infants | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | loicdedonck@gmail.com | NA
  2403 | GAPS_infants | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  2404 | GAPS_infants | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  2405 | GAPS_infants | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  2406 | GAPS_infants | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2407 | GAPS_infants | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2408 | GAPS_infants | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2409 | GAPS_infants | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2410 | GAPS_infants | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2411 | GAPS_infants | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2412 | GAPS_infants | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2413 | GAPS_infants | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  2414 | GAPS_infants | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2415 | GAPS_infants | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2416 | GAPS_infants | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  2417 | GAPS_infants | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC | seamus.owen.stein@dartmouth.edu | NA
  2418 | GAPS_infants | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2419 | GAPS_infants | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2420 | GAPS_infants | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2421 | GAPS_infants | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2422 | GAPS_infants | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2423 | GAPS_infants | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2424 | GAPS_infants | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2425 | istudy | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2426 | istudy | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2427 | istudy | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2428 | istudy | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2429 | istudy | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2430 | istudy | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2431 | istudy | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2432 | istudy | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2433 | istudy | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2434 | istudy | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2435 | istudy | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2436 | istudy | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2437 | istudy | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2438 | istudy | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2439 | istudy | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2440 | istudy | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2441 | istudy | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2442 | istudy | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2443 | istudy | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2444 | istudy | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2445 | istudy | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2446 | istudy | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2469 | vaccinetests | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  2470 | vaccinetests | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  2471 | vaccinetests | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  2472 | vaccinetests | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2473 | vaccinetests | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2474 | vaccinetests | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2475 | vaccinetests | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2476 | vaccinetests | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2477 | vaccinetests | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2478 | vaccinetests | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2479 | vaccinetests | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  2480 | vaccinetests | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2481 | vaccinetests | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2482 | vaccinetests | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  2483 | vaccinetests | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2484 | vaccinetests | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2485 | vaccinetests | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2486 | vaccinetests | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2487 | vaccinetests | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2488 | vaccinetests | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2489 | vaccinetests | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2490 | vaccinetests | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2491 | sostudy | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2492 | sostudy | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2493 | sostudy | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2494 | sostudy | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2495 | sostudy | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2496 | sostudy | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2497 | sostudy | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2498 | sostudy | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2499 | sostudy | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2500 | sostudy | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2501 | sostudy | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2502 | sostudy | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2503 | sostudy | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2504 | sostudy | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2505 | sostudy | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2506 | sostudy | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2507 | sostudy | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2508 | sostudy | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2509 | sostudy | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2510 | sostudy | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2511 | sostudy | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2512 | sostudy | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2513 | newst | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2514 | newst | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2515 | newst | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2516 | newst | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2517 | newst | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2518 | newst | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2519 | newst | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2520 | newst | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2521 | newst | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2522 | newst | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2523 | newst | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2524 | newst | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2525 | newst | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2526 | newst | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2527 | newst | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2528 | newst | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2529 | newst | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2530 | newst | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2531 | newst | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2532 | newst | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2533 | newst | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2534 | newst | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2535 | val | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2536 | val | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2537 | val | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2538 | val | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2539 | val | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2540 | val | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2541 | val | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2542 | val | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2543 | val | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2544 | val | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2545 | val | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2546 | val | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2547 | val | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2548 | val | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2549 | val | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2550 | val | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2551 | val | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2552 | val | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2553 | val | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2554 | val | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2555 | val | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2556 | val | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2557 | nt | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2558 | nt | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2559 | nt | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2560 | nt | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2561 | nt | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2562 | nt | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2563 | nt | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2564 | nt | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2565 | nt | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2566 | nt | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2567 | nt | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2568 | nt | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2569 | nt | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2570 | nt | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2571 | nt | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2572 | nt | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2573 | nt | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2574 | nt | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2575 | nt | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2576 | nt | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2577 | nt | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2578 | nt | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2579 | sea | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2580 | sea | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2581 | sea | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2582 | sea | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2583 | sea | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2584 | sea | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2585 | sea | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2586 | sea | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2587 | sea | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2588 | sea | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2589 | sea | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2590 | sea | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2591 | sea | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2592 | sea | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2593 | sea | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2594 | sea | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2595 | sea | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2596 | sea | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2597 | sea | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2598 | sea | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2599 | sea | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2600 | sea | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2601 | is | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2602 | is | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2603 | is | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2604 | is | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2605 | is | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2606 | is | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2607 | is | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2608 | is | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2609 | is | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2610 | is | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2611 | is | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2612 | is | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2613 | is | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2614 | is | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2615 | is | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2616 | is | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2617 | is | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2618 | is | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2619 | is | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2620 | is | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2621 | is | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2622 | is | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2623 | d | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 53
  2624 | d | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 53
  2625 | d | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 53
  2626 | d | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 53
  2627 | d | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 53
  2628 | d | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 53
  2629 | d | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 53
  2630 | d | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 53
  2631 | d | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2632 | d | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2633 | d | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 53
  2634 | d | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2635 | d | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 53
  2636 | d | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 53
  2637 | d | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2638 | d | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 53
  2639 | d | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 53
  2640 | d | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2641 | d | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2642 | d | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2643 | d | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2644 | d | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 53
  2645 | e | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2646 | e | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2647 | e | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2648 | e | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2649 | e | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2650 | e | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2651 | e | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2652 | e | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2653 | e | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2654 | e | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2655 | e | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2656 | e | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2657 | e | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2658 | e | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2659 | e | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2660 | e | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2661 | e | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2662 | e | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2663 | e | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2664 | e | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2665 | e | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2666 | e | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2667 | y | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 43
  2668 | y | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 43
  2669 | y | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 43
  2670 | y | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2671 | y | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2672 | y | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 43
  2673 | y | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2674 | y | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 43
  2675 | y | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2676 | y | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2677 | y | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 43
  2678 | y | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2679 | y | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2680 | y | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 43
  2681 | y | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2682 | y | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2683 | y | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 43
  2684 | y | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2685 | y | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2686 | y | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2687 | y | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2688 | y | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 43
  2689 | stone | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2690 | stone | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2691 | stone | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2692 | stone | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2693 | stone | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2694 | stone | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2695 | stone | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2696 | stone | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2697 | stone | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2698 | stone | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2699 | stone | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2700 | stone | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2701 | stone | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2702 | stone | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2703 | stone | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC06 | seamus.owen.stein@dartmouth.edu | 44
  2704 | stone | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2705 | stone | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2706 | stone | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2707 | stone | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2708 | stone | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2709 | stone | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2710 | stone | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2711 | stone2 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2712 | stone2 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2713 | stone2 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2714 | stone2 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2715 | stone2 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2716 | stone2 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2717 | stone2 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2718 | stone2 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2719 | stone2 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2720 | stone2 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2721 | stone2 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2722 | stone2 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2723 | stone2 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2724 | stone2 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2725 | stone2 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2726 | stone2 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2727 | stone2 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2728 | stone2 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2729 | stone2 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2730 | stone2 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2731 | stone2 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2732 | stone2 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2733 | stone3 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2734 | stone3 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2735 | stone3 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2736 | stone3 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2737 | stone3 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2738 | stone3 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2739 | stone3 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2740 | stone3 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2741 | stone3 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2742 | stone3 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2743 | stone3 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2744 | stone3 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2745 | stone3 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2746 | stone3 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2747 | stone3 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2748 | stone3 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2749 | stone3 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2750 | stone3 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2751 | stone3 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2752 | stone3 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2753 | stone3 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2754 | stone3 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2755 | st_test | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2756 | st_test | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2757 | st_test | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2758 | st_test | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2759 | st_test | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2760 | st_test | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2761 | st_test | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2762 | st_test | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2763 | st_test | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2764 | st_test | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2765 | st_test | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2766 | st_test | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2767 | st_test | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2768 | st_test | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2769 | st_test | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2770 | st_test | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2771 | st_test | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2772 | st_test | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2773 | st_test | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2774 | st_test | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2775 | st_test | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2776 | st_test | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2777 | st_test2 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2778 | st_test2 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2779 | st_test2 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2780 | st_test2 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2781 | st_test2 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2782 | st_test2 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2783 | st_test2 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2784 | st_test2 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2785 | st_test2 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2786 | st_test2 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2787 | st_test2 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2788 | st_test2 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2789 | st_test2 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2790 | st_test2 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2791 | st_test2 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2792 | st_test2 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2793 | st_test2 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2794 | st_test2 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2795 | st_test2 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2796 | st_test2 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2797 | st_test2 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2798 | st_test2 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2799 | st_test4 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2800 | st_test4 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2801 | st_test4 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2802 | st_test4 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2803 | st_test4 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2804 | st_test4 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2805 | st_test4 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2806 | st_test4 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2807 | st_test4 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2808 | st_test4 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2809 | st_test4 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2810 | st_test4 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2811 | st_test4 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2812 | st_test4 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2813 | st_test4 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2814 | st_test4 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2815 | st_test4 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2816 | st_test4 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2817 | st_test4 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2818 | st_test4 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2819 | st_test4 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2820 | st_test4 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2821 | stest | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2822 | stest | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2823 | stest | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2824 | stest | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2825 | stest | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2826 | stest | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2827 | stest | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2828 | stest | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2829 | stest | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2830 | stest | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2831 | stest | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2832 | stest | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2833 | stest | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2834 | stest | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2835 | stest | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2836 | stest | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2837 | stest | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2838 | stest | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2839 | stest | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2840 | stest | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2841 | stest | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2842 | stest | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2843 | stest2 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2844 | stest2 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2845 | stest2 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2846 | stest2 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2847 | stest2 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2848 | stest2 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2849 | stest2 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2850 | stest2 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2851 | stest2 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2852 | stest2 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2853 | stest2 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2854 | stest2 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2855 | stest2 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2856 | stest2 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2857 | stest2 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2858 | stest2 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2859 | stest2 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2860 | stest2 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2861 | stest2 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2862 | stest2 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2863 | stest2 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2864 | stest2 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2865 | s_docker | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2866 | s_docker | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2867 | s_docker | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2868 | s_docker | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2869 | s_docker | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2870 | s_docker | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2871 | s_docker | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2872 | s_docker | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2873 | s_docker | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2874 | s_docker | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2875 | s_docker | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2876 | s_docker | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2877 | s_docker | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2878 | s_docker | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2879 | s_docker | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2880 | s_docker | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2881 | s_docker | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2882 | s_docker | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2883 | s_docker | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2884 | s_docker | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2885 | s_docker | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2886 | s_docker | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2887 | s_docker2 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 44
  2888 | s_docker2 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 44
  2889 | s_docker2 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 44
  2890 | s_docker2 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2891 | s_docker2 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2892 | s_docker2 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 44
  2893 | s_docker2 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2894 | s_docker2 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 44
  2895 | s_docker2 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2896 | s_docker2 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2897 | s_docker2 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 44
  2898 | s_docker2 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2899 | s_docker2 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2900 | s_docker2 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 44
  2901 | s_docker2 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2902 | s_docker2 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2903 | s_docker2 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 44
  2904 | s_docker2 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2905 | s_docker2 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2906 | s_docker2 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2907 | s_docker2 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2908 | s_docker2 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 44
  2909 | teststudy | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  2910 | teststudy | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  2911 | teststudy | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  2912 | teststudy | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2913 | teststudy | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2914 | teststudy | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  2915 | teststudy | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2916 | teststudy | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  2917 | teststudy | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2918 | teststudy | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2919 | teststudy | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  2920 | teststudy | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2921 | teststudy | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2922 | teststudy | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  2923 | teststudy | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2924 | teststudy | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2925 | teststudy | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  2926 | teststudy | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2927 | teststudy | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2928 | teststudy | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2929 | teststudy | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2930 | teststudy | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  2931 |  | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | scottest@gmail.com | NA
  2932 |  | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | scottest@gmail.com | NA
  2933 |  | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | scottest@gmail.com | NA
  2934 |  | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | NA
  2935 |  | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | NA
  2936 |  | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | NA
  2937 |  | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | scottest@gmail.com | NA
  2938 |  | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | scottest@gmail.com | NA
  2939 |  | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | scottest@gmail.com | NA
  2940 |  | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | scottest@gmail.com | NA
  2941 |  | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | scottest@gmail.com | NA
  2942 |  | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | scottest@gmail.com | NA
  2943 |  | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | NA
  2944 |  | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | scottest@gmail.com | NA
  2945 |  | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | scottest@gmail.com | NA
  2946 |  | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | NA
  2947 |  | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | NA
  2948 |  | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | scottest@gmail.com | NA
  2949 |  | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | scottest@gmail.com | NA
  2950 |  | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | scottest@gmail.com | NA
  2951 |  | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | scottest@gmail.com | NA
  2952 |  | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | scottest@gmail.com | NA
  2953 | Gutter_testing | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | scottest@gmail.com | 45
  2954 | Gutter_testing | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | scottest@gmail.com | 45
  2955 | Gutter_testing | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | scottest@gmail.com | 45
  2956 | Gutter_testing | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | 45
  2957 | Gutter_testing | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | 45
  2958 | Gutter_testing | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | scottest@gmail.com | 45
  2959 | Gutter_testing | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | scottest@gmail.com | 45
  2960 | Gutter_testing | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | scottest@gmail.com | 45
  2961 | Gutter_testing | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | scottest@gmail.com | 45
  2962 | Gutter_testing | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | scottest@gmail.com | 45
  2963 | Gutter_testing | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | scottest@gmail.com | 45
  2964 | Gutter_testing | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | scottest@gmail.com | 45
  2965 | Gutter_testing | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | 45
  2966 | Gutter_testing | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | scottest@gmail.com | 45
  2967 | Gutter_testing | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NIBSC06140 | scottest@gmail.com | 45
  2968 | Gutter_testing | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | 45
  2969 | Gutter_testing | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | scottest@gmail.com | 45
  2970 | Gutter_testing | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | scottest@gmail.com | 45
  2971 | Gutter_testing | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | scottest@gmail.com | 45
  2972 | Gutter_testing | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | scottest@gmail.com | 45
  2973 | Gutter_testing | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | All Antigens | scottest@gmail.com | 45
  2974 | Gutter_testing | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | act_42,dt_78,fha_27,fim_15,ipv1_19,ipv2_64,ipv3_36,pentamer_12,prn_30,pt_75,t... | scottest@gmail.com | 45
  2975 | Gutter_testing | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 45
  2976 | Gutter_testing | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 45
  2977 | Gutter_testing | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 45
  2978 | Gutter_testing | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  2979 | Gutter_testing | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  2980 | Gutter_testing | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  2981 | Gutter_testing | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  2982 | Gutter_testing | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  2983 | Gutter_testing | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2984 | Gutter_testing | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2985 | Gutter_testing | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 45
  2986 | Gutter_testing | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2987 | Gutter_testing | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  2988 | Gutter_testing | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | included | seamus.owen.stein@dartmouth.edu | 45
  2989 | Gutter_testing | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | SD | seamus.owen.stein@dartmouth.edu | 45
  2990 | Gutter_testing | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  2991 | Gutter_testing | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  2992 | Gutter_testing | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2993 | Gutter_testing | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2994 | Gutter_testing | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2995 | Gutter_testing | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2996 | Gutter_testing | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  2997 | mytest23r432145 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | NA
  2998 | mytest23r432145 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | NA
  2999 | mytest23r432145 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | NA
  3000 | mytest23r432145 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  3001 | mytest23r432145 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  3002 | mytest23r432145 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | NA
  3003 | mytest23r432145 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  3004 | mytest23r432145 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | NA
  3005 | mytest23r432145 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3006 | mytest23r432145 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3007 | mytest23r432145 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | NA
  3008 | mytest23r432145 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3009 | mytest23r432145 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  3010 | mytest23r432145 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | NA
  3011 | mytest23r432145 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3012 | mytest23r432145 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  3013 | mytest23r432145 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | NA
  3014 | mytest23r432145 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3015 | mytest23r432145 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3016 | mytest23r432145 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3017 | mytest23r432145 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3018 | mytest23r432145 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | NA
  3019 | test2 | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 45
  3020 | test2 | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 45
  3021 | test2 | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 45
  3022 | test2 | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3023 | test2 | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3024 | test2 | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3025 | test2 | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3026 | test2 | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3027 | test2 | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3028 | test2 | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3029 | test2 | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 45
  3030 | test2 | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3031 | test2 | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3032 | test2 | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 45
  3033 | test2 | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3034 | test2 | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3035 | test2 | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3036 | test2 | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3037 | test2 | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3038 | test2 | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3039 | test2 | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3040 | test2 | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3041 | testim | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 45
  3042 | testim | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 45
  3043 | testim | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 45
  3044 | testim | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3045 | testim | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3046 | testim | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3047 | testim | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3048 | testim | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3049 | testim | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3050 | testim | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3051 | testim | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 45
  3052 | testim | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3053 | testim | standard_curve_options | mean_mfi | Compute Mean MFI at each Dilution Factor | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3054 | testim | standard_curve_options | blank_option | Blank Control: | string | 35 | radioButtons | ignored,included,subtracted,subtracted_3x,subtracted_10x | NA | NA | ignored | seamus.owen.stein@dartmouth.edu | 45
  3055 | testim | standard_curve_options | default_source | Source: | string | 35 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3056 | testim | standard_curve_options | is_log_mfi_axis | Use Log Units for MFI | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3057 | testim | standard_curve_options | applyProzone | Apply Prozone Correction | boolean | 35 | switchInput | TRUE,FALSE | NA | TRUE | NA | seamus.owen.stein@dartmouth.edu | 45
  3058 | testim | subgroup_settings | reference_arm | Reference Level: | string | 128 | radioButtons | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3059 | testim | subgroup_settings | timeperiod_order | Time Period Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3060 | testim | subgroup_settings | primary_timeperiod_comparison |  Compare Two Time Periods: | string | 128 | selectInput,Multiple | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3061 | testim | antigen_family | antigen_family_order | Antigen Family Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3062 | testim | antigen_family | antigen_order | Antigen Order: | string | 128 | orderInput | NULL | NA | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3063 | brstudy | dilution_analysis | node_order | Type of Sample Limit | string | 20 | selectInput, multiple | limits_of_detection,limits_of_quantification,linear_region | NA | NA | linear_region | seamus.owen.stein@dartmouth.edu | 45
  3064 | brstudy | dilution_analysis | valid_gate_class | Passing Limit of Detection | string | 20 | selectInput | Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_... | NA | NA | Between_Limits_of_Detection | seamus.owen.stein@dartmouth.edu | 45
  3065 | brstudy | dilution_analysis | is_binary_gc | Use Passing Limit of Detection as binary | boolean | 9 | checkbox | TRUE,FALSE | NA | FALSE | NA | seamus.owen.stein@dartmouth.edu | 45
  3066 | brstudy | dilution_analysis | zero_pass_diluted_Tx | Zero Passing Dilutions Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3067 | brstudy | dilution_analysis | zero_pass_concentrated_Tx | Zero Passing Dilutions Too Concentrated AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3068 | brstudy | dilution_analysis | zero_pass_concentrated_diluted_Tx | Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | all_au | seamus.owen.stein@dartmouth.edu | 45
  3069 | brstudy | dilution_analysis | one_pass_acceptable_Tx | One Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3070 | brstudy | dilution_analysis | two_plus_pass_acceptable_Tx | Two or More Passing Dilution AU Treatment: | string | 20 | radioButtons | all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au | NA | NA | geom_passing_au | seamus.owen.stein@dartmouth.edu | 45
  3071 | brstudy | bead_count | lower_bc_threshold | Lower Threshold | numeric | 9 | numericInput | NA,Inf | 35 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3072 | brstudy | bead_count | upper_bc_threshold | Upper Threshold | numeric | 9 | numericInput | NA,Inf | 50 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  3073 | brstudy | bead_count | failed_well_criteria | Failed Well Criteria | categorical | 35 | radioButtons | Below_Upper_Threshold,Below_Lower_Threshold | NA | NA | Below_Lower_Threshold | seamus.owen.stein@dartmouth.edu | 45
  3074 | brstudy | bead_count | pct_agg_threshold | Aggregate Beads Threshold | numeric | 9 | numericInput | NA,Inf | 30 | NA | NA | seamus.owen.stein@dartmouth.edu | 45
  ... (truncated at 2000 rows)

## 6. Long / expression-like values (character cols > 40 chars)
  -- column: param_label
     Zero Passing Dilutions Too Concentrated and Too Diluted AU Treatment:
     Two or More Passing Dilution AU Treatment:
     Zero Passing Dilutions Too Concentrated AU Treatment:
     Zero Passing Dilutions Too Diluted AU Treatment:
  -- column: param_choices_list
     ignored,included,subtracted,subtracted_3x,subtracted_10x
     all_au,passing_au,geom_all_au,geom_passing_au,replace_blank,exclude_au
     limits_of_detection,limits_of_quantification,linear_region
     Below_Upper_Threshold,Below_Lower_Threshold
     limits_of_detection,limits_of_quantification
     Between_Limits_of_Detection,Above_Upper_Limit_of_Detection,Below_Lower_Limit_of_Detection
  -- column: param_character_value
     a27l_21,a35r_12,b16r_75,b2r_25,b6r_15,bsa_42,e8l_20,g_b_45,h3l_22,m1r_36
     a_darwin_14,hong_kong_55,cambodia_63,b_austria_18,cg2_61,fha_27,g_b_12,ipv_123_65,ipv1_38,pentamer_15,prn_30,pt_25,s2_45,rbd_42,cg1_20,tt_21,victoria_57,washington_72,guangdong_53,phuket_78,s1_48,ipv3
     A_Victoria_HA,A_Hong.Kong_HA,B_Austria_HA,B_Washington_HA,TT,A_California_HA,A_Wisconsin_HA,B_Phuket_HA,A_Singapore_HA,gB,A_Hong.Kong_NA,A_California_NA,A_Tasmania_HA,A_Darwin_HA,A_Massachusetts_HA,A_
     TT,B_Phuket_HA,A_Singapore_HA,gB,A_California_NA,A_Victoria_HA,A_Hong.Kong_HA,B_Austria_HA,B_Washington_HA,A_California_HA,A_Wisconsin_HA,A_Hong.Kong_NA,A_Tasmania_HA,A_Darwin_HA,A_Massachusetts_HA,A_
     Influenza,CMV,TT,B.pertussis,Polio,SARS-CoV-2
     pertactin_30,tt_21,cmv_g_b_12,cmv_pentamer_15,pt_25,rsv_pre_f_19,rsv_post_f_14,fha_27
     S1,Victoria,Cambodia,Washington,RBD,Guangdong,TT,S2,IPV.123,IPV2,CG1,gB,B_Austria,A_Darwin,IPV1,FHA,Hong.kong,Phuket,Pentamer,PRN,CG2,IPV3,PT
     act,dt,pt,prn,ipv1,fim,ipv3,pentamer,fha,ipv2,tt
     A_Victoria_HA,A_Hong.Kong_HA,B_Austria_HA,B_Washington_HA,TT,A_Wisconsin_HA,A_California_HA,B_Phuket_HA,A_Singapore_HA,gB,A_Hong.Kong_NA,A_California_NA,A_Tasmania_HA,A_Darwin_HA,A_Massachusetts_HA,A_
     ACT,DT,FHA,Fim,IPV1,IPV2,IPV3,Pentamer,PRN,PT,TT
     CMV,Pertussis,SARS-CoV-2,TT,B/Victoria,H1N1,B/Yamagata,Polio,H3N2
     cmv_g_b_12,cmv_pentamer_15,fha_27,pertactin_30,pt_25,rsv_post_f_14,rsv_pre_f_19,tt_21
     vp1,t_ag_22,s_t_ag_28,agnoprotein_42,pentamer_cmv_15,pentamer_15,tt_21,pt_75
     T0,post3rddose(+1mo),pre1stdose(2mo),pre3rddose(4mo),post3rddose(+5mo)
     s2,tt_21,pt_25,a_darwin_14,cg1_20,cambodia_new_36,victoria_57,ipv2,phuket_78,ipv3_67,pentamer_15,washington_72,hong_kong_55,g_b_12,s1_48,cg2_61,hong_kong,rbd_42,s2_45,b_austria_18,guangdong_53,prn_30,
     H3N2,SARS-CoV-2,B/Victoria,CMV,Pertussis,Polio,TT,H1N1,B/Yamagata
     fim,act,dt,pentamer,tt,prn,pt,ipv1,fha,ipv2,ipv3
     PT,PRN,FHA,Fim,DT,TT,ACT,IPV1,IPV2,IPV3,Pentamer
     act_42,dt_78,fha_27,fim_15,ipv1_19,ipv2_64,ipv3_36,pentamer_12,prn_30,pt_75,tt_21
     A_Brisbane_HA,A_Darwin_HA,A_California_HA,A_California_NA,A_Hong.Kong_HA,A_Hong.Kong_NA,A_Massachusetts_HA,A_Singapore_HA,A_Tasmania_HA,A_Victoria_HA,A_Wisconsin_HA,B_Austria_HA,B_Phuket_HA,B_Washingt
     All Antigens,H3N2,SARS-CoV-2,Polio,B/Victoria,CMV,Pertussis,TT,H1N1,B/Yamagata
     a_darwin_14,cambodia_63,b_austria_18,hong_kong_55,cg2_61,fha_27,g_b_12,ipv_123_65,ipv1_38,pentamer_15,prn_30,pt_25,s2_45,rbd_42,cg1_20,tt_21,victoria_57,washington_72,guangdong_53,phuket_78,s1_48,ipv3
     DT,vp1,pentamer_cmv_15,pentamer_15,agnoprotein_42,t_ag_22,tt_21,pt_75,s_t_ag_28
     vp1,DT,pentamer_cmv_15,pentamer_15,agnoprotein_42,t_ag_22,tt_21,pt_75,s_t_ag_28
