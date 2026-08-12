## Subgroup Detection set up
###specify maximum number of plot colors:

clust.colors <- c("#E69F00",  "#CC79A7",  "#D31E00", "#FB894B") # no more than 4

# class colors normal and low at time 0
t0_class_colors <-  c("#009E73", "#56B4E9")

# one color for each covariate level
covar.boxplot.color_4 <- c("#E04C5C", "#86B3B6", "#23AECE", "#E9A3C9")
covar.boxplot.color_3<-c(covar.boxplot.color_4[1:3])
covar.boxplot.color_2<-c(covar.boxplot.color_4[1:2])

# repeat color for each covariate level
covar.crossbar_4 <- c("#E04C5C","#E04C5C","#86B3B6","#86B3B6", "#23AECE","#23AECE", "#E9A3C9", "#E9A3C9")
covar.crossbar_3<-c(covar.crossbar_4[1:6])
covar.crossbar_2<-c(covar.crossbar_4[1:4])

# one color for each covariate level
manual.color = covar.boxplot.color_3

# repeat color for each covariate level
color.crossbar <- covar.crossbar_2

# one color for each visit (time point)
visit.boxplot.color <- c("#4D1836", "#91CF60")

# repeat color for each covariate level
visit.crossbar_4<-c("#4D1836", "#4D1836","#4D1836", "#4D1836", "#91CF60", "#91CF60","#91CF60","#91CF60")
visit.crossbar_3<-c("#4D1836", "#4D1836","#4D1836","#91CF60", "#91CF60","#91CF60")
visit.crossbar_2<-c("#4D1836", "#4D1836","#91CF60", "#91CF60")


## custom distance fx
signed_euclidean <- function(x,y){
  diff_sq <- (x-y)^2
  return_val <- ifelse(x > y, sqrt(sum(diff_sq)) * -1, sqrt(sum(diff_sq)))
  return(return_val)
}


## fx to apply distance fx
##  Function to find number of clusters to use in k means using gap statistic or the elbow method.
find_k_cluster <- function(res2) {
  # Scale the data for kmeans finding optimal k and get distinct data
  #kmeans_data <-scale(res2$dist.eu)
  kmeans_data <- as.matrix(res2$dist.eu)
  kmeans_data <- unique(kmeans_data)

  if (nrow(kmeans_data) < 3) {
    warning("Not enough unique data points for meaningful clustering. Returning 1 cluster.")
    return(setNames(c(1), "Not_enough_unique_subjects"))
  }

  silhouette_method <- fviz_nbclust(kmeans_data, kmeans, method = "silhouette", k.max = nrow(kmeans_data) -1)
  optimal_k_silhouette <- as.numeric(silhouette_method$data$clusters[which.max(silhouette_method$data$y)])


  gap_statisitc <- clusGap(kmeans_data, FUN = kmeans, nstart = 25,
                           K.max = nrow(kmeans_data) -1, B = 50)

  print(gap_statisitc)
  optimal_k_gap_statistic <- maxSE(gap_statisitc$Tab[, "gap"], gap_statisitc$Tab[, "SE.sim"])

  # ensure that no cluster is above 4 and if they all are above 4 return 1 cluster. If equal denote that.
  optimal_k_vector <- setNames(c(optimal_k_silhouette, optimal_k_gap_statistic), c("silhoute", "gap"))
  optimal_k_vector <- optimal_k_vector[optimal_k_vector <= 4]
  if (length(optimal_k_vector) == 0) {
    optimal_k_vector <- setNames(c(1), c("Min_k"))
    return(optimal_k_vector)
  }
  #  else if (as.numeric(optimal_k_vector["silhoute"]) == as.numeric(optimal_k_vector["gap"])) {
  #     k <- as.numeric(optimal_k_vector["silhoute"])
  #     optimal_k_equal <- setNames(c(k), c("silhouette_gap_equal"))
  #    return(optimal_k_equal)
  else {
    optimal_k_max <- optimal_k_vector[which.max(optimal_k_vector)]
    return(optimal_k_max)

  }
}

# ncluster was an argument
subject_feature_clustering <- function(input_data, time1, time2,
                                       dist_fx,feature,antigen){
  # input_data <- formt1t2
  # time1 <- t0
  # time2 <- t1
  # dist_fx <- signed_euclidean
  # feature <- "IgG3"
  allres <- list()
  # input_data_view <<- input_data
  ## filter datasets to antigen and making it wider (change to input data not form1t2)
  sub1 <- input_data[input_data$timeperiod == time1, c("best_sample_se_all_id", "subject_accession", "antigen", "agroup",
                                                       "feature","log_assay_value")]
  sub2 <- input_data[input_data$timeperiod == time2, c("best_sample_se_all_id", "subject_accession", "antigen","agroup",
                                                       "feature","log_assay_value")]
  # ensure there are no duplicates
  sub1 <- sub1 %>% distinct(subject_accession, agroup, feature, .keep_all = TRUE)
  sub2 <- sub2 %>% distinct(subject_accession, agroup, feature, .keep_all = TRUE)

  cat("before sub1")
  cat("names_sub1\n")
  print(names(sub1)) #c("subject_accession", "arm_name") # need subject accession and arm name
  sub1 <- pivot_wider(sub1, id_cols = c("best_sample_se_all_id", "subject_accession", "agroup") ,names_from = "feature"
                      , values_from = "log_assay_value")

  cat("before sub2")
  sub2_view <- sub2
  sub2 <- pivot_wider(sub2,id_cols = c("best_sample_se_all_id", "subject_accession", "agroup"), names_from = "feature"
                      , values_from = "log_assay_value")

  cat("after sub2")
  sub1 <- na.omit(sub1)
  sub2 <- na.omit(sub2)


  # check that feature exist in df before subsetting
  needed_cols <- c("subject_accession", "agroup", feature)
  if (!all(needed_cols %in% names(sub1)) || !all(needed_cols %in% names(sub2))) {
     return(NULL)
  }

  x <- sub1[,c('subject_accession','agroup',feature)]
  y <- sub2[,c('subject_accession','agroup',feature)]
  #  cat("x:")
  # # x_view <<- x
  #  print(names(x))
  #  print(class(x))
  #  print(sapply(x, class))
  #
  #  cat("y:\n")
  # # y_view <<- y
  #  print(names(y))
  #  print(class(y))
  #  print(sapply(y, class))

  # flatten list columns
  # if (any(sapply(x, is.list))) {
  #   x <- data.frame(lapply(x, function(col) if (is.list(col)) unlist(col) else col))
  # }
  #
  # if (any(sapply(y, is.list))) {
  #   y <- data.frame(lapply(y, function(col) if (is.list(col)) unlist(col) else col))
  # }


  x <- x[complete.cases(x),]
  y <- y[complete.cases(y),]

  y <- y %>% filter(subject_accession %in% x$subject_accession)

  #cat("after complete_cases")

  # ensure no duplicates for subject accession to ensure each subject is proccesed once.
  x <- distinct(x)
  y <- distinct(y)

  # print(head(x))
  # print(head(y))
  x_view <- x
  res <- data.frame()
  for(subject in unique(x[["subject_accession"]])){
    # print(subject)
    # print(length(x$arm_name[x$subject_accession == subject]))
    #print(subject)

    #   print(head(x[x$subject_accession == subject, paste0(feature)]))

    # print("x class")
    # print(class(x[x$subject_accession == subject,paste0(feature)]))
    # print(str(x[x$subject_accession == subject, paste0(feature)]))
    # print("y class")
    # print(class(y[y$subject_accession == subject,paste0(feature)]))
    # print(str(y[y$subject_accession == subject, paste0(feature)]))


    x.1 <- as.numeric(x[x$subject_accession == subject,paste0(feature)])
    y.1 <- as.numeric(y[y$subject_accession == subject,paste0(feature)])
    x.y <- as.numeric(proxy::dist(x = x.1, y = y.1,method = dist_fx))
    # cat("x.1 dim")
    # print(str(x.1))
    # cat("y.1 dim")
    # cat(str(dim(y.1)))
    # cat("x.y dim")
    # print(str(x.y))

    samp <- data.frame(subject_accession = subject,
                       agroup = x$agroup[x$subject_accession==subject],
                       feature = feature,
                       time1 = x.1,
                       time2 = y.1,
                       dist.eu = x.y)
    res <- rbind(res,samp)
    res2 <- na.omit(res)
  }

  #   for (subject in unique(x$subject_accession)) {
  #      x.1 <<- as.numeric(x[x$subject_accession == subject, 3][[1]])
  #      y.1 <<- as.numeric(y[y$subject_accession == subject, 3][[1]])
  #      x.y <<- as.numeric(proxy::dist(x = x.1, y = y.1,method = dist_fx))
  #      # samp <- data.frame(subject_accession = subject,
  #      #                                         arm_name = x$arm_name[x$subject_accession==subject],
  #      #                                         feature = feature,
  #      #                                         time1 = x.1,
  #      #                                         time2 = y.1,
  #      #                                         dist.eu = x.y)
  #      #                      res <- rbind(res,samp)
  #      #                      res2 <- na.omit(res)
  # }

  ## kmeans clustering
  # euclidian <- res2$dis.eu
  #
  # if (is.null(euclidian)) {
  #   cat("The data has less rows then the number of clusters")
  # }
  # res2_view <<- res2
  # nrow_dist <<- nrow(res2)

  # cat("before res2")
  # print(res2)
  # cat("nrows of res2")
  # print(nrow(res2))
  # check to see if there are data for clustering
  if (nrow(res2) == 0) {
    cat("There are no rows for clustering")
    # res2row(nrow(res2))

    #return(NULL)
  }
  #  res2_view <<- res2
  # determine the optimal number of clusters via GAP statistic or silhouette_method
  ncluster <- find_k_cluster(res2 = res2)
  cat("ncluster\n")
  print(ncluster)
  if (as.numeric(ncluster) == 1) {
    message("assigning all observations to single cluster")
    res2$kmeans_cluster <- factor(1)
  } else {
    clusters <- kmeans(res2$dist.eu, centers = as.numeric(ncluster))
    cat("after kmeans function")
    cluster_order <- order(clusters$centers)
    res2$kmeans_cluster <- factor(clusters$cluster,
                                  labels = c(1:ncluster),levels = cluster_order)
  }

  epsilon <- 1e-10  # Small value to prevent division by zero or log(0)

  res2$diffratio <- as.numeric(lapply(res2$kmeans_cluster, function(x) {
    raw_diff <- log(mean(res2$time2[res2$kmeans_cluster == x]) / mean(res2$time1[res2$kmeans_cluster == x]))
    if (is.nan(raw_diff)){
      time1_mean <- mean(res2$time1[res2$kmeans_cluster == x], na.rm = T)
      time2_mean <- mean(res2$time2[res2$kmeans_cluster == x], na.rm = T)

      # Replace zero or NA values with epsilon
      time1_mean <- ifelse(is.na(time1_mean) | time1_mean <= 0, epsilon, time1_mean)
      time2_mean <- ifelse(is.na(time2_mean) | time2_mean <= 0, epsilon, time2_mean)

      return(log(time2_mean / time1_mean))
    } else {
      return(raw_diff)
    }
  }))

  #res2$diffratio <- as.numeric(lapply(res2$kmeans_cluster,function(x) log(mean(res2$time2[res2$kmeans_cluster == x])/mean(res2$time1[res2$kmeans_cluster == x]))))
  res2$diffchange <- as.numeric(lapply(res2$kmeans_cluster,function(x) ifelse(mean(res2$diffratio[res2$kmeans_cluster == x]) < 0,1-exp(mean(res2$diffratio[res2$kmeans_cluster == x])),(exp(mean(res2$diffratio[res2$kmeans_cluster == x]))) - 1)))

  res2$diffdir_kmeans <- as.character(lapply(res2$kmeans_cluster,function(x) ifelse(mean(res2$diffratio[res2$kmeans_cluster == x]) < 0,paste0("decreasing by ",round(100*mean(res2$diffchange[res2$kmeans_cluster == x]),2),"%"),paste0("increasing by ",round(mean(res2$diffchange[res2$kmeans_cluster == x])*100,2),"%"))))

  res2 <- res2 %>% mutate(diffdir_kmeans = factor(diffdir_kmeans, levels = names(sort(tapply(diffratio,diffdir_kmeans,mean)))))


  res2 <- res2 %>% dplyr::select(subject_accession,agroup,feature,kmeans_cluster,time1,time2,dist.eu,diffdir_kmeans)

  # reshape data to longer for later merging
  res_long <- res2 %>% pivot_longer(cols = c(time1,time2),names_to = "timeperiod",values_to = "log_assay_value")
  res_long$timeperiod <- ifelse(res_long$timeperiod=="time1",time1,time2)
  allres[['res_wide']] <- res2
  allres[['res_long']] <- res_long
  # plot(dif)
  #res2_view2 <<- res2
  return(list(allres, ncluster))

}

to_log2_vec <- function(x, is_log) {
  ## sanity checks ------------------------------------------------------------
  if (!is.numeric(x)) stop("`x` must be numeric")
  if (!is.logical(is_log)) stop("`is_log` must be logical")
  if (length(x) != length(is_log))
    stop("`x` and `is_log` must have the same length")

  ## element‑wise conversion --------------------------------------------------
  ifelse(is_log,
         # x is already log10 → convert to log2
         x / log10(2),
         # x is raw counts → log2(x+1)
         log2(x + 1))
}
#
# to_log2_plus1 <- function(x, is_log10) {
#   if (!is.numeric(x)) stop("`x` must be numeric")
#   if (!is.logical(is_log10)) stop("`is_log10` must be logical")
#   if (length(x) != length(is_log10))
#     stop("length mismatch")
#
#   ifelse(
#     is_log10,
#     log2(10^x + 1),
#     log2(x + 1)
#   )
# }

to_log2_plus1 <- function(x, is_log10) {
  if (!is.numeric(x)) stop("`x` must be numeric")
  if (!is.logical(is_log10)) stop("`is_log10` must be logical")
  if (length(x) != length(is_log10))
    stop("length mismatch")

  out <- numeric(length(x))

  idx <- is_log10 %in% TRUE

  out[idx]  <- log2(10^x[idx] + 1)
  out[!idx] <- log2(x[!idx] + 1)

  out
}


## Create data form
# Pass in sample data, t0 and t1, and desired outcome (norm mfi, mfi, au)
create_data_form_df <- function(data, t0, t1, log_assay_outcome) {


  # Features within the study
  data$antigen_feature <- paste(data$antigen, data$feature, sep = "_")
  include_feature <- unique(data$antigen_feature)
  #print(include_feature)
  arm_list <- unique(data$agroup) # arm_list
  # print(arm_list)
  timepoints <- unique(data$timeperiod) # Visits are time points (visit_name)

  #### specify time points
  ## needs to come from the UI select from the list of all time points
  # t0 <- timepoints[1]
  #t1 <- timepoints[2]

  tdiff<-paste0(t1,"/",t0)

  names_plotset <- c("best_sample_se_all_id","subject_accession", "timeperiod", "agroup", "antigen", "feature", "log_assay_value")
  # print(names_plotset)
  combined_data <- data[data$timeperiod %in% timepoints & data$antigen_feature %in% include_feature, ]
  #print(head(combined_data))
  #pcdata <- predata[predata$visit_name %in% timepoints & predata$feature %in% include_feature, ]
  #combined_data <- rbind(cdata, pcdata)
  #combined_data$visit_name <- droplevels(combined_data$visit_name)
  #print(combined_data$visit_name)

  levels(combined_data$timeperiod) <- unique(combined_data$timeperiod)
  #print(levels(combined_data$visit_name))
  combined_data$timeperiod <- factor(combined_data$timeperiod, ordered = FALSE)
  cat("Visit Name\n")
  print(unique(combined_data$timeperiod))
  combined_data$timeperiod <- relevel(combined_data$timeperiod, ref = t0) #time 0
  #print(levels(combined_data$visit_name))

  #combined_data$visit_name <- relevel(combined_data$visit_name, ref=t0) # time 0

  # update and filter combined data with log assay value  based on the outcome of interest
  combined_data <- combined_data[combined_data$agroup %in% arm_list, ]
  #combined_data <<- combined_data
  if (log_assay_outcome == "MFI") {
   # combined_data$log_assay_value <- log10(combined_data$assay_response + 1)
  combined_data$log_assay_value <- to_log2_plus1(combined_data$assay_response, combined_data$is_log_response)
  } else if (log_assay_outcome == "Normalized MFI") {
    #combined_data$log_assay_value <- log10(combined_data$norm_assay_response + 1)
    combined_data$log_assay_value <- to_log2_plus1(combined_data$norm_assay_response, combined_data$is_log_response)
  } else {
    keep <- is.finite(combined_data$final_predicted_concentration)
    combined_data <- combined_data[keep, ]
    combined_data$log_assay_value <-
      to_log2_plus1(
        combined_data$final_predicted_concentration,
        is_log10 = rep(FALSE, nrow(combined_data))
      )

    #model only detectable antibody concentration among respondents set to very low concentrations for stability
    #when final predicted concentration is used.
    combined_data$log_assay_value[combined_data$log_assay_value == 0] <- 0.001
  }

  # #### connection.R generates unstandardized, not scaled connection_data
  combined_data$study_accession <- factor(combined_data$study_accession)
  combined_data$experiment_accession <- factor(combined_data$experiment_accession)
  combined_data$subject_accession <- combined_data$patientid
  combined_data$subject_accession <- factor(combined_data$subject_accession)
  combined_data$feature <- factor(combined_data$feature)

  # antigen_family , , "value_imputed
  combined_data <- combined_data[,c("best_sample_se_all_id", "study_accession", "experiment_accession",
                                    "subject_accession", "agroup", "antigen",
                                    "feature", "antigen_feature", "timeperiod", "assay_response", "log_assay_value")]

  # print(head(combined_data))
  #### Unstandardized
  #### calculate difference and fold change between timepoints
  # requires these variables: c("study_accession","experiment_accession",
  # "subject_accession","arm_name","antigen","antigen_family",
  # "feature","visit_name","value_reported","log_assay_value","value_imputed",covariates)

  data_unstand <- combined_data[ , names_plotset]
  data_unstand$transformtype <- 'Unstandardized, not scaled'
  table(data_unstand$agroup)
  table(data_unstand$timeperiod)
  # print(data_unstand)
  # #### Unstandardized, scaled
  # #### calculate scaled log assay values, grouped by feature and visit
  data_unstandc <- group_by(data_unstand, feature, timeperiod) %>%
    mutate(log_assay_valuec = as.numeric(scale(log_assay_value)))
  data_unstandc$log_assay_value <- data_unstandc$log_assay_valuec
  data_unstandc$transformtype <- 'Unstandardized, scaled'
  data_unstandc <- subset(data_unstandc, select=-c(log_assay_valuec))
  table(data_unstandc$agroup)
  table(data_unstandc$timeperiod)
  #
  # print(data_unstandc)

  data_form <- rbind(data_unstand,data_unstandc)
  data_form$antigen_feature <- paste(data_form$antigen, data_form$feature, sep = "_")
  data_form <- data_form[data_form$timeperiod %in% c(timepoints, tdiff) & data_form$antigen_feature %in% include_feature, ]
  # table(data_form$visit_name,data_form$transformtype)
  #
  data_form <- data_form[with(data_form, order(antigen, feature, transformtype)), ]
  data_form$feature<-factor(data_form$feature)
  # table(data_form$arm_name)
  cat(names(data_form))
  cat("end data form")
  return(data_form)
}

# Set reference arm and transformation type. Read in the data form df as a result of create_data_form_df
set_reference_arm_transform_type <- function(data_form, arm_ref_group, selected_transformation, selected_antigen, selected_feature) {
  ## categorical variables
  data_form$agroup <- factor(data_form$agroup, ordered = FALSE)
  data_form$agroup <- relevel(data_form$agroup, ref=arm_ref_group)
  table(data_form$agroup)
  var_label(data_form$agroup)<-"study arm"

  data_form <- data_form[data_form$transformtype == selected_transformation & data_form$antigen == selected_antigen & data_form$feature == selected_feature, ]
  return(data_form)
}

# can_fit_mixture <- function(df, y) {
#   if (is.null(df)) return(FALSE)
#   if (nrow(df) < 3) return(FALSE)
#   yv <- df[[y]]
#   if (!any(is.finite(yv))) return(FALSE)
#   if (sd(yv, na.rm = TRUE) == 0) return(FALSE)
#   TRUE
# }

# Fit the finite mixture model and return it with the day0 set
compute_finite_mixture_model <- function(data_form_reference, t0, t1) {
  # select antigen from UI
  # data_form_reference <- data_form_reference[data_form_reference$antigen == selected_antigen,]
  # # get the current feature from UI.
  # selected_antigen_feature <- data_form_reference[data_form_reference$feature == selected_feature,]

  # print(head(data_form_reference))
  # print(str(data_form_reference))

  formt1t2 <- data_form_reference[data_form_reference$timeperiod %in% c(t0, t1), c("subject_accession","agroup","timeperiod","antigen","feature", "antigen_feature","log_assay_value")]
  # head(formt1t2)
  # data subsets for visits
  t0set <- formt1t2[formt1t2$timeperiod==t0,]
  t1set <- formt1t2[formt1t2$timeperiod==t1,]


  #titletxt <- paste(selected_feature, "not transformed", sep = "-") t0set$feature==selected_feature
  day0set <- t0set[!is.na(t0set$log_assay_value), ]
  day0set <- day0set[order(day0set$log_assay_value), ]
  #print(head(day0set))
  day0_values <- na.omit(as.vector(day0set$log_assay_value))
  # day0_values <- order(day0_values)

  #cat("before models")
  mo1 <- FLXMRglm(family = "gaussian")
  mo2 <- FLXMRglm(family = "gaussian")
  mo3 <- FLXMRglm(family = "gaussian")

  #cat("before flexmix")
  #day0set_v <<- day0set
  flexfit <- flexmix(log_assay_value ~ 1, data = day0set, k = 2, model = list(mo1, mo2))

  if (is.na(unlist(flexmix::parameters(flexfit)[1])[3])) {
    day0set <- cbind(day0set,data.frame(clusters=clusters(flexfit)))
  } else {
    if (unlist(flexmix::parameters(flexfit)[1])[3] > unlist(flexmix::parameters(flexfit)[1])[1]) {
      day0set <- cbind(day0set,data.frame(clusters=ifelse(clusters(flexfit) == 1, 2, 1)))
    } else {
      day0set <- cbind(day0set,data.frame(clusters=clusters(flexfit)))
    }
  }

  # The nth day after day 0.
  day_n_set <- merge(t1set[!is.na(t1set$log_assay_value), ],
                     day0set[ , c("subject_accession","agroup","antigen","feature","clusters")])
  # combine all days
  daysset <- rbind(day0set, day_n_set)


  parmdf <- clean_names(data.frame(t(as.data.frame(flexmix::parameters(flexfit)))[1:2,c("coef.(Intercept)","sigma")]))
  parmdf$min <- parmdf$coef_intercept - (2*parmdf$sigma)
  parmdf$max <- parmdf$coef_intercept + (2*parmdf$sigma)
  parmdf <- parmdf[order(parmdf$coef_intercept), ]
  drange <- parmdf[2,4] - parmdf[1,3]
  #cat(paste("Time 0:",t0))
  cat("\n\n")
  if (parmdf[2,3] - parmdf[1,4] > -(0.1 * drange)) {
    print("two distributions")
  } else {
    print("one distribution")
    daysset$clusters <- 1
  }


  return(daysset)
}

# Create density histogram of the finite mixture model data_form_reference, t0, t1
density_histogram <- function(day0set, n_clusters) {

  cluster_levels <-  unique(as.character(day0set$clusters))

  legend_labels <- if (length(cluster_levels) == 2) {
    setNames(c("Low", "Normal"), cluster_levels)
  } else {
    setNames(c("Normal"), cluster_levels)
  }


  histogram_cuta_plotly <- plot_ly()

  histogram_cuta_plotly <- histogram_cuta_plotly %>%
    add_trace(
      data = day0set,  # Use the entire dataset
      x = ~log_assay_value,
      type = "histogram",
      histnorm = "probability density",
      color = ~factor(clusters),  # Map color to the direction
      colors = t0_class_colors,  # Define colors for each direction
      name = ~ legend_labels[as.character(clusters)],#factor(clusters, labels = legend_labels), #paste0(clusters),
      opacity = 0.6,
      marker = list(line = list(color = "black", width = 1))
    )


  for (cluster in 1:n_clusters) {
    cluster_dat <- day0set[day0set$clusters == cluster, ]

    density_dat <- cluster_dat %>% summarise(x = list(density(log_assay_value)$x),
                                             y = list(density(log_assay_value)$y)) %>%
      unnest(cols = c(x, y))
    density_dat$clusters <- cluster

    histogram_cuta_plotly <- histogram_cuta_plotly %>% add_lines(
      data = density_dat,
      x = ~x,
      y = ~y,
      color = ~factor(clusters),
      colors = t0_class_colors,
      line = list(width = 1.5),
      name = ~paste("Density", legend_labels[as.character(clusters)]) #Cluster", clusters)
    )
  }

  histogram_cuta_plotly <- histogram_cuta_plotly %>%
    layout(
      title = list(text = paste(unique(day0set$antigen_feature), "at Visit", unique(day0set$timeperiod)), x = 0.5),
      xaxis = list(title = "log<sub>2</sub> Assay Value"),
      yaxis = list(title = "Density"),
      barmode = 'overlay',
      legend = list(title = list(text = "Clusters"))
    )

  return(histogram_cuta_plotly)
}


# Preform K-Means Clustering
obtain_difres_clustering <- function(data_form_reference_in, t0, t1, selected_feature, selected_antigen) {

  #ncluster <- 3

  #print(names(data_form_reference_in))
  #function outputs a list with two dataframes of the results - one in a wide and another in a long format. Wide format is used for scatter plot
  formt1t2 <- data_form_reference_in[data_form_reference_in$timeperiod %in% c(t0, t1), c("best_sample_se_all_id", "subject_accession","agroup",
                                                                                         "timeperiod","antigen","feature", "antigen_feature",
                                                                                         "log_assay_value"),]
  # print(head(formt1t2))
  # print(sum(is.na(formt1t2$log_assay_value)))
  #
  #tryCatch({
  difres <- subject_feature_clustering(input_data = formt1t2,
                                       time1 = t0,
                                       time2 = t1,
                                       dist_fx = signed_euclidean,
                                       feature = selected_feature,
                                       antigen = selected_antigen
                                       #ncluster = ncluster
  )
  # if (is.null(difres)) {
  #   return(NULL)
  # }else {
    return(difres)

  # if (length(difres) == 2) {
  #   return(difres)
  # } else {
  # either a list or a df.
  #return(difres)

  # }, error = function(e) {
  #    if (grepl("number of cluster centeres", e$message)){
  #      message("K-means clustering error:", e$message)
  #      return(NULL)
  #    }
  # })

}

# Visit difference scatter plot
visit_difference <- function(difres_input, t0, t1, selected_feature, selected_antigen) {

  ncluster <- as.numeric(difres_input[[2]])

  res <- difres_input[[1]]$res_wide
  res_long <- difres_input[[1]]$res_long
  ##scatter plot for kmeans clusters
  # if(length(unique(res$kmeans_cluster))==1){
  #   cat("Only 1 cluster")
  #   cat(" \n\n")
  # } else{
  unqcl <- sort(unique(res$kmeans_cluster))
  for(cl in unqcl){
    var_name <- paste0("clus_max_",cl)
    # var_name2 <- paste0("clus_min_",cl)
    var_name3 <- paste0("clus_mean_",cl)
    cl.line <- max(res$dist.eu[res$kmeans_cluster == cl])
    # cl.line2 <- min(res2$dist.eu[res2$cluster == cl]) - 0.1
    cl.mean <- round(mean(res$dist.eu[res$kmeans_cluster == cl]),4)
    assign(var_name,cl.line)
    # assign(var_name2,cl.line2)
    assign(var_name3,cl.mean)
  }

  #initilize figure
  dif_plotly <- plot_ly()

  # Nested loop, cluster, arm, plot points
  for (cluster in 1:ncluster) {
    for (arm in unique(res$agroup)) {
      # get current cluster mean
      clus_mean_var <- paste0("clus_mean_", cluster)
      clus_mean <- get(clus_mean_var)

      dif_plotly <- dif_plotly %>%
        add_trace(data = res[res$kmeans_cluster == cluster & res$agroup== arm,],
                  x = ~time1,
                  y = ~time2,
                  color = ~kmeans_cluster,
                  symbol = ~agroup,
                  name = paste("Cluster", cluster, ". Mean \u2248", clus_mean, "<br>Arm:", arm),
                  type = "scatter",
                  mode = "markers",
                  marker = list(color = clust.colors[cluster]),
                  text = ~paste0(
                    "Subject: ",subject_accession,
                    "<br>Arm: ", agroup, "<br>",
                    t0, ": ",round(time1,2), "<br>",
                    t1,": ", round(time2,2)
                  ),
                  hoverinfo = "text"
        )

    }
  }
  # Plot cluster boundaries
  for (cluster in 1:ncluster) {
    # Filter data for the current cluster
    data_cluster <- res[res$kmeans_cluster == cluster, ]


    # Generate x-values and y-values for the cluster boundary
    x_values_cluster <- seq(min(data_cluster$time1),max(data_cluster$time2), length.out = nrow(data_cluster))

    #  retrieve the max boundary for the current cluster
    clus_max_var <- paste0("clus_max_", cluster)
    # if (!exists(clus_max_var)) {
    #   stop(paste("Variable", clus_max_var, "does not exist"))
    # }
    clus_max <- get(clus_max_var)

    # Calculate y-values for the cluster boundary
    y_values_cluster <- 1 * x_values_cluster + clus_max

    dif_plotly <- dif_plotly %>%
      add_trace(
        x = x_values_cluster,
        y = y_values_cluster,
        type = "scatter",
        mode = "lines",
        line = list(dash = 'dash', color = clust.colors[cluster]),
        name = paste("Cluster", cluster, "Boundary")
      )
  }



  dif_plotly <- dif_plotly %>%
    add_trace(
      x = seq(min(res$time1),max(res$time2), length.out = nrow(res)),
      y = seq(min(res$time1),max(res$time2), length.out = nrow(res)),
      type = "scatter",
      mode = "lines",
      line = list(dash = 'solid', color = "black"),
      name = paste("Line of Equivalence")
    )

  # add title and axis labels
  dif_plotly <- dif_plotly %>%
    layout(
      title = paste(t1, "vs.", t0, "for", selected_antigen, "in", selected_feature),
      xaxis = list(title = paste(t0)),
      yaxis = list(title = paste(t1))
    )

  return(dif_plotly)


}

# difference histogram with euclidean distance as the x-axis
difference_histogram <- function(difres_input, selected_antigen, selected_feature) {
  res <- difres_input[[1]]$res_wide
  # histogram_cutb <- ggplot(res, aes(x = dist.eu)) +
  #    geom_histogram(aes(y = after_stat(density), fill = factor(diffdir_kmeans)), color= 1) +
  #    geom_density(lwd = 1.2, aes(color = factor(diffdir_kmeans))) +
  #    labs(title = paste(selected_antigen, selected_feature, sep = "_")) +
  #    theme_bw()

  unique_directions <- as.character(unique(res$diffdir_kmeans))
  # get specific color to match directions
  dif_class_colors <-clust.colors[1:length(levels(res$diffdir_kmeans))]
  names(dif_class_colors) <- levels(res$diffdir_kmeans)

  plotly_histogram <- plot_ly()

  for (direction_hist in length(unique_directions)) {
    plotly_histogram <- plotly_histogram %>%
      add_trace(
        data = res,  # Use the entire dataset
        x = ~dist.eu,
        type = "histogram",
        histnorm = "probability density",
        color = ~factor(diffdir_kmeans),  # Map color to the direction
        colors = unlist(dif_class_colors),  # Define colors for each direction
        name = ~paste0(diffdir_kmeans),#paste(direction_hist),
        opacity = 0.6,
        marker = list(line = list(color = "black", width = 1))
        # bingroup = 1
      )
  }

  for (direction in unique_directions) {
    # print(direction)
    # print(clust.colors[direction])
    cluster_data <- res[res$diffdir_kmeans == direction, ]
    if (nrow(cluster_data) >= 2) {
      density_data <- density(cluster_data$dist.eu)

      # get hex code for this density.
      this_dir_color_hex <- dif_class_colors[[direction]]

      plotly_histogram <- plotly_histogram %>%
        add_trace(
          data = data.frame(x = density_data$x, y = density_data$y),
          # x = ~dist.eu,
          x = density_data$x,
          y = density_data$y,
          type = 'scatter',
          mode = 'lines',
          line = list(width = 2, color = this_dir_color_hex),
          #colors = clust.colors,
          name = paste("Density Curve:", direction)
          #opacity = 0.6
          #group = cluster
        )
    } else {

      showNotification(
        paste("Skipping density for direction:", direction, "due to less than 2 data points at the visits specified"),
        type = "warning",
        duration = 5  # Show notification for 5 seconds
      )
    }
  }

  plotly_histogram <- plotly_histogram %>%
    layout(title = paste(selected_antigen, selected_feature, sep = "_"),
           xaxis = list(title = "Euclidean Distance"),
           yaxis = list(title = "Density"),
           legend = list(title=list(text='K-Means Direction')),
           barmode = 'overlay')  # Overlay the bars


  return(plotly_histogram)
}

## Create datsub
create_datsub <- function(difres_in, daysset_in, t0, t1) {
  res_long <- difres_in[[1]]$res_long
  datsub <- merge(res_long,daysset_in[, c("subject_accession", "agroup", "timeperiod", "feature", "clusters")],
                  by = c("subject_accession", "agroup", "timeperiod", "feature"), all.x = TRUE)

  ## adjust variable type
  datsub$subject_accession = factor(datsub$subject_accession)
  datsub$agroup = factor(datsub$agroup)
  datsub$feature = factor(datsub$feature)
  # Create cluster factor with visit labels
  if (length(unique(datsub$clusters)) == 1) {
    datsub$fclusters = factor(datsub$clusters, labels = c(paste("Normal at",t0)))
  } else {
    #print(length(unique(datsub$clusters))) # was low at t1.
    datsub$fclusters = factor(datsub$clusters, labels = c(paste("Normal at",t0), paste("Low at", t0)))
  }
  # make  into new variables
  # datsub$diffdir = factor(datsub$diffdir)
  datsub$diffdir_kmeans = factor(datsub$diffdir_kmeans)
  datsub$visit = factor(datsub$timeperiod)
  datsub$covar = datsub$agroup
  datsub$response = datsub$log_assay_value


  return(datsub)
}

# Create arm subplot for assay classification.
create_arm_subplot <- function(datsub_df_arm, dif_class_colors, t0_class_colors, max_response_value, log_assay_outcome, t0, t1) {
  # datsub_df_arm_in <<- datsub_df_arm
  # ensure visit order is correct based on t0 and t1 inputs
  datsub_df_arm$visit <- factor(datsub_df_arm$visit, levels = c(t0, t1))
  #datsub_df_arm_in <<- datsub_df_arm
  datsub_df_arm$diffdir_kmeans <- as.factor(datsub_df_arm$diffdir_kmeans)

  #cluster_levels <-  unique(as.character(day0set$clusters))
  # legend_labels <- if (length(cluster_levels) == 2) {
  #   setNames(c("Low", "Normal"), cluster_levels)
  # } else {
  #   setNames(c("Normal"), cluster_levels)
  # }
  #
  # color = ~factor(clusters),  # Map color to the direction
  # colors = t0_class_colors,  # Define colors for each direction
  # name = ~ legend_labels[as.character(clusters)],
  # Initialize the plot
  arm_subplot <- plot_ly(datsub_df_arm,
                         x = ~visit,
                         y = ~response,
                         color = ~fclusters,
                         colors = t0_class_colors,
                         name = ~paste(fclusters, agroup),
                         type = "scatter",
                         mode = "markers",
                         text = ~paste("Subject Accession: ", subject_accession,
                                       "<br>Status:", fclusters,
                                       "<br>log<sub>2</sub>(",log_assay_outcome,"+1):", round(response,2),
                                       "<br> K-Means direction:", diffdir_kmeans, # line color
                                       "<br>Arm:", agroup),
                         hoverinfo = "text")

  # Loop over directions and subjects to add lines for each subject
  unique_directions <- as.character(unique(datsub_df_arm$diffdir_kmeans))
  for (direction in unique_directions) {
    current_direction_df <- datsub_df_arm[datsub_df_arm$diffdir_kmeans == direction, ]
    this_class_color_hex <- dif_class_colors[[direction]]

    unique_subjects <- unique(current_direction_df$subject_accession)
    for (subject in unique_subjects) {
      current_subject_df <- current_direction_df[current_direction_df$subject_accession == subject,]
      arm_subplot <- arm_subplot %>%
        add_trace(
          data = current_subject_df,
          x = current_subject_df$visit,
          y = current_subject_df$response,
          type = 'scatter',
          mode = 'lines',
          line = list(color = this_class_color_hex),
          name = paste(direction),
          showlegend = F
        )
    }
  } # end directions for loop.


  # median_data <<- datsub_df_arm %>%
  #   group_by(visit) %>%
  #   summarize(median_response = median(response, na.rm = TRUE))

  # Calculate median response at each visit in the arm
  median_data <- datsub_df_arm %>%
    group_by(visit) %>%
    mutate(median_response = median(response, na.rm = TRUE))

  median_data <- median_data[!duplicated(median_data$visit), ]
  # median_data$subject_accession <- "Median"
  # median_data$fclusters <- ""

  # Assign the visits
  #visit1 <- levels(median_data$visit)[1]
  #visit2 <- levels(median_data$visit)[2]

  # visit1_median <-  median_data[median_data$visit == visit1,]$median_response
  # visit2_median <-  median_data[median_data$visit == visit2,]$median_response

  # arm_subplot <- arm_subplot %>%
  #   add_trace(data = median_data,
  #             x = ~visit,
  #             y = ~median_response,
  #             type = "scatter",
  #             mode = "lines",
  #             line = list(color = 'black', width = 2))
  # Add median for visit 1
  arm_subplot <- arm_subplot %>%
    add_trace(data = median_data[median_data$visit == t0, ],
              x = ~visit,
              y = ~median_response,
              type = "scatter",
              mode = "markers",
              marker = list(color = "black", size = 10, symbol = "cross"),
              text = ~paste("Median at Visit",visit,"<br>log<sub>2</sub>(", log_assay_outcome,"+1):", round(median_response,2)),
              name = ~paste("Median at Visit", visit, "Arm:", agroup),
              hoverinfo = "text")
  # add median for visit 2
  arm_subplot <- arm_subplot %>%
    add_trace(data = median_data[median_data$visit == t1,],
              x = ~visit,
              y = ~median_response,
              type = "scatter",
              mode = "markers",
              marker = list(color = "black", size = 10, symbol = "cross"),
              text = ~paste("Median at Visit",visit,"<br>log<sub>2</sub>(", log_assay_outcome, "+1):", round(median_response,2)),
              name = ~paste("Median at Visit", visit, "Arm:",agroup),
              hoverinfo = "text")



  arm_subplot <- arm_subplot %>%
    layout(
      yaxis = list(title = paste("log<sub>2</sub>(", log_assay_outcome, "+1)")),
      xaxis = list(tickangle = -45, title_standoff = 40),
      # Display Arm on top of plot
      annotations = list(
        x = 0.5, # Center the annotation
        y = max_response_value,
        text = unique(datsub_df_arm$agroup),
        showarrow = FALSE,
        xref = "x",
        yref = "y"
      )

    )

  return(arm_subplot)
}

#  Main function to plot assay classification.  Plot assay classification. Accounts for n arms.
plot_assay_classification <- function(datsub_df, selected_antigen, selected_feature, log_assay_outcome, visit1, visit2) {
  #dateub_assay <- datsub_df
  # Split the dataset into the two arms
  datsub_df_arms <- levels(datsub_df$agroup)
  n_arms <- length(datsub_df_arms)
  # plot both arms if available
  if (n_arms >= 2) {
    datsub_df_reference_arm <- datsub_df[datsub_df$agroup == datsub_df_arms[1],]
    # datsub_df_arm2 <- datsub_df[datsub_df$arm_name == datsub_df_arms[2],]
    #
    # # for plotting arm label later on add buffer space above the max_response
    # max_response <- max(datsub_df_reference_arm$response, datsub_df_arm2$response) + 0.5
    # Define colors for directions
    dif_class_colors <- clust.colors[1:length(levels(datsub_df_reference_arm$diffdir_kmeans))]
    names(dif_class_colors) <- levels(datsub_df_reference_arm$diffdir_kmeans)
    max_response <- max(datsub_df$response) + 0.5



    # Create the subplots for both arms using the helper function
    reference_subplot <- create_arm_subplot(datsub_df_arm = datsub_df_reference_arm, dif_class_colors = dif_class_colors, t0_class_colors = t0_class_colors,
                                            max_response_value = max_response, log_assay_outcome = log_assay_outcome, t0 = visit1, t1 = visit2)

    subplot_list <- list(reference_subplot)
    for (i in 2:n_arms) {
      datsub_df_arm_i <- datsub_df[datsub_df$agroup == datsub_df_arms[i],]
      arm_i_subplot <- create_arm_subplot(datsub_df_arm = datsub_df_arm_i, dif_class_colors = dif_class_colors, t0_class_colors = t0_class_colors,
                                          max_response_value = max_response, log_assay_outcome = log_assay_outcome, t0 = visit1, t1 = visit2)
      subplot_list <- append(subplot_list, list(arm_i_subplot))

    }

    # Combine both subplots into one figure
    fig <- subplot(subplot_list, shareY = T, shareX = F) %>%
      layout(
        annotations =
          list(
            list(
              x = 0.5,  # Position at the center of the plot
              y = -0.1,  # Adjust the vertical position (negative to place it below)
              xref = "paper",  # Reference to the whole figure
              yref = "paper",  # Reference to the whole figure
              text = "Visits",  # Common X-axis label
              showarrow = FALSE,
              font = list(size = 14),  # Font size
              align = "center"
            )
          ),

        # lapply(seq_along(names(dir_colors)), function(i) {
        #   direction <- names(dir_colors)[i]
        #   list(
        #     x = 0.5,
        #     y = -0.3,
        #     text = paste(direction),  # Annotation text
        #     showarrow = FALSE,
        #     font = list(size = 12, color = dir_colors[[direction]]),
        #     xref = "paper",
        #     yref = "paper"
        #   )
        # })


        # margin = list(
        #   l = 80,
        #   r = 80,
        #   t = 80,
        #   b = 90
        # ),
        title = paste("Assay Classification for", selected_antigen, "in", selected_feature)
      )

    return(fig)
  } else {
    datsub_df_reference_arm <- datsub_df[datsub_df$agroup == datsub_df_arms[1],]

    # for plotting arm label later on add buffer space above the max_response
    max_response <- max(datsub_df_reference_arm$response) + 0.5
    # Define colors for directions
    dif_class_colors <- clust.colors[1:length(levels(datsub_df_reference_arm$diffdir_kmeans))]
    names(dif_class_colors) <- levels(datsub_df_reference_arm$diffdir_kmeans)

    # Create the subplots for the one arm
    one_arm_plot <- create_arm_subplot(datsub_df_arm = datsub_df_reference_arm, dif_class_colors = dif_class_colors, t0_class_colors = t0_class_colors,
                                       max_response_value = max_response, log_assay_outcome = log_assay_outcome,
                                       t0 = visit1, t1 = visit2)

    fig <- one_arm_plot %>%
      layout(
        xaxis = list(title = "Visits"),
        title = paste("Assay Classification for", selected_antigen, "in", selected_feature)
      )
    return(fig)
  } # end else


}

## Download Data
# Figure 1
download_first_visit_class <- function(download_df, selected_transformation, selected_antigen, selected_feature) {
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_antigen, "_",selected_feature, "-", selected_transformation, "_first_visit_class_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download ", selected_transformation," First Visit Class Data for ",selected_antigen, " in ", selected_feature),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

# Figure 2 Visit Difference
download_visit_difference <- function(download_df, selected_transformation, selected_antigen, selected_feature, t0, t1) {
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_antigen, "_",selected_feature, "-", selected_transformation, "visit_difference", t0, "_", t1),
    output_extension = ".xlsx",
    button_label = paste0("Download ", selected_transformation," Visit difference (", t0, " - ", t1, ") Data for ",selected_antigen, " in ", selected_feature),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}

# Figure 3 difference histogram
download_difference_histogram_data <-  function(download_df, selected_transformation, selected_antigen, selected_feature) {
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_antigen, "_",selected_feature, "-", selected_transformation, "_visit_difference_kmeans_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download ", selected_transformation," K-Means Direction Data for ",selected_antigen, " in ", selected_feature),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
}
# Figure 4 assay classification
download_assay_classification_data <- function(download_df, selected_transformation, selected_antigen, selected_feature) {
  download_plot_data <- download_this(
    download_df,
    output_name = paste0(selected_antigen, "_",selected_feature, "-", selected_transformation, "_visit_plot_data"),
    output_extension = ".xlsx",
    button_label = paste0("Download ", selected_transformation," Visit Plot Data for ",selected_antigen, " in ", selected_feature),
    button_type = "warning",
    icon = "fa fa-save",
    class = "hvr-sweep-to-left"
  )
  return(download_plot_data)
}


