# Define Color Palettes
antigen.colors = c("cg_1_20" = "#f6a600", "fha_27" = "#a1caf1", "gB" = "#008856", "PRN" = "#dcd300", "PT" = "#0067a5", "TT" = "#e25822")
cluster.colors = c("ADCD" = "#604e97", "ADNP/ADCP" = "#654522", "FcgR2a/FcgR3b" = "#c2b280", "IgG2/IgG4" = "#882d17", "IgG1" = "#2b3d26", "IgG3" = "#f3c300", "Total IgG" = "#e68fac")
subgroup.colors = c("1" = "#848482", "2" = "#f99379", "3" = "#875692", "4" = "#8db600", "5" = "#be0032", "6" = "lightgrey")

# The datsub set is the classify set
# reformat the classification set
prepare_classify_set <- function(classify_set, baseline_visit, followup_visit) {
  classify_set$feature <- paste(classify_set$antigen, classify_set$feature, sep = "_")
  classify_set <- classify_set[, !(colnames(classify_set) %in% c("dist.eu", "kmeans_cluster", "covar", "clusters", "response"))]

  classify_set$timeperiod <- ifelse(classify_set$timeperiod == baseline_visit, "t0", ifelse(classify_set$timeperiod == followup_visit, "t1", NA))

  return(classify_set)
}

# create subgroups of low medium and high response and add to classify set.
create_categorized_subgroups <- function(classify_set) {
  categorized_subgroups <- unique(classify_set[ c("feature", "diffdir_kmeans")])
  categorized_subgroups$difference <- as.numeric(str_extract(categorized_subgroups$diffdir_kmeans, "\\d+\\.?\\d*"))
  # adjust sign based on decreasing
  categorized_subgroups$difference <- ifelse(str_detect(categorized_subgroups$diffdir_kmeans, "decreasing"),
                                             -categorized_subgroups$difference,
                                             categorized_subgroups$difference)

  categorized_subgroups$rank <- ave(categorized_subgroups$difference, categorized_subgroups$feature, FUN = rank)
  categorized_subgroups$response_level <- factor(ifelse(categorized_subgroups$rank == 1, "Low",
                                                        ifelse(categorized_subgroups$rank == 2, "Moderate", "High")),
                                                 levels = c("Low", "Moderate", "High"))

  # dummy variables (binary indicators)
  categorized_subgroups$low <- as.integer(categorized_subgroups$response_level == "Low")
  categorized_subgroups$moderate = as.integer(categorized_subgroups$response_level == "Moderate")
  categorized_subgroups$high = as.integer(categorized_subgroups$response_level == "High")


  categorized_subgroups <- categorized_subgroups[, c("feature", "diffdir_kmeans", "response_level", "low", "moderate", "high")]
  classify_set <- classify_set %>% left_join(categorized_subgroups, by = c("feature", "diffdir_kmeans"))

  return(classify_set)
}

# create the categorical heatmap
#display heatmap colors based on categorical response (low / moderate / high)
create_catigorical_heatmap_matrix <- function(classify_set, baseline_visit) {
  #display heatmap colors based on categorical response (low / moderate / high)
  categorical_heatmap_matrix <- classify_set[,c("subject_accession", "visit","feature", "response_level")]
  categorical_heatmap_matrix <- categorical_heatmap_matrix[categorical_heatmap_matrix$visit == baseline_visit, ]
  categorical_heatmap_matrix <- categorical_heatmap_matrix[, c("subject_accession", "feature", "response_level")]


  categorical_heatmap_matrix$response_level = as.numeric(categorical_heatmap_matrix$response_level)

  categorical_heatmap_matrix <-  categorical_heatmap_matrix %>% pivot_wider(names_from = feature, values_from = response_level,
                                                                            id_cols =  c("subject_accession")) %>%
    column_to_rownames("subject_accession")

  return(categorical_heatmap_matrix)
}

# create classified set in a new shape to take the difference
create_classifed_set_visit <- function(classify_set) {
  #classify_set_view <<- classify_set
  classify_set_visit <- classify_set %>% pivot_wider(names_from = timeperiod, values_from = log_assay_value, id_cols =  c("subject_accession", "feature"))


  classify_set_visit <- merge(classify_set_visit, classify_set[,!names(classify_set) %in% c("visit_name", "visit", "timeperiod", "covar", "response", "log_assay_value")], by = c("subject_accession", "feature"), all.x = TRUE)

  classify_set_visit <- unique(classify_set_visit)

  return(classify_set_visit)
}

# Create continuous heatmap matrix
create_continuous_heatmap_matrix <- function(classify_set_visit) {
 #classify_set_visit_view <<- classify_set_visit
  classify_set_visit$t1 <- as.numeric(lapply(classify_set_visit$t1, function(x) as.numeric(x[[1]])))
  classify_set_visit$t0 <- as.numeric(lapply(classify_set_visit$t0, function(x) as.numeric(x[[1]])))
 # classify_set_visit_view <<- classify_set_visit


  classify_set_visit$difference <- classify_set_visit$t1 - classify_set_visit$t0
  classify_set_visit <- classify_set_visit[,c("subject_accession", "feature", "difference")]

  continuous_heatmap_matrix <- reshape(classify_set_visit,
                                       idvar = "subject_accession",
                                       timevar = "feature",
                                       direction = "wide")

  colnames(continuous_heatmap_matrix) <- sub("^difference\\.", "", colnames(continuous_heatmap_matrix))
  rownames(continuous_heatmap_matrix) <- NULL

  continuous_heatmap_matrix <-  continuous_heatmap_matrix %>% column_to_rownames("subject_accession")
  return(continuous_heatmap_matrix)
}
# create baseline symbols for pheatmap
create_baseline_symbols <- function(classify_set, baseline_visit, continuous_heatmap_matrix) {
  missing_data_subjects <- rowSums(is.na(continuous_heatmap_matrix)) != 0

  baseline_symbols <- unique(classify_set[, c("subject_accession", "feature", "fclusters")])


  baseline_symbols <- baseline_symbols %>% pivot_wider(names_from = feature, values_from = fclusters) %>%
    column_to_rownames("subject_accession")

  # convert columns to characters to replace text as symbols.
  baseline_symbols[] <- lapply(baseline_symbols, as.character)

  baseline_symbols[baseline_symbols == paste("Low at", baseline_visit)] <-"\u25CF"#"●"
  baseline_symbols[baseline_symbols != "\u25CF"] <- ""
  # filter out any missing subjects
  baseline_symbols <- baseline_symbols[!missing_data_subjects,]
  return(baseline_symbols)
}

# conduct heirachial clustering taking in both the continuous and categorical heat map matrices
preform_heirachial_clustering <- function(continuous_heatmap_matrix, categorical_heatmap_matrix) {
  # continuous_heatmap_matrix_1 <<- continuous_heatmap_matrix
  # categorical_heatmap_matrix_1 <<- categorical_heatmap_matrix
  continuous_heatmap_matrix <- as.matrix(continuous_heatmap_matrix)

  valid_rows <- function(x) {
    # x must be a numeric matrix
    rowSums(!is.finite(x)) == 0
  }

  keep <- valid_rows(continuous_heatmap_matrix)

  continuous_heatmap_matrix <- continuous_heatmap_matrix[keep, ,drop = FALSE]
  categorical_heatmap_matrix <- categorical_heatmap_matrix[keep, ,drop = FALSE]
  #missing_data_subjects <- rowSums(is.na(continuous_heatmap_matrix)) != 0
 # missing_data_subjects <- rowSums(!is.finite(as.matrix(continuous_heatmap_matrix))) > 0
  # categorical_heatmap_matrix <- categorical_heatmap_matrix[!missing_data_subjects,]
  # continuous_heatmap_matrix <-  continuous_heatmap_matrix[!missing_data_subjects,]
  # baseline_symbols <- baseline_symbols[!missing_data_subjects,]

  row_clusters <- hclust(dist(continuous_heatmap_matrix))

  col_clusters <- hclust(dist(t(continuous_heatmap_matrix)))


  return(list(row_clusters, col_clusters, keep))
}

# Cluster by antigen for pheatmap
grouped_antigens <- function(classify_set) {
  col_annotation_antigen <- as.data.frame(unique(classify_set[,c("feature", "antigen")]))
  names(col_annotation_antigen)[names(col_annotation_antigen) == 'antigen'] <- 'Antigen'

  # rownames(col_annotation_antigen) <- col_annotation_antigen %>% pull(feature) %>% as.character()
  rownames(col_annotation_antigen) <- as.character(col_annotation_antigen$feature)

  col_annotation_antigen <- col_annotation_antigen[, c("Antigen"), drop = F]
  annotation_colors_antigen <- list(
    Antigen = antigen.colors,
    Subgroup = subgroup.colors
  )

  return(list(col_annotation_antigen, annotation_colors_antigen))
}

# Plot heatmap as a result of hclust clustering.
plot_heatmap_hclust <- function(heatmap_matrix_in, num_subgroups_option_selected_in, symbols_in, row_annotation_in, row_clust_in, col_clust_in) {
  # Convert heatmap_matrix to a numeric matrix
  heatmap_matrix_numeric <- as.matrix(heatmap_matrix_in)
  subgroup_color_subset <- subgroup.colors[1:num_subgroups_option_selected_in]
  # Get unique values present in the heatmap matrix
  unique_values <- sort(unique(as.vector(heatmap_matrix_numeric)))

  # Define full legend mappings
  breaks_full <- c(1, 2, 3)
  labels_full <- c("Low", "Moderate", "High")
  colors_full <- c("#4575B4", "#FEFEC0", "#D73027")

  # Filter based on present values
  legend_breaks <- breaks_full[breaks_full %in% unique_values]
  legend_labels <- labels_full[breaks_full %in% unique_values]
  color_palette <- colors_full[breaks_full %in% unique_values]  # Adjust colors

  # Generate heatmap with dynamic colors
  subject_feature_heatmap <- pheatmap(
    heatmap_matrix_numeric,
    display_numbers = symbols_in,
    annotation_row = row_annotation_in,
    annotation_colors = list(Subgroup = subgroup_color_subset),
    cluster_rows = row_clust_in,
    cluster_cols = col_clust_in,
    silent = TRUE,
    cutree_rows = num_subgroups_option_selected_in,
    color = color_palette,  # Dynamically adjusted colors
    legend_breaks = legend_breaks,
    legend_labels = legend_labels
  )
  return(subject_feature_heatmap)
}

# Heatmaply version.
plot_heatmap_hclust_heatmaply <- function(heatmap_matrix_in, num_subgroups_option_selected_in, symbols_in, row_annotation_in, row_clust_in, col_clust_in, keep) {

  heatmap_matrix_in <- heatmap_matrix_in[keep, , drop = FALSE]
  symbols_in        <- symbols_in[keep, , drop = FALSE]
  row_annotation_in <- row_annotation_in[keep, , drop = FALSE]

  # They are all the same values in each cell so extract one.
  # get_max_value <- function(x) {
  #   max(x[[1]])
  # }
  get_max_value <- function(x) {
    v <- x[[1]]
    v <- v[is.finite(v)]
    if (length(v) == 0) return(NA_real_)
    max(v)
  }

  # Apply the function to the entire matrix
  heatmap_matrix_numeric <- apply(heatmap_matrix_in, c(1, 2), get_max_value)
  # heatmap_matrix_numeric[!is.finite(heatmap_matrix_numeric)] <- NA

  row_clust_in <- row_clust_in
  col_clust_in <- col_clust_in

  # Perform hierarchical clustering for rows and columns
  row_dend <- as.dendrogram(row_clust_in)
  col_dend <- as.dendrogram(col_clust_in)

  # color the dendrogram by colors
  row_dend <- color_branches(row_dend, k = num_subgroups_option_selected_in, col = subgroup.colors)

  # Cut dendrograms into specified number of clusters
  row_clusters <- cutree(row_dend, k = num_subgroups_option_selected_in)
  col_clusters <- cutree(col_dend, k = num_subgroups_option_selected_in)


  # Create a data frame for row and column annotations
  row_annotation <- data.frame(Subgroup = factor(row_clusters, labels = paste("Cluster", 1:num_subgroups_option_selected_in)))
  col_annotation <- data.frame(Subgroup = factor(col_clusters, labels = paste("Cluster", 1:num_subgroups_option_selected_in)))

  subgroup_color_subset <- subgroup.colors[row_clusters]
  # Get unique values present in the heatmap matrix
  unique_values <- sort(unique(as.vector(heatmap_matrix_numeric)))
  # unique_values <- tryCatch(
  #   sort(unique(as.vector(heatmap_matrix_numeric))),
  #   error = function(e) sort(unique(unlist(heatmap_matrix_numeric)))
  # )

  #heatmap_matrix_numeric <<- do.call(rbind, heatmap_matrix_numeric)


  breaks_full <- c(1, 2, 3)
  labels_full <- c("Low", "Moderate", "High")
  colors_full <- c("#4575B4", "#FEFEC0", "#D73027")

  # Filter based on present values
  legend_breaks <- breaks_full[breaks_full %in% unique_values]
  legend_labels <- labels_full[breaks_full %in% unique_values]
  color_palette <- colors_full[breaks_full %in% unique_values]

 heatmap_matrix_numeric[] <- as.numeric(
  factor(
    heatmap_matrix_numeric,
    levels = breaks_full
  )
)

  # Create the interactive heatmap with heatmaply
  heatmaply(
    heatmap_matrix_numeric,
    Rowv = row_dend,
    Colv = col_dend,
    colors = color_palette,  # Color scale for values
    dendrogram = "both",  # Both row and column dendrogram
    showticklabels = c(TRUE, TRUE),  # Display axis labels
    # row_side_colors = subgroup_color_subset,  # Categorical color for rows
    #column_side_colors = col_colors,  # Categorical color for columns
    annotation_row = row_annotation,
    annotation_col = col_annotation,
    cellnote = as.matrix(symbols_in),  # Show values as symbols
    draw_cellnote = T,
    cellnote_size = 6,
    # grid_gap = 0.6,
    #grid_color = "black",
    branches_lwd = 0.4,

    #custom_hovertext = heatmap_matrix_numeric,

    # colorbar = list(
    #   tickvals = legend_breaks,  # Define the ticks based on the categories
    #   ticktext = legend_labels  # Labels for Low, Moderate, High
    # ),
    colorbar = list(
      tickmode = "array",
      tickvals = seq_along(legend_breaks),
      ticktext = legend_labels,
      title = "Response"
    ),
    color = color_palette,
    xlab = "Antigen",
    ylab = "Subject",
    key.title = "Response"
  )
}

