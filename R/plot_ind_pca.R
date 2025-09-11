#' Plots distance metrics relative to reference run in principal component space
#'
#'@param data.dir directory where the data files are stored
#'@param ref.ind.file file containing the eco indicators data
#'@param ref.state.file file containing the reference eco state data
#'@param figure.dir directory where the figures will be saved
#'
#'
#'@return dataframe of eco indicators by year
#'
#'@export

plot_ind_pca = function(data.dir, ref.ind.file, ref.state.file,figure.dir){
  
  #Read in data
  observations_df = readRDS(ref.ind.file) |> 
    dplyr::arrange(run) |> 
    dplyr::select(run,year,Variable,state.value) |> 
    #convert from long to wide
    tidyr::pivot_wider(names_from = Variable, values_from = state.value) |> 
    dplyr::select(-reactive_ascendancy) |> 
    na.omit()
    #filter to only relevant variables
  
  #variables to keep
  var.keep = c('year','run','bio.tot','catch.tot','prop.of','prop.bio.pelagic','mean.tl.bio',
               'steep','bio_inf','tl_inf',
               'small.large.ratio.anom.mean',
               'JS_divergence',
               'connectance','mean_in_degree','network_betweenness_centralization',
               'avg_jaccard_similarity','ascendancy','capacity','resilience_eigenvalue')
  observations_df = observations_df |> dplyr::select(dplyr::all_of(var.keep))
  metric_names_ordered <- colnames(observations_df)
  metrics_for_projection <- metric_names_ordered[!(metric_names_ordered %in% c("year","run"))]

  #Get reference state information  
  ref.state = readRDS(ref.state.file)
  
  # ref.state.year = readRDS(ref.state.year.file) |> 
  #   dplyr::select(-year) |> 
  #   dplyr::select(dplyr::all_of(metric_names_ordered))
  # 
  ref.state <- ref.state[match(metric_names_ordered, ref.state$Variable), ] |> 
    dplyr::filter(!is.na(Variable))
  
  
  #Define desired state from reference state
  desired.lower.base = ref.state$desired.min
  desired.upper.base = ref.state$desired.max
  
  target_ranges_df = matrix(c(desired.lower.base, desired.upper.base), ncol = 2, byrow = F) |> t() |> as.data.frame()
  rownames(target_ranges_df) <- c("lower", "upper")
  colnames(target_ranges_df) = metrics_for_projection # Exclude 'year' from metric names
  
  #Run PCA
  years = observations_df$year
  runs = observations_df$run
  pca_data = dplyr::select(observations_df,-year, -run)
  
  pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
  observations_pca <- data.frame(
    year = years,
    run = runs,
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2]
  )
  
  saveRDS(pca_result, file = paste0(data.dir, 'pca_result.rds'))
  
  #Rescale reference state to PCA space
 
  # Create a list where each element is a vector of [lower, upper] for a metric
  # Ensure grid_args creation matches the column order of observations_df
  grid_args <- lapply(metrics_for_projection, function(metric_name) {
    c(target_ranges_df["lower", metric_name], target_ranges_df["upper", metric_name])
  })
  # Names are also set using the filtered list
  names(grid_args) <- metrics_for_projection
  
  # The rest of your code for creating and projecting the corners will now work correctly
  corner_df <- do.call(expand.grid, grid_args)
  corner_matrix <- as.matrix(corner_df)
  
  scaled_corners <- scale(corner_matrix,
                          center = pca_result$center,
                          scale = pca_result$scale)
  
  projected_corners <- as.data.frame(scaled_corners %*% pca_result$rotation[, 1:2])
  colnames(projected_corners) <- c("PC1", "PC2")
  
  
  # Calculate the convex hull of the projected corners to define the target space boundary
  hull_indices <- geometry::convhulln(projected_corners[, c("PC1", "PC2")])
  hull_points <- projected_corners[hull_indices, ]
  
  # Order hull points for plotting
  hull_centroid_x <- mean(hull_points$PC1)
  hull_centroid_y <- mean(hull_points$PC2)
  angles <- atan2(hull_points$PC2 - hull_centroid_y, hull_points$PC1 - hull_centroid_x)
  target_hull <- hull_points[order(angles), ]
  target_hull <- rbind(target_hull, target_hull[1, ]) # Close the polygon
  
  
  # Plot PCA
  
  variance_explained <- round((pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100, 2)
  
  p <- ggplot2::ggplot() +
    # Plot the target space polygon
    ggplot2::geom_polygon(data = target_hull,
                          ggplot2::aes(x = PC1, y = PC2),
                          fill = "lightgreen", alpha = 0.5, color = "darkgreen",
                          linetype = "dashed", linewidth = 0.8) +
    
    # CRITICAL: Plot the observations and map color to 'year'
    # ggplot2::geom_point(data = observations_pca,
    #                     ggplot2::aes(x = PC1, y = PC2, color = year),
    #                     alpha = 0.8, size = 3) +
    
    # Optionally, add a path to show the trajectory over time
    ggplot2::geom_path(data = observations_pca,
                       ggplot2::aes(x = PC1, y = PC2, color = factor(run)), # group=1 connects all points
                       alpha = 0.5) +
    
    # Use a sequential color scale appropriate for time
    ggplot2::scale_color_viridis_d() +
    ggplot2::guides()
    ggplot2::labs(
      title = "PCA Trajectory of Observations Over Time",
      subtitle = "Green polygon represents the projected target space",
      x = paste0("Principal Component 1 (", variance_explained[1], "%)"),
      y = paste0("Principal Component 2 (", variance_explained[2], "%)"),
      color = "Year" # Update the legend title
    ) +
    ggplot2::coord_fixed(ratio = 1) + # Essential for correct PCA interpretation
    ggplot2::theme_bw()
  p
}

# data.dir = 'D:/catch_thresholds_eof_3/output/'
# ref.ind.file = paste0(data.dir,'catch_thresholds_eof_3_run_eco_ind.rds')
# ref.state.file = here::here('data-raw','ref_eco_state.rds')
# ref.state.year.file = here::here('data-raw','ref_eco_state_year.rds')