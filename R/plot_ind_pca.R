# Plots distance metrics relative to reference run in principal component space
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
    dplyr::select(run,Variable,state.value) |> 
    #convert from long to wide
    tidyr::pivot_wider(names_from = Variable, values_from = state.value) |> 
    dplyr::select(-run) |> 
    na.omit()
  
  metric_names_ordered <- colnames(observations_df)

  #Get reference state information  
  ref.state = readRDS(ref.state.file)
  
  # ref.state.year = readRDS(ref.state.year.file) |> 
  #   dplyr::select(-year) |> 
  #   dplyr::select(dplyr::all_of(metric_names_ordered))
  # 
    ref.state <- ref.state[match(metric_names_ordered, ref.state$Variable), ]
  
  #Define desired state from reference state
  desired.lower.base = ref.state$desired.min
  desired.upper.base = ref.state$desired.max
  
  target_ranges_df = matrix(c(desired.lower.base, desired.upper.base), ncol = 2, byrow = F) |> t() |> as.data.frame()
  rownames(target_ranges_df) <- c("lower", "upper")
  colnames(target_ranges_df) = metric_names_ordered
  
  #Run PCA
  pca_result <- prcomp(observations_df, scale. = TRUE, center = TRUE)
  observations_pca <- as.data.frame(pca_result$x[, 1:2])
  colnames(observations_pca) <- c("PC1", "PC2")
  
  saveRDS(pca_result, file = paste0(data.dir, 'pca_result.rds'))
  
  #Rescale reference state to PCA space
  # ref.state.year.pca <- as.data.frame(scale(ref.state.year,
  #                                     center = pca_result$center,
  #                                     scale = pca_result$scale))
  # ref.state.year.scaled <- as.matrix(ref.state.year.pca) %*% pca_result$rotation
  # ref.state.year.df <- data.frame(PC1 = ref.state.year.scaled[, 1], PC2 = ref.state.year.scaled[, 2])
  # 
  # #Define convex hull for desired state (envelope of reference state in PCA space)
  # ref.state.year.hull = geometry::convhulln(ref.state.year.scaled[, c("PC1", "PC2")], options = "Fx") 
  # ref.state.year.hull = ref.state.year.scaled[ref.state.year.hull, ]|> as.data.frame()
  # ref.state.year.hull <- rbind(ref.state.year.hull, ref.state.year.hull[1, ])  # Close the polygon
  # 
  # # Order the convex hull points for plotting
  # base_centroid_x <- mean(ref.state.year.hull$PC1)
  # base_centroid_y <- mean(ref.state.year.hull$PC2)
  # angle.base <- atan2(ref.state.year.hull$PC2 - base_centroid_y, ref.state.year.hull$PC1 - base_centroid_x)
  # ordered.indices.base <- order(angle.base)
  # ref.hull <- ref.state.year.hull[ordered.indices.base, ]
  # ref.hull <- rbind(ref.hull, ref.hull[1, ]) # Close the polygon
  
  #Project the desired state to PCA space
  
  # Create a list where each element is a vector of [lower, upper] for a metric
  # Ensure grid_args creation matches the column order of observations_df
  grid_args <- lapply(metric_names_ordered, function(metric_name) {
    c(target_ranges_df["lower", metric_name], target_ranges_df["upper", metric_name])
  })
  names(grid_args) <- metric_names_ordered
  
  corner_df <- do.call(expand.grid, grid_args)
  corner_matrix <- as.matrix(corner_df)
  
  # IMPORTANT: Apply the SAME scaling and centering as the original PCA
  scaled_corners <- scale(corner_matrix,
                          center = pca_result$center,
                          scale = pca_result$scale)
  
  
  # Project these corners onto the first two principal components
  projected_corners <- as.data.frame(scaled_corners %*% pca_result$rotation[, 1:2])
  colnames(projected_corners) <- c("PC1", "PC2")
  
  
  # Calculate the convex hull of the projected corners to define the target space boundary
  if (nrow(projected_corners) < 3 || length(unique(projected_corners$PC1)) < 2 || length(unique(projected_corners$PC2)) < 2) {
    warning("Not enough unique projected corners or points forming a line/point for a meaningful convex hull. Plot might be empty or a line.")
    target_hull <- data.frame(PC1 = numeric(), PC2 = numeric()) # Empty data frame
  } else {
    hull_indices <- geometry::convhulln(projected_corners[, c("PC1", "PC2")], options = "Fx")
    hull_points <- projected_corners[hull_indices, ]
    
    # Order the convex hull points for plotting
    hull_centroid_x <- mean(hull_points$PC1)
    hull_centroid_y <- mean(hull_points$PC2)
    angles <- atan2(hull_points$PC2 - hull_centroid_y, hull_points$PC1 - hull_centroid_x)
    ordered_indices <- order(angles)
    target_hull <- hull_points[ordered_indices, ]
    target_hull <- rbind(target_hull, target_hull[1, ]) # Close the polygon
  }
  
  
  # Plot PCA
  
  variance_explained <- round((pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100, 2)
  
  p <- ggplot2::ggplot()+ 
    ggplot2::geom_polygon(data = target_hull,
                          ggplot2::aes(x = PC1, y = PC2),
                              fill = "lightgreen", alpha = 0.5, color = "darkgreen",
                              linetype = "dashed", linewidth = 0.8)+
    # geom_polygon(data = ref.hull,aes(x=PC1,y =PC2),fill = 'lightblue',alpha = 0.5,
    #              color = 'darkblue',linetype = 'dashed',linewidth = 0.8)
  
    ggplot2::geom_point(data = cbind(observations_pca, run = 1:nrow(observations_pca)),
                        ggplot2::aes(x = PC1, y = PC2, color = run),
              alpha = 0.7, size = 2) +
    ggplot2::scale_color_viridis_c()+
    ggplot2::labs(
      title = "Observations and Projected Target Space (PCA)",
      x = paste0("Principal Component 1 (", variance_explained[1], "%)"),
      y = paste0("Principal Component 2 (", variance_explained[2], "%)")
    )+
    ggplot2::coord_fixed(ratio = 1)+
    theme_bw()
  
  p
  ggplot2::ggsave(filename = paste0(figure.dir,'/pca_projection_plot.png'), width = 10, height = 8)
  
  print(variance_explained)
  
  
}

# data.dir = 'D:/catch_thresholds_eof_3/output/'
# ref.ind.file = paste0(data.dir,'catch_thresholds_eof_3_run_eco_ind.rds')
# ref.state.file = here::here('data-raw','ref_eco_state.rds')
# ref.state.year.file = here::here('data-raw','ref_eco_state_year.rds')