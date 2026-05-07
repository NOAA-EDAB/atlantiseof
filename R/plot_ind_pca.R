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
plot_ind_pca = function(data.dir, ref.ind.file, ref.state.file, figure.dir){
  
  # Read in data
  observations_df = base::readRDS(ref.ind.file) |> 
    dplyr::filter(Variable != 'reactive_ascendancy')
  
  var.keep = base::c('year', 'run.id', base::sort(base::unique(observations_df$Variable)))
  
  observations_df = observations_df |> 
    dplyr::arrange(run.id) |> 
    dplyr::select(run.id, year, Variable, state.value) |> 
    # convert from long to wide
    tidyr::pivot_wider(names_from = Variable, values_from = state.value) |> 
    stats::na.omit()
  
  observations_df = observations_df |> dplyr::select(dplyr::all_of(var.keep))
  
  # --- NEW: Identify and remove columns with zero variance ---
  # Isolate just the data columns (ignore year and run.id)
  data_cols = base::setdiff(base::colnames(observations_df), base::c("year", "run.id"))
  
  # Calculate variance for each column
  col_vars = base::apply(observations_df[, data_cols, drop = FALSE], 2, stats::var)
  
  # Keep only columns where variance is greater than 0
  valid_cols = base::names(col_vars[col_vars > 0])
  
  # Re-subset the dataframe to drop the flatlined indicators
  observations_df = observations_df |> 
    dplyr::select(year, run.id, dplyr::all_of(valid_cols))
  # -----------------------------------------------------------
  
  metric_names_ordered <- base::colnames(observations_df)
  metrics_for_projection <- metric_names_ordered[!(metric_names_ordered %in% base::c("year","run.id"))]
  
  # Get reference state information  
  ref.state = base::readRDS(ref.state.file)
  
  ref.state <- ref.state[base::match(metric_names_ordered, ref.state$Variable), ] |> 
    dplyr::filter(!base::is.na(Variable))
  
  
  # Define desired state from reference state
  desired.lower.base = ref.state$desired.min
  desired.upper.base = ref.state$desired.max
  
  target_ranges_df = base::matrix(base::c(desired.lower.base, desired.upper.base), ncol = 2, byrow = FALSE) |> 
    base::t() |> 
    base::as.data.frame()
  
  base::rownames(target_ranges_df) <- base::c("lower", "upper")
  base::colnames(target_ranges_df) = metrics_for_projection # Exclude 'year' from metric names
  
  # Run PCA
  years = observations_df$year
  runs = observations_df$run.id
  pca_data = dplyr::select(observations_df, -year, -run.id)
  
  pca_result <- stats::prcomp(pca_data, scale. = TRUE, center = TRUE)
  observations_pca <- base::data.frame(
    year = years,
    run.id = runs,
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2]
  )
  
  base::saveRDS(pca_result, file = base::paste0(data.dir, 'pca_result.rds'))
  
  # ---------------------------------------------------------
  # EFFICIENT TARGET SPACE PROJECTION (Angular Sweep Method)
  # ---------------------------------------------------------
  
  # 1. Extract and pre-scale the bounds using the PCA center and scale
  lower_bounds <- base::as.numeric(target_ranges_df["lower", metrics_for_projection])
  upper_bounds <- base::as.numeric(target_ranges_df["upper", metrics_for_projection])
  
  pca_center <- pca_result$center[metrics_for_projection]
  pca_scale <- pca_result$scale[metrics_for_projection]
  
  scaled_lower <- (lower_bounds - pca_center) / pca_scale
  scaled_upper <- (upper_bounds - pca_center) / pca_scale
  
  # 2. Define angles to sweep (e.g., 360 degrees) to find the extreme outer boundary
  angles <- base::seq(0, 2 * base::pi, length.out = 360)
  W <- pca_result$rotation[metrics_for_projection, 1:2]
  
  # 3. Create an empty matrix to hold the extreme corners in scaled original space
  extreme_scaled_points <- base::matrix(NA, nrow = base::length(angles), ncol = base::length(metrics_for_projection))
  
  for (i in 1:base::length(angles)) {
    # Define a 2D direction vector for the current angle
    dir_2d <- base::c(base::cos(angles[i]), base::sin(angles[i]))
    
    # Calculate how each dimension contributes to this 2D direction
    w_dir <- W %*% dir_2d
    
    # If the contribution is positive, take the upper bound to maximize the distance. 
    # If negative, take the lower bound.
    selected_pt <- base::ifelse(w_dir > 0, scaled_upper, scaled_lower)
    extreme_scaled_points[i, ] <- selected_pt
  }
  
  # 4. Project these specific extreme points into PCA space
  projected_corners <- base::as.data.frame(extreme_scaled_points %*% W)
  base::colnames(projected_corners) <- base::c("PC1", "PC2")
  
  # 5. Calculate the convex hull of these projected boundaries
  hull_indices <- geometry::convhulln(base::as.matrix(projected_corners))
  hull_points <- projected_corners[hull_indices, ]
  
  # Order hull points for plotting
  hull_centroid_x <- base::mean(hull_points$PC1)
  hull_centroid_y <- base::mean(hull_points$PC2)
  angles <- base::atan2(hull_points$PC2 - hull_centroid_y, hull_points$PC1 - hull_centroid_x)
  target_hull <- hull_points[base::order(angles), ]
  target_hull <- base::rbind(target_hull, target_hull[1, ]) # Close the polygon
  
  
  # Plot PCA
  variance_explained <- base::round((pca_result$sdev^2 / base::sum(pca_result$sdev^2)) * 100, 2)
  
  p <- ggplot2::ggplot() +
    # Plot the target space polygon
    ggplot2::geom_polygon(data = target_hull,
                          ggplot2::aes(x = PC1, y = PC2),
                          fill = "lightgreen", alpha = 0.5, color = "darkgreen",
                          linetype = "dashed", linewidth = 0.8) +
    
    # Optionally, add a path to show the trajectory over time
    ggplot2::geom_path(data = observations_pca,
                       ggplot2::aes(x = PC1, y = PC2, color = base::factor(run.id)), 
                       alpha = 0.5) +
    
    # Use a sequential color scale appropriate for time
    ggplot2::scale_color_viridis_d() +
    ggplot2::guides() + # <-- Added the missing '+' here!
    ggplot2::labs(
      title = "PCA Trajectory of Observations Over Time",
      subtitle = "Green polygon represents the projected target space",
      x = base::paste0("Principal Component 1 (", variance_explained[1], "%)"),
      y = base::paste0("Principal Component 2 (", variance_explained[2], "%)"),
      color = "Year" 
    ) +
    ggplot2::coord_fixed(ratio = 1) + 
    ggplot2::theme_bw()
  
  p
}