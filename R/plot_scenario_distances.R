#' Plot Scenario Euclidean Distances
#'
#' This function reads the multivariate distance dataset and generates ggplots
#' evaluating the Euclidean distance of scenarios from the reference state.
#' It averages the distance across years for each run to cleanly visualize
#' the scenario trends.
#'
#' @param dist_file Character. Path to the RDS file containing the multivariate
#'   distance data (e.g., "out_dir/scenario_multvar_distance.rds").
#' @param out_dir Character. Directory where the generated plots will be saved.
#' @param min.year Numeric. Optional minimum year to truncate the data before averaging.
#' @param max.year Numeric. Optional maximum year to truncate the data before averaging.
#'
#' @return A list containing the ggplot objects (returned invisibly).
#'
#' @importFrom dplyr filter group_by summarize mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_viridis_d theme_minimal theme element_text labs
#' @importFrom grDevices pdf dev.off
#'
#' @export

dist_file = paste0(out_dir, "scenario_multvar_distance.rds")
out_dir = 'Z:/atlantiseof/figures/'
min.year = 35
max.year = 55

plot_scenario_distances <- function(dist_file, out_dir, min.year = NULL, max.year = NULL) {
  
  # 1. Read Data ----
  message("Reading multivariate distance data...")
  multivar_data <- readRDS(dist_file)
  
  # 1.5. Truncate by Year ----
  if (!is.null(min.year)) {
    message("Filtering data to year >= ", min.year)
    multivar_data <- multivar_data |> dplyr::filter(year >= min.year)
  }
  if (!is.null(max.year)) {
    message("Filtering data to year <= ", max.year)
    multivar_data <- multivar_data |> dplyr::filter(year <= max.year)
  }
  
  # 2. Summarize Data by Run ----
  message("Averaging distance across years for each run...")
  # We average across years to prevent massive overplotting (39k+ points)
  run_summaries <- multivar_data |>
    dplyr::group_by(
      run.id, 
      scenario_name, 
      scenario_original_name, 
      dominant_group, 
      dominance_factor, 
      eof_threshold, 
      catch.scalar
    ) |>
    dplyr::summarize(
      mean_multivar_dist = mean(multivar_euclidean_dist, na.rm = TRUE),
      sd_multivar_dist = stats::sd(multivar_euclidean_dist, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # 3. Setup PDF Output ----
  pdf_file <- file.path(out_dir, "scenario_distances_by_group.pdf")
  message("Generating plots to PDF: ", pdf_file)
  grDevices::pdf(file = pdf_file, width = 8, height = 6)
  
  plot_list <- list()
  
  subtitle_text <- sprintf("Averaged over years %s to %s", 
                           ifelse(is.null(min.year), "Start", min.year), 
                           ifelse(is.null(max.year), "End", max.year))
  
  # 4. Plot Targeting Scenarios (One page per dominant_group) ----
  targeting_data <- run_summaries |>
    dplyr::filter(scenario_name %in% c("targeting", "targeted")) |>
    dplyr::mutate(dominance_factor = as.factor(dominance_factor))
  
  dominant_groups <- sort(unique(targeting_data$dominant_group))
  
  for (grp in dominant_groups) {
    grp_data <- targeting_data |> dplyr::filter(dominant_group == grp)
    
    p <- ggplot2::ggplot(
      data = grp_data, 
      ggplot2::aes(
        x = catch.scalar, 
        y = mean_multivar_dist, 
        # color = dominance_factor, 
        # group = dominance_factor
      )
    ) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::geom_line(alpha = 0.8, linewidth = 1) +
      ggplot2::facet_wrap(~dominance_factor)+
      # ggplot2::scale_color_viridis_d(name = "Dominance\nFactor", option = "plasma") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "right",
        plot.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::labs(
        title = paste("Targeting Scenario:", grp),
        subtitle = subtitle_text,
        x = "Catch Scalar",
        y = "Mean Multivariate Euclidean Distance"
      )
    
    print(p)
    plot_list[[paste0("targeting_", grp)]] <- p
  }
  
  # 5. Plot Uniform Scenarios ----
  uniform_data <- run_summaries |>
    dplyr::filter(scenario_name == "uniform")
  
  if (nrow(uniform_data) > 0) {
    p_uniform <- ggplot2::ggplot(
      data = uniform_data, 
      ggplot2::aes(
        x = catch.scalar, 
        y = mean_multivar_dist
      )
    ) +
      ggplot2::geom_point(size = 2, alpha = 0.8, color = "#2c7bb6") +
      ggplot2::geom_line(alpha = 0.8, color = "#2c7bb6", linewidth = 1) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::labs(
        title = "Uniform Catch Scenario",
        subtitle = subtitle_text,
        x = "Catch Scalar",
        y = "Mean Multivariate Euclidean Distance"
      )
    
    print(p_uniform)
    plot_list[["uniform"]] <- p_uniform
  }
  
  # 6. Close PDF device ----
  grDevices::dev.off()
  message("  PDF generation complete.")
  
  # Return the plots as a list so they can be viewed in the R session
  return(invisible(plot_list))
}