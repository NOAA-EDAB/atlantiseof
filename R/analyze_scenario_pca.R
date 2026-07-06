#' Perform PCA and Analyze Scenario Distances
#'
#' This function reshapes the scaled indicator data, performs a Principal Component 
#' Analysis (PCA), and analytically determines if the "reference" state falls 
#' outside the 95% confidence ellipses of various scenario groups. It outputs 
#' both analytical tables and diagnostic biplots, including trajectory maps
#' and distance response surfaces.
#'
#' @param data_file Character. Path to the RDS file containing the scaled scenario data.
#' @param out_dir Character. Directory where the outputs (plots and data) will be saved.
#' @param start_year Numeric. Optional year to start filtering the data before analysis.
#' @param stop_year Numeric. Optional year to stop filtering the data before analysis.
#' @param annual Logical. If TRUE, keeps annual resolution. If FALSE, averages indicator
#'   values across years for each run prior to performing the PCA. Defaults to TRUE.
#' @param scenario_names Character vector. Specific scenario names to include in the analysis 
#'   (e.g., c("targeting", "uniform")). Defaults to "all". The "reference" scenario is always kept.
#' @param bin_width Numeric. Optional size of year bins. If provided and annual = TRUE, 
#'   groups data into blocks of `bin_width` years for plotting and trajectory mapping.
#'
#' @return A list containing the PCA outputs, analytical results, and ggplot objects.
#'
#' @importFrom dplyr select filter group_by summarise mutate bind_rows all_of arrange pull
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom stats prcomp cov mahalanobis pchisq qchisq
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text stat_ellipse labs theme_minimal theme scale_color_viridis_d ggsave geom_path geom_line arrow unit scale_size_continuous facet_grid facet_wrap geom_hline
#' @importFrom grDevices pdf dev.off
#'
#' @export
analyze_scenario_pca <- function(data_file, out_dir, start_year = NULL, stop_year = NULL, annual = TRUE, scenario_names = "all", bin_width = NULL) {
  
  # Determine file suffix based on the annual flag
  file_suffix <- ifelse(annual, "_annual", "")
  
  # 1. Read and Prep Data ----
  message("Reading data and preparing for PCA...")
  data <- readRDS(data_file)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Filter by year range if specified
  if (!is.null(start_year)) {
    message("Filtering data: year >= ", start_year)
    data <- data |> dplyr::filter(year >= start_year)
  }
  if (!is.null(stop_year)) {
    message("Filtering data: year <= ", stop_year)
    data <- data |> dplyr::filter(year <= stop_year)
  }
  
  # Create Year Bins if requested and annual is TRUE
  if (annual && !is.null(bin_width)) {
    min_y <- ifelse(is.null(start_year), min(data$year, na.rm = TRUE), start_year)
    max_y <- ifelse(is.null(stop_year), max(data$year, na.rm = TRUE), stop_year)
    
    breaks <- seq(min_y, max_y + bin_width, by = bin_width)
    labels <- paste(breaks[-length(breaks)], breaks[-length(breaks)] + bin_width - 1, sep = "-")
    data$year_bin <- cut(data$year, breaks = breaks, right = FALSE, labels = labels)
  } else if (annual) {
    # Ensure it's an ordered factor so x-axis plots chronologically, not alphabetically
    data$year_bin <- factor(data$year, levels = sort(unique(data$year)))
  } else {
    data$year_bin <- factor("All")
  }
  
  # Filter by scenario names if specified
  if (!("all" %in% scenario_names)) {
    message("Filtering data to scenarios: ", paste(scenario_names, collapse = ", "))
    # Always keep "reference" so that centroid/distance math doesn't break
    data <- data |> dplyr::filter(scenario_name %in% c(scenario_names, "reference"))
  }
  
  # Extract variable names for PCA matrix
  indicator_vars <- unique(data$Variable)
  
  # Average duplicates and prepare long format
  wide_data_long <- data |>
    dplyr::select(year, year_bin, run.id, scenario_name, dominant_group, dominance_factor, catch.scalar, Variable, Value.scaled) |>
    # Average across duplicates if any exist
    dplyr::group_by(year, year_bin, run.id, scenario_name, dominant_group, dominance_factor, catch.scalar, Variable) |>
    dplyr::summarise(Value.scaled = mean(Value.scaled, na.rm = TRUE), .groups = "drop")
  
  # If annual flag is FALSE, average experimental data for each run into a single point, 
  # but strictly keep reference data un-averaged so its natural interannual variance defines the ellipse
  if (!annual) {
    message("Averaging indicators across all years for experimental runs prior to PCA...")
    
    ref_long <- wide_data_long |> 
      dplyr::filter(scenario_name == "reference")
    
    exp_long <- wide_data_long |>
      dplyr::filter(scenario_name != "reference") |>
      dplyr::group_by(run.id, scenario_name, dominant_group, dominance_factor, catch.scalar, Variable) |>
      dplyr::summarise(Value.scaled = mean(Value.scaled, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(year_bin = factor("All"), year = NA)
    
    wide_data_long <- dplyr::bind_rows(ref_long, exp_long)
  }
  
  # Pivot to wide format (Rows = Runs/Years, Columns = Indicators)
  wide_data <- wide_data_long |>
    tidyr::pivot_wider(names_from = Variable, values_from = Value.scaled)
  
  # Check for and remove indicators that are entirely NA
  all_na_vars <- indicator_vars[sapply(wide_data[indicator_vars], function(x) all(is.na(x)))]
  if (length(all_na_vars) > 0) {
    message("  Dropping variables because they are entirely NA: ", paste(all_na_vars, collapse = ", "))
    indicator_vars <- setdiff(indicator_vars, all_na_vars)
    wide_data <- wide_data |> dplyr::select(-dplyr::all_of(all_na_vars))
  }
  
  # Impute remaining NAs with the column mean so prcomp() can run safely
  wide_data <- wide_data |>
    dplyr::mutate(dplyr::across(dplyr::all_of(indicator_vars), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # 2. Perform Global PCA ----
  message("Performing Principal Component Analysis...")
  pca_matrix <- wide_data |> dplyr::select(dplyr::all_of(indicator_vars))
  
  # Run PCA. Data is min-max scaled, but we center it for standard PCA.
  pca_res <- stats::prcomp(pca_matrix, center = TRUE, scale. = FALSE)
  
  # Extract Scores and merge with metadata
  scores <- as.data.frame(pca_res$x)
  pca_data <- cbind(wide_data |> dplyr::select(-dplyr::all_of(indicator_vars)), scores)
  
  # Extract Loadings
  loadings <- as.data.frame(pca_res$rotation)
  loadings$Variable <- rownames(loadings)
  
  # Isolate Reference Point(s)
  ref_data <- pca_data |> dplyr::filter(scenario_name == "reference")
  exp_data <- pca_data |> dplyr::filter(scenario_name != "reference")
  
  if (nrow(ref_data) == 0) {
    stop("No 'reference' scenario found in the dataset to perform analytical comparisons.")
  }
  
  # Calculate mean reference point (centroid of natural variance)
  ref_centroid <- c(PC1 = mean(ref_data$PC1), PC2 = mean(ref_data$PC2))
  
  # 2.5 Calculate Reference Ellipse Containment ----
  ref_containment <- NULL
  if (nrow(ref_data) > 2) {
    message("Calculating reference ellipse containment for scenarios...")
    
    # Calculate the interannual covariance matrix of the reference scenario
    ref_cov <- stats::cov(ref_data[, c("PC1", "PC2")])
    
    # Treat each scenario and run as a unique identifier by finding its centroid
    run_centroids <- exp_data |>
      dplyr::group_by(run.id, scenario_name, dominant_group, dominance_factor, catch.scalar) |>
      dplyr::summarize(
        PC1 = mean(PC1, na.rm = TRUE),
        PC2 = mean(PC2, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate distance of each experimental scenario centroid to the reference centroid
    ref_containment <- run_centroids |>
      dplyr::mutate(
        Mahalanobis_D2 = stats::mahalanobis(
          as.matrix(cbind(PC1, PC2)), 
          center = ref_centroid, 
          cov = ref_cov
        ),
        # 95% bounds of a bivariate normal distribution correspond to chisq with 2 DF
        Inside_Reference_Ellipse = Mahalanobis_D2 <= stats::qchisq(0.95, df = 2)
      ) |>
      # Output shows each unique scenario that is WITHIN the reference ellipse
      dplyr::filter(Inside_Reference_Ellipse) |>
      dplyr::select(run.id, scenario_name, dominant_group, dominance_factor, catch.scalar, 
                    PC1, PC2, Mahalanobis_D2)
    
    contain_file <- file.path(out_dir, paste0("scenarios_within_reference_ellipse", file_suffix, ".csv"))
    write.csv(ref_containment, contain_file, row.names = FALSE)
    message("  Scenarios within reference ellipse saved to: ", contain_file)
  }
  
  # 3. Analytical Determination (Mahalanobis Distance for Groups) ----
  message("Analytically determining if reference lies outside 95% confidence ellipses...")
  
  # Helper function to check ellipse containment
  check_ellipse <- function(df, group_col, group_name, ref_pt) {
    groups <- unique(df[[group_col]])
    groups <- groups[!is.na(groups)]
    
    results <- list()
    for (g in groups) {
      g_df <- df |> dplyr::filter(!!rlang::sym(group_col) == g)
      if (nrow(g_df) > 2) {
        cov_mat <- stats::cov(g_df[, c("PC1", "PC2")])
        cent <- colMeans(g_df[, c("PC1", "PC2")])
        
        # Calculate Mahalanobis Distance Squared (D2)
        md2 <- stats::mahalanobis(ref_pt, cent, cov_mat)
        
        # P-value from Chi-Square distribution (df=2 for PC1 & PC2)
        pval <- stats::pchisq(md2, df = 2, lower.tail = FALSE)
        
        results[[length(results) + 1]] <- data.frame(
          Comparison = group_name,
          Group = as.character(g),
          Mahalanobis_D2 = md2,
          P_Value = pval,
          Outside_95_Ellipse = pval < 0.05
        )
      }
    }
    return(dplyr::bind_rows(results))
  }
  
  # Run Checks
  res_scenario <- check_ellipse(exp_data, "scenario_name", "Scenario Overall", ref_centroid)
  
  target_data <- exp_data |> dplyr::filter(scenario_name %in% c("targeting", "targeted"))
  res_group <- check_ellipse(target_data, "dominant_group", "Targeting: Dominant Group", ref_centroid)
  res_factor <- check_ellipse(target_data, "dominance_factor", "Targeting: Dominance Factor", ref_centroid)
  
  analytical_results <- dplyr::bind_rows(res_scenario, res_group, res_factor)
  
  # Save Analytical Results
  res_file <- file.path(out_dir, paste0("pca_ellipse_analytical_results", file_suffix, ".csv"))
  write.csv(analytical_results, res_file, row.names = FALSE)
  message("  Analytical results saved to: ", res_file)
  
  # 4. Generate Plots ----
  message("Generating PCA biplots...")
  
  # Calculate scaling factor for loadings arrows to fit nicely on the plot
  max_score <- max(abs(c(exp_data$PC1, exp_data$PC2)))
  max_load <- max(abs(c(loadings$PC1, loadings$PC2)))
  scale_fac <- (max_score / max_load) * 0.8 
  
  # Variance explained for axis labels
  var_explained <- summary(pca_res)$importance[2, 1:2] * 100
  xlab_str <- sprintf("PC1 (%.1f%% Variance)", var_explained[1])
  ylab_str <- sprintf("PC2 (%.1f%% Variance)", var_explained[2])
  
  # Base Biplot Function
  create_biplot <- function(df, color_col, title, subtitle, show_ellipse = TRUE) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = PC1, y = PC2)) 
    
    # Plot the Reference Interannual Ellipse if annual == FALSE
    if (!annual && nrow(ref_data) > 2) {
      p <- p + ggplot2::stat_ellipse(
        data = ref_data, 
        ggplot2::aes(x = PC1, y = PC2),
        color = "red", linetype = "dashed", type = "norm", level = 0.95, linewidth = 0.8,
        inherit.aes = FALSE
      )
    } else if (annual && nrow(ref_data) > 1) {
      # Plot individual Reference Interannual points if annual == TRUE
      p <- p + ggplot2::geom_point(
        data = ref_data,
        ggplot2::aes(x = PC1, y = PC2),
        color = "firebrick", alpha = 0.3, size = 1.5, shape = 16,
        inherit.aes = FALSE
      )
    }
    
    # Plot the actual experimental scenario points
    p <- p + ggplot2::geom_point(ggplot2::aes(color = as.factor(.data[[color_col]])), alpha = 0.5)
    
    if (show_ellipse) {
      # Safety check: stat_ellipse requires at least 3 points and a positive definite 
      # covariance matrix to avoid chol() failures (e.g. points in a perfect straight line).
      valid_ellipse_data <- df |> 
        dplyr::group_by(.data[[color_col]]) |> 
        dplyr::filter(dplyr::n() >= 3) |> 
        dplyr::filter({
          # Calculate covariance matrix for the group
          cv <- stats::cov(cbind(PC1, PC2))
          # Keep group only if variance is non-zero (cv[1,1] > 0) and it has an area (det > 0)
          !any(is.na(cv)) && cv[1, 1] > 1e-8 && det(cv) > 1e-8
        }) |> 
        dplyr::ungroup()
      
      if (nrow(valid_ellipse_data) > 0) {
        p <- p + ggplot2::stat_ellipse(
          data = valid_ellipse_data, 
          ggplot2::aes(color = as.factor(.data[[color_col]])), 
          type = "norm", level = 0.95, linewidth = 1
        )
      }
    }
    
    caption_text <- ifelse(show_ellipse, 
                           "Red triangle = Reference Centroid. Colored ellipses = 95% group bounds.",
                           "Red triangle = Reference Centroid.")
    if (!annual && nrow(ref_data) > 2) {
      caption_text <- paste(caption_text, "\nRed dashed ellipse = 95% Reference interannual bounds.")
    } else if (annual && nrow(ref_data) > 1) {
      caption_text <- paste(caption_text, "\nFaint red points = Reference interannual spread.")
    }
    
    p <- p +
      # Add Loadings (Variables)
      ggplot2::geom_segment(data = loadings, ggplot2::aes(x = 0, y = 0, xend = PC1 * scale_fac, yend = PC2 * scale_fac), 
                            arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "grey30", alpha = 0.7) +
      ggplot2::geom_text(data = loadings, ggplot2::aes(x = PC1 * scale_fac * 1.1, y = PC2 * scale_fac * 1.1, label = Variable), 
                         color = "black", size = 3) +
      
      # Add Reference Point (Centroid)
      ggplot2::geom_point(data = data.frame(PC1 = ref_centroid[1], PC2 = ref_centroid[2]), 
                          shape = 24, fill = "red", color = "black", size = 4, stroke = 1.5) +
      
      ggplot2::scale_color_viridis_d(name = color_col, option = "plasma") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = title, subtitle = subtitle, x = xlab_str, y = ylab_str,
                    caption = caption_text)
    
    return(p)
  }
  
  p1 <- create_biplot(exp_data, "scenario_name", "PCA: Uniform vs. Targeting", "Factor loadings compared across experimental designs")
  
  p2 <- create_biplot(target_data |> tidyr::drop_na(dominant_group), "dominant_group", 
                      "PCA: Targeting by Dominant Group", "Focusing exclusively on targeting scenarios")
  
  p3 <- create_biplot(target_data |> tidyr::drop_na(dominance_factor), "dominance_factor", 
                      "PCA: Targeting by Dominance Factor", "Focusing exclusively on targeting scenarios")
  
  p6 <- create_biplot(exp_data |> tidyr::drop_na(catch.scalar), "catch.scalar", 
                      "PCA: Scenarios by Catch Scalar", "Comparing experimental designs across catch multiplier")
  
  p7 <- NULL
  if (annual && !is.null(bin_width)) {
    p7 <- create_biplot(exp_data |> tidyr::drop_na(year_bin), "year_bin", 
                        "PCA: Scenarios by Year Bin", "Comparing experimental designs across time blocks")
  }
  
  # 4.5. Generate Intersection and Trajectory Plots ----
  message("Generating PCA trajectory and distance departure plots...")
  
  # Extract scores. We ALWAYS retain year_bin to enable plotting it on the x-axis for p8/p9.
  run_scores <- target_data |>
    dplyr::filter(!is.na(dominant_group)) |>
    dplyr::group_by(run.id, dominant_group, dominance_factor, catch.scalar, year_bin) |>
    dplyr::summarise(
      PC1 = mean(PC1, na.rm = TRUE), 
      PC2 = mean(PC2, na.rm = TRUE), 
      .groups = "drop"
    )
  
  run_scores <- run_scores |>
    # Calculate Euclidean distance to the reference state in PC space
    dplyr::mutate(
      dist_to_ref = sqrt((PC1 - ref_centroid[1])^2 + (PC2 - ref_centroid[2])^2)
    ) |>
    # Sort strictly to ensure path lines connect chronologically by increasing catch intensity and time
    dplyr::arrange(dominant_group, dominance_factor, catch.scalar, year_bin)
  
  # Plot 4: State Trajectories (Path maps in PCA Space)
  caption_p4 <- "Red triangle represents the unperturbed Reference state. Grouped by targeted guild."
  if (!annual && nrow(ref_data) > 2) {
    caption_p4 <- paste(caption_p4, "\nRed dashed ellipse = 95% Reference interannual bounds.")
  } else if (annual && nrow(ref_data) > 1) {
    caption_p4 <- paste(caption_p4, "\nFaint red points = Reference interannual spread.")
  }
  
  p4 <- ggplot2::ggplot(run_scores, ggplot2::aes(x = PC1, y = PC2)) +
    # Draw reference centroid
    ggplot2::geom_point(data = data.frame(PC1 = ref_centroid[1], PC2 = ref_centroid[2]), 
                        shape = 24, fill = "red", color = "black", size = 5, stroke = 1.5)
  
  if (!annual && nrow(ref_data) > 2) {
    p4 <- p4 + ggplot2::stat_ellipse(
      data = ref_data, 
      ggplot2::aes(x = PC1, y = PC2),
      color = "red", linetype = "dashed", type = "norm", level = 0.95, linewidth = 0.8,
      inherit.aes = FALSE
    )
  } else if (annual && nrow(ref_data) > 1) {
    p4 <- p4 + ggplot2::geom_point(
      data = ref_data,
      ggplot2::aes(x = PC1, y = PC2),
      color = "firebrick", alpha = 0.3, size = 1.5, shape = 16,
      inherit.aes = FALSE
    )
  }
  
  p4 <- p4 +
    # Draw paths of change for each dominance factor
    ggplot2::geom_path(ggplot2::aes(group = as.factor(dominance_factor), color = as.factor(dominance_factor)), 
                       arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")), linewidth = 0.8, alpha = 0.7) +
    # Draw points sized by the catch.scalar intensity
    ggplot2::geom_point(ggplot2::aes(size = catch.scalar, color = as.factor(dominance_factor)), alpha = 0.6) +
    ggplot2::scale_color_viridis_d(name = "Dominance\nFactor", option = "plasma") +
    ggplot2::scale_size_continuous(name = "Catch Scalar") +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 10)) +
    ggplot2::labs(
      title = "PCA State Trajectories: How the Ecosystem Departs from Reference",
      subtitle = "Path lines track increasing Catch Scalar (indicated by node size) for each Dominance Factor",
      x = xlab_str, y = ylab_str,
      caption = caption_p4
    )
  
  # Plot 5: Distance Departure Response Surfaces
  p5 <- ggplot2::ggplot(run_scores, ggplot2::aes(x = catch.scalar, y = dist_to_ref, 
                                                 color = as.factor(dominance_factor), 
                                                 group = dominance_factor)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::scale_color_viridis_d(name = "Dominance\nFactor", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", size = 10)) +
    ggplot2::labs(
      title = "Ecosystem Departure from Reference State in PC Space",
      subtitle = "Euclidean distance to Reference centroid in PC1-PC2 coordinate space",
      x = "Catch Scalar",
      y = "Euclidean Distance from Reference State",
      caption = "Tracks tipping points: how fast the ecosystem state degrades as fishing pressure scales."
    )
  
  # Plot 8: PC1 Score Trajectories vs Time
  p8 <- ggplot2::ggplot(run_scores, ggplot2::aes(x = year_bin, y = PC1, 
                                                 color = as.factor(catch.scalar), 
                                                 group = catch.scalar)) +
    ggplot2::geom_hline(yintercept = ref_centroid[1], color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::facet_grid(dominant_group ~ dominance_factor, scales = "free_y") +
    ggplot2::scale_color_viridis_d(name = "Catch\nScalar", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "PC1 Score Trajectories over Time",
      subtitle = "Tracking movement along PC1 axis across time bins by target group and dominance factor",
      x = "Time (Year / Bin)",
      y = xlab_str,
      caption = "Red dashed line indicates the unperturbed Reference state's PC1 score."
    )
  
  # Plot 9: PC2 Score Trajectories vs Time
  p9 <- ggplot2::ggplot(run_scores, ggplot2::aes(x = year_bin, y = PC2, 
                                                 color = as.factor(catch.scalar), 
                                                 group = catch.scalar)) +
    ggplot2::geom_hline(yintercept = ref_centroid[2], color = "red", linetype = "dashed", linewidth = 1) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::facet_grid(dominant_group ~ dominance_factor, scales = "free_y") +
    ggplot2::scale_color_viridis_d(name = "Catch\nScalar", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::labs(
      title = "PC2 Score Trajectories over Time",
      subtitle = "Tracking movement along PC2 axis across time bins by target group and dominance factor",
      x = "Time (Year / Bin)",
      y = ylab_str,
      caption = "Red dashed line indicates the unperturbed Reference state's PC2 score."
    )
  
  # Add dynamic faceting to P4 and P5
  if (annual && !is.null(bin_width)) {
    p4 <- p4 + ggplot2::facet_grid(year_bin ~ dominant_group, scales = "free")
    p5 <- p5 + ggplot2::facet_grid(year_bin ~ dominant_group, scales = "free_y")
  } else {
    p4 <- p4 + ggplot2::facet_wrap(~ dominant_group, scales = "free")
    p5 <- p5 + ggplot2::facet_wrap(~ dominant_group, scales = "free_y")
  }
  
  # Save Plots to PDF ----
  pdf_file <- file.path(out_dir, paste0("pca_scenario_biplots", file_suffix, ".pdf"))
  grDevices::pdf(file = pdf_file, width = 11, height = 8)
  print(p1)
  print(p2)
  print(p3)
  print(p6)
  if (!is.null(p7)) print(p7)
  print(p4)
  print(p5)
  print(p8)
  print(p9)
  grDevices::dev.off()
  message("  Plots saved to: ", pdf_file)
  
  # Create list of plots to return
  plot_list <- list(scenario = p1, dominant_group = p2, dominance_factor = p3, catch_scalar = p6)
  if (!is.null(p7)) plot_list$year_bin <- p7
  plot_list$trajectory <- p4
  plot_list$departure <- p5
  plot_list$pc1_traj <- p8
  plot_list$pc2_traj <- p9
  
  # Save Data ----
  data_out_file <- file.path(out_dir, paste0("pca_scores_and_loadings", file_suffix, ".rds"))
  saveRDS(list(scores = pca_data, loadings = loadings, analysis = analytical_results), data_out_file)
  
  return(invisible(list(
    pca_data = pca_data,
    loadings = loadings,
    analytical_results = analytical_results,
    reference_containment = ref_containment,
    plots = plot_list
  )))
}