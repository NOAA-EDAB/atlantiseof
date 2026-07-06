#' Prune Covariates Based on Spearman Correlation
#'
#' This function performs covariate pruning on the Atlantis EOF scenario dataset.
#' It groups variables by their defined "Role", calculates pairwise Spearman 
#' correlations between them, and iteratively removes variables that are highly 
#' correlated (|r| > threshold). When deciding which variable to drop in a highly
#' correlated pair, it drops the one with the lowest absolute correlation to 
#' the `catch.scalar` target variable.
#'
#' @param data_file Character. Path to the RDS file containing the scenario data 
#'   (e.g., "out_dir/atlantis_EOF_scenario_data.rds").
#' @param def_file Character. Path to the CSV file containing indicator definitions 
#'   (e.g., "data-raw/indicator_defs.csv").
#' @param out_file Character. Path where the pruned dataset should be saved 
#'   (e.g., "out_dir/atlantis_EOF_scenario_data_pruned.rds").
#' @param cor_threshold Numeric. The absolute Spearman correlation threshold used 
#'   to identify highly correlated variables. Defaults to 0.7.
#' @param plot_diagnostics Logical. If TRUE, prints a paneled ggplot of the 
#'   pre-pruning correlation matrices for each role. Defaults to FALSE.
#'
#' @return A data frame containing the pruned scenario data (returned invisibly).
#'
#' @importFrom dplyr select filter group_by summarize left_join mutate all_of bind_rows rename arrange
#' @importFrom tidyr pivot_wider
#' @importFrom stats cor
#' @importFrom utils read.csv
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c scale_fill_gradient2 facet_wrap theme_minimal theme element_text element_blank labs geom_vline geom_hline
#'
#' @export
prune_covariates <- function(data_file, 
                             def_file, 
                             out_file, 
                             cor_threshold = 0.7,
                             plot_diagnostics = FALSE) {
  
  # 1. Read Data ----
  message("Reading scenario data and indicator definitions...")
  scenario_data <- readRDS(data_file)
  ind_data <- utils::read.csv(def_file)
  
  # Optional: Keep only indicators marked as 'Used' if applicable
  if ("Used" %in% names(ind_data)) {
    ind_data <- ind_data |> dplyr::filter(Used == 1)
  }
  
  # 1.5. Remove variables that are entirely NA ----
  message("Checking for and removing all-NA variables...")
  na_summary <- scenario_data |>
    dplyr::filter(Variable %in% ind_data$Variable) |>
    dplyr::group_by(Variable) |>
    dplyr::summarize(all_na = all(is.na(Value)), .groups = "drop")
  
  vars_all_na <- na_summary |>
    dplyr::filter(all_na) |>
    dplyr::pull(Variable)
  
  if (length(vars_all_na) > 0) {
    message(sprintf("  Dropped %d all-NA variables: %s", 
                    length(vars_all_na), 
                    paste(vars_all_na, collapse = ", ")))
    scenario_data <- scenario_data |> dplyr::filter(!Variable %in% vars_all_na)
    ind_data <- ind_data |> dplyr::filter(!Variable %in% vars_all_na)
  }
  
  # 2. Calculate Correlation to catch.scalar ----
  message("Calculating baseline correlations to catch.scalar...")
  # We calculate the absolute Spearman correlation for each variable against catch.scalar
  target_cors <- scenario_data |>
    dplyr::filter(Variable %in% ind_data$Variable) |>
    dplyr::group_by(Variable) |>
    dplyr::summarize(
      # Suppressing warnings that happen when a variable has 0 variance (e.g. all 0s)
      cor_catch = suppressWarnings(abs(stats::cor(
        x = Value, 
        y = catch.scalar, 
        method = "spearman", 
        use = "pairwise.complete.obs"
      ))),
      .groups = "drop"
    ) |>
    # Fill NAs with 0 (e.g. if standard deviation was 0, correlation couldn't be calculated)
    dplyr::mutate(cor_catch = ifelse(is.na(cor_catch), 0, cor_catch)) |>
    dplyr::left_join(ind_data, by = "Variable")
  
  # 3. Prepare Wide Data for Inter-variable Correlation ----
  # Pivot wider so that we have Year + Run.ID as rows, and Variables as columns
  wide_data <- scenario_data |>
    dplyr::filter(Variable %in% ind_data$Variable) |>
    dplyr::select(year, run.id, scenario_name, Variable, Value) |>
    tidyr::pivot_wider(
      id_cols = c(year, run.id, scenario_name), 
      names_from = Variable, 
      values_from = Value
    )
  
  # 4. Optional Diagnostic Plot ----
  if (plot_diagnostics) {
    message("Generating diagnostic correlation plot...")
    cor_df_list <- list()
    roles <- unique(target_cors$Variable.Role)
    
    for (role in roles) {
      vars_in_role <- target_cors$Variable[target_cors$Variable.Role == role]
      if (length(vars_in_role) > 1) {
        cor_mat <- suppressWarnings(stats::cor(
          x = wide_data |> dplyr::select(dplyr::all_of(vars_in_role)),
          method = "spearman",
          use = "pairwise.complete.obs"
        ))
        
        cor_long <- as.data.frame(as.table(cor_mat)) |>
          dplyr::rename(Var1 = Var1, Var2 = Var2, Cor = Freq) |>
          dplyr::mutate(Role = role, AbsCor = abs(Cor))
        
        cor_df_list[[role]] <- cor_long
      }
    }
    
    if (length(cor_df_list) > 0) {
      plot_data <- dplyr::bind_rows(cor_df_list)
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Var1, y = Var2, fill = AbsCor)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_viridis_c(limits = c(0, 1), name = "|Spearman r|") +
        ggplot2::facet_wrap(~ Role, scales = "free") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 6),
          axis.text.y = ggplot2::element_text(size = 6),
          axis.title = ggplot2::element_blank()
        ) +
        ggplot2::labs(title = "Pre-Pruning Indicator Correlation by Role")
      
      print(p)
    } else {
      message("Not enough variables in any role to generate correlation plot.")
    }
  }
  
  # 5. Pruning Process by Role ----
  message(sprintf("Pruning variables by role (threshold = %s)...", cor_threshold))
  variables_to_keep <- c()
  roles <- unique(target_cors$Variable.Role)
  
  for (role in roles) {
    # Get all variables belonging to the current role
    vars_in_role <- target_cors$Variable[target_cors$Variable.Role == role]
    
    # If there is only 1 or 0 variables in this role, nothing to prune
    if (length(vars_in_role) <= 1) {
      variables_to_keep <- c(variables_to_keep, vars_in_role)
      next
    }
    
    current_vars <- vars_in_role
    
    # Iteratively prune highly correlated pairs
    while (TRUE) {
      if (length(current_vars) <= 1) break
      
      # Calculate correlation matrix for the current subset of variables
      cor_mat <- suppressWarnings(stats::cor(
        x = wide_data |> dplyr::select(dplyr::all_of(current_vars)),
        method = "spearman",
        use = "pairwise.complete.obs"
      ))
      
      # Set diagonal to 0 so we don't flag self-correlations
      diag(cor_mat) <- 0
      
      # Find the maximum absolute correlation in the matrix
      max_cor <- max(abs(cor_mat), na.rm = TRUE)
      
      # If the highest correlation is below our threshold (or all are NA), stop pruning this role
      if (is.infinite(max_cor) || max_cor <= cor_threshold) {
        break
      }
      
      # Find the indices of the pair with the maximum correlation
      # arr.ind returns the row/col indices of the match
      idx <- which(abs(cor_mat) == max_cor, arr.ind = TRUE)
      
      # Get the names of the two highly correlated variables
      var1 <- rownames(cor_mat)[idx[1, 1]]
      var2 <- colnames(cor_mat)[idx[1, 2]]
      
      # Look up their correlations with catch.scalar
      c1 <- target_cors$cor_catch[target_cors$Variable == var1]
      c2 <- target_cors$cor_catch[target_cors$Variable == var2]
      
      # Drop the one with the weaker relationship to the target
      if (c1 < c2) {
        current_vars <- setdiff(current_vars, var1)
        message(sprintf("  [%s] Dropped '%s' in favor of '%s' (max_cor = %.2f)", role, var1, var2, max_cor))
      } else {
        current_vars <- setdiff(current_vars, var2)
        message(sprintf("  [%s] Dropped '%s' in favor of '%s' (max_cor = %.2f)", role, var2, var1, max_cor))
      }
    }
    
    # Add surviving variables from this role to our master list
    variables_to_keep <- c(variables_to_keep, current_vars)
  }
  
  # 6. Filter and Save Output ----
  message(sprintf("\nPruning complete. Kept %d variables out of %d.", length(variables_to_keep), length(ind_data$Variable)))
  
  scenario_data_pruned <- scenario_data |>
    dplyr::filter(Variable %in% variables_to_keep)
  
 
  saveRDS(scenario_data_pruned, out_file)
  message("Pruned dataset saved to: ", out_file)
  
  # 7. Final Diagnostic Plot ----
  if (plot_diagnostics && length(variables_to_keep) > 1) {
    message("Generating final pruned correlation plot...")
    
    # Order variables by Role for the plot
    var_meta <- target_cors |> 
      dplyr::filter(Variable %in% variables_to_keep) |> 
      dplyr::arrange(Variable.Role, Variable)
    
    var_order <- var_meta$Variable
    
    # Ensure we only include numeric classes (removes any 'character' class variables)
    valid_numeric_vars <- names(wide_data)[sapply(wide_data, is.numeric)]
    var_order <- intersect(var_order, valid_numeric_vars)
    
    # Re-filter metadata in case any character variables were dropped
    var_meta <- var_meta |> dplyr::filter(Variable %in% var_order)
    
    cor_mat_final <- suppressWarnings(stats::cor(
      x = wide_data |> dplyr::select(dplyr::all_of(var_order)),
      method = "spearman",
      use = "pairwise.complete.obs"
    ))
    
    cor_long_final <- as.data.frame(as.table(cor_mat_final)) |>
      dplyr::rename(Var1 = Var1, Var2 = Var2, Cor = Freq) |>
      dplyr::mutate(
        AbsCor = abs(Cor),
        Var1 = factor(Var1, levels = var_order),
        Var2 = factor(Var2, levels = rev(var_order)) # reverse so diagonal is top-left to bottom-right
      )
    
    # Determine line breaks for visual grouping
    role_counts <- var_meta |> 
      dplyr::group_by(Variable.Role) |> 
      dplyr::summarize(n = length(Variable), .groups = "drop") |> 
      dplyr::mutate(cum_n = cumsum(n))
    
    line_breaks_x <- role_counts$cum_n[-nrow(role_counts)] + 0.5
    line_breaks_y <- length(var_order) - role_counts$cum_n[-nrow(role_counts)] + 0.5
    
    p_final <- ggplot2::ggplot(cor_long_final, ggplot2::aes(x = Var1, y = Var2, fill = AbsCor)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(
        low = "#4575b4", mid = "#ffffbf", high = "#d73027", 
        midpoint = cor_threshold, 
        limits = c(0, 1), 
        name = "|Spearman r|"
      ) +
      ggplot2::geom_vline(xintercept = line_breaks_x, color = "black", linewidth = 0.5) +
      ggplot2::geom_hline(yintercept = line_breaks_y, color = "black", linewidth = 0.5) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6),
        axis.text.y = ggplot2::element_text(size = 6),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank() # Remove grid to make grouping lines stand out
      ) +
      ggplot2::labs(
        title = "Post-Pruning Indicator Correlation (All Kept Variables)",
        subtitle = paste("Ordered and visually grouped by Role. Diverging at r =", cor_threshold)
      )
    
    print(p_final)
  }
  
  return(invisible(scenario_data_pruned))
}