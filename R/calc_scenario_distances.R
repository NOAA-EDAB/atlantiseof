#' Calculate Euclidean Distance from Reference State
#'
#' This function takes the pruned Atlantis EOF scenario dataset and calculates
#' both the univariate difference (experiment - reference) and the multivariate
#' Euclidean distance for each scenario/year combination relative to the 
#' mean reference state.
#'
#' @param pruned_data_file Character. Path to the RDS file containing the pruned 
#'   scenario data (e.g., "out_dir/atlantis_EOF_scenario_data_pruned.rds").
#' @param out_dir Character. Directory where the calculated distance datasets 
#'   will be saved.
#'
#' @return A list containing `scenario_data_distance` and `scenario_multvar_distance` 
#'   (returned invisibly).
#'
#' @importFrom dplyr filter group_by summarize left_join mutate select any_of
#' @importFrom utils read.csv
#'
#' @export

calc_scenario_distances <- function(pruned_data_file, out_dir) {
  
  # 1. Read Data ----
  message("Reading pruned scenario data...")
  scenario_data_pruned <- readRDS(pruned_data_file)
  
  # 2. Extract and Average Reference Data ----
  message("Calculating mean reference values...")
  # Separate the reference data and calculate the mean over all years 
  # for both raw and scaled values
  ref_means <- scenario_data_pruned |>
    dplyr::filter(scenario_name == "reference") |>
    dplyr::group_by(Variable) |>
    dplyr::summarize(
      ref_Value_mean = mean(Value, na.rm = TRUE),
      ref_Value_scaled_mean = mean(Value.scaled, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(ref_means) == 0) {
    stop("No 'reference' scenario found in the dataset. Cannot calculate distances.")
  }
  
  # 3. Calculate Univariate Distance (Experiment - Reference) ----
  message("Calculating univariate distances (experiment - reference)...")
  # Filter out the reference scenario so we only evaluate experimental runs
  scenario_data_distance <- scenario_data_pruned |>
    dplyr::filter(scenario_name != "reference") |>
    dplyr::left_join(ref_means, by = "Variable") |>
    dplyr::mutate(
      diff_raw = Value - ref_Value_mean,
      diff_scaled = Value.scaled - ref_Value_scaled_mean
    )
  
  # Save the univariate distance dataset
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  univ_out_file <- file.path(out_dir, "scenario_data_distance.rds")
  saveRDS(scenario_data_distance, univ_out_file)
  message("Univariate distances saved to: ", univ_out_file)
  
  # 4. Calculate Multivariate Euclidean Distance ----
  message("Calculating multivariate Euclidean distance...")
  # Standard Euclidean distance is sqrt( sum( (exp - ref)^2 ) ).
  # We use the scaled difference to ensure variables contribute equally.
  scenario_multvar_distance <- scenario_data_distance |>
    dplyr::group_by(
      year, 
      run.id, 
      scenario_name, 
      scenario_original_name,
      dominant_group, 
      dominance_factor, 
      eof_threshold, 
      catch.scalar
    ) |>
    dplyr::summarize(
      multivar_euclidean_dist = sqrt(sum(diff_scaled^2, na.rm = TRUE)),
      # Optional: also calculate raw euclidean distance if needed
      multivar_euclidean_dist_raw = sqrt(sum(diff_raw^2, na.rm = TRUE)),
      n_variables = dplyr::n(), # Track how many variables went into the distance
      .groups = "drop"
    )
  
  # Save the multivariate distance dataset
  mult_out_file <- file.path(out_dir, "scenario_multvar_distance.rds")
  saveRDS(scenario_multvar_distance, mult_out_file)
  message("Multivariate distances saved to: ", mult_out_file)
  
  # Return both as a list invisibly
  return(invisible(list(
    univariate = scenario_data_distance,
    multivariate = scenario_multvar_distance
  )))
}