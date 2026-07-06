#' Compile Atlantis EOF Scenario Dataset
#'
#' This function processes and combines ecosystem indicator data from multiple Atlantis
#' simulation runs (targeting and uniform scenarios) along with a reference ecosystem state.
#' It calculates summary statistics and scales the indicators for further analysis.
#'
#' @param targeting_run_prefix Character. The prefix used to identify targeting run directories.
#' @param uniform_run_prefix Character. The prefix used to identify uniform run directories.
#' @param targeting_run_root Character. Path to the root directory containing targeting run analysis.
#' @param uniform_run_root Character. Path to the root directory containing uniform run analysis.
#' @param targeting_setup_file Character. Path to the CSV file containing setup parameters for targeting runs.
#' @param uniform_setup_file Character. Path to the CSV file containing setup parameters for uniform runs.
#' @param ref_state_year_file Character. Path to the RDS file containing the annual reference ecosystem state.
#' @param def_file Character. Path to the CSV file containing indicator definitions (e.g., "data-raw/indicator_defs.csv").
#' @param out_dir Character. Directory where the final processed dataset will be saved.
#'
#' @return A data frame containing the combined and scaled scenario data (returned invisibly).
#'
#' @importFrom dplyr mutate left_join rename select bind_rows group_by ungroup any_of
#' @importFrom tidyr pivot_longer
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
#' @importFrom utils read.csv tail
#' @importFrom stats median sd
#'
#' @export

make_scenario_dataset <- function(targeting_run_prefix,
                                  uniform_run_prefix,
                                  targeting_run_root,
                                  uniform_run_root,
                                  targeting_setup_file,
                                  uniform_setup_file,
                                  ref_state_year_file,
                                  def_file,
                                  out_dir) {
  
  num_cores <- parallel::detectCores() - 1
  
  ind.def = read.csv(def_file)
  
  # 1. Get directories in run roots ----
  targeting_run_dirs <- list.files(targeting_run_root, pattern = targeting_run_prefix, full.names = TRUE)
  targeting_run_dirs = targeting_run_dirs[dir.exists(targeting_run_dirs)]
  uniform_run_dirs <- list.files(uniform_run_root, pattern = uniform_run_prefix, full.names = TRUE)
  uniform_run_dirs = uniform_run_dirs[dir.exists(uniform_run_dirs)]
  
  # 2. Read Setup Files ----
  targeting_setup_df <- utils::read.csv(targeting_setup_file) |> 
    dplyr::rename('run.id' = dplyr::starts_with('run'))
  
  uniform_setup_df <- utils::read.csv(uniform_setup_file) |> 
    dplyr::rename('run.id' = dplyr::starts_with('run'),
                  eof_threshold_mT = 'c')
  
  # 3. Helper Function to pull run data ----
  pull_run_data <- function(run_dir) {
    ind_file = list.files(run_dir, pattern = "eco_indicators_ts.rds", full.names = TRUE, recursive = T)
    # ind_file <- file.path(run_dir, "eco_indicators_ts.rds")
    
    if (!file.exists(ind_file)) return(NULL)
    
    this_run_name <- basename(run_dir)
    this_run_id <- utils::tail(base::strsplit(this_run_name, split = "_")[[1]], 1)
    
    ind_data <- readRDS(ind_file) |>
      dplyr::mutate(run.id = this_run_id)
    
    return(ind_data)
  }
  
  # 4. Process Targeting Data ----
  message("Processing targeting run data...")
  targeting_data_ls <- pbapply::pblapply(targeting_run_dirs, pull_run_data, cl = num_cores) |>
    dplyr::bind_rows()
  
  targeting_data_long <- targeting_data_ls |>
    tidyr::pivot_longer(cols = -c(run.id, year), names_to = "Variable", values_to = "Value") |>
    dplyr::mutate(
      run.id = as.numeric(run.id),
      scenario_name = "targeting",
      scenario_original_name = targeting_run_prefix
    ) |>
    dplyr::left_join(targeting_setup_df, by = "run.id")
  
  # unique(targeting_data_long$Variable)
  targeting.vars = unique(targeting_data_long$Variable)
  targeting.mismatch.names = targeting.vars[which(!(targeting.vars %in% ind.def$Variable))]
  if(length(targeting.mismatch.names) > 0 ){
    message(paste('Warning: Indicators ', paste0(targeting.mismatch.names, collapse = ', '), 'have improper names'))
  }
  
  # 5. Process Uniform Catch Data ----
  message("Processing uniform catch run data...")
  uniform_data_ls <- pbapply::pblapply(uniform_run_dirs, pull_run_data, cl = num_cores) |>
    dplyr::bind_rows()
  
  uniform_data_long <- uniform_data_ls |>
    tidyr::pivot_longer(cols = -c(run.id, year), names_to = "Variable", values_to = "Value") |>
    dplyr::mutate(
      run.id = as.numeric(run.id),
      scenario_name = "uniform",
      scenario_original_name = uniform_run_prefix
    ) |>
    dplyr::left_join(uniform_setup_df, by = "run.id") |>
    dplyr::rename(eof_threshold = catch.threshold) |>
    dplyr::select(-dplyr::any_of("catch.force")) |>
    dplyr::mutate(
      dominant_group = NA,
      dominance_factor = NA
    )
  
  uniform.vars = unique(uniform_data_long$Variable)
  uniform.mismatch.names = uniform.vars[which(!(uniform.vars %in% ind.def$Variable))]
  if(length(uniform.mismatch.names) > 0 ){
    message(paste('Warning: Indicators ', paste0(uniform.mismatch.names, collapse = ', '), 'have improper names'))
  }
  
  # 6. Combine Scenarios ----
  scenario_data <- dplyr::bind_rows(targeting_data_long, uniform_data_long) |>
    dplyr::mutate(run.id = as.character(run.id))
  
  # 7. Process Reference State ----
  message("Processing reference state data...")
  if (!file.exists(ref_state_year_file)) {
    warning("Reference state file not found at: ", ref_state_year_file)
    ref_data <- NULL
  } else {
    ref_data <- readRDS(ref_state_year_file) |>
      tidyr::pivot_longer(cols = -year, names_to = "Variable", values_to = "Value") |>
      dplyr::mutate(
        run.id = "reference",
        scenario_name = "reference",
        scenario_original_name = "reference",
        dominant_group = NA,
        dominance_factor = NA,
        eof_threshold_mT = NA,
        catch.scalar = 1
      )
  }
  
  ref.vars = unique(ref_data$Variable)
  ref.mismatch.names = ref.vars[which(!(ref.vars %in% ind.def$Variable))]
  if(length(ref.mismatch.names) > 0 ){
    message(paste('Warning: Indicators ', paste0(ref.mismatch.names, collapse = ', '), 'have improper names'))
  }
  
  # 8. Calculate Statistics and Scale ----
  message("Calculating statistics and scaling data...")
  all_data <- scenario_data |>
    dplyr::bind_rows(ref_data) |>
    dplyr::group_by(Variable) |>
    dplyr::mutate(
      Value.mean = mean(Value, na.rm = TRUE),
      Value.min = min(Value, na.rm = TRUE),
      Value.max = max(Value, na.rm = TRUE),
      Value.median = stats::median(Value, na.rm = TRUE),
      Value.sd = stats::sd(Value, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Value.scaled = (Value - Value.min) / (Value.max - Value.min)
    )
  
  # uniform_data_long |>
  #   dplyr::summarise(n = dplyr::n(), .by = c(year, run.id, scenario_name, Variable)) |>
  #   dplyr::filter(n > 1L)
  all.vars = unique(all_data$Variable)
  all.mismatch.names = all.vars[which(!(all.vars %in% ind.def$Variable))]
  if(length(all.mismatch.names) > 0 ){
    message(paste('Warning: Indicators ', paste0(all.mismatch.names, collapse = ', '), 'have improper names'))
  }
  
  # 9. Save and Return ----
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  save_path <- file.path(out_dir, "atlantis_EOF_scenario_data.rds")
  saveRDS(all_data, save_path)
  message("Dataset saved to: ", save_path)
  
  return(invisible(all_data))
}
