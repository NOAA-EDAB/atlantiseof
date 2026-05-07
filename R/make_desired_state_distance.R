#'Distance from Desired State
#'Calculates the multivariate distance of observations from desired state refd on multiple atlantis runs/observations
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param ref.state.file Character String. Path to ref eco state file
#'@param data.dir Character String. Path to directory with atlantis runs you're looking to compare
#'@param out.dir Character String. Path to directory where you want to save data output
#'@param run.prefix Character String. Prefix for the atlantis runs you're looking to compare
#'@param setup.file Character String. Path to setup file for the atlantis runs you're looking to compare
#'@param cores Integer. Number of cores to use for parallel processing (defaults to system cores - 1)
#'@param debug Logical. If TRUE, runs length-matching diagnostics before calculating distance.
#'
#'@return dataframe of eco indicators by year
#'
#'@importFrom foreach %dopar%
#'@export
#'
make_desired_state_distance = function(param.dir, atl.dir, dietSource, ref.state.file, data.dir, out.dir, run.prefix, setup.file, cores = parallel::detectCores() - 1, debug = FALSE) {
  
  setup.df = utils::read.csv(setup.file)
  
  # Read in reference 
  ref.state = base::readRDS(ref.state.file) |> 
    dplyr::mutate(desired.min.scaled = base::ifelse(!base::is.finite(desired.min.scaled), -Inf, desired.min.scaled),
                  desired.max.scaled = base::ifelse(!base::is.finite(desired.max.scaled), Inf, desired.max.scaled))
  
  # Get run set directory names and IDs
  run.names = base::list.files(data.dir, pattern = "^(.*)_(\\d+)_(\\d+)$", full.names = FALSE, recursive = FALSE, include.dirs = TRUE)
  run.id = base::as.numeric(base::gsub(base::paste0(run.prefix, '_'), '', run.names))
  
  
  # Get link thresholds from reference run
  ref.threshold = atlantiseof::est_link_threshold(param.dir = param.dir,
                                                  atl.dir = atl.dir,
                                                  dietSource = dietSource,
                                                  TL = NA,
                                                  TE = base::c(0.1, 0.15),
                                                  alpha = base::c(0.15, 0.2),
                                                  year = base::c(2000, 2010))
  ref.threshold.range = base::range(ref.threshold$threshold)
  ref.threshold.mean = base::mean(ref.threshold$t)
  
  # --- Setup Parallel Backend ---
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
  # Ensure cluster closes if function errors or completes
  base::on.exit(parallel::stopCluster(cl)) 
  
  # Loop through runs in parallel and calculate distance from ref run
  results <- foreach::foreach(i = 1:base::length(run.names), 
                              .packages = base::c('dplyr', 'tidyr'), 
                              .export = base::c('timeRange')) %dopar% {
                                
                                # read in eco indicators for this run, using make_eco_indicators on each run
                                this.run.ind = base::readRDS(base::paste0(data.dir, run.names[i], '/eco_indicators_ts.rds')) |> 
                                  dplyr::mutate(run.name = run.names[i],
                                                run.id = run.id[i]) |> 
                                  dplyr::left_join(setup.df, by = 'run.id') |> 
                                  dplyr::select(run.name, run.id, dplyr::everything())
                                
                                # Gather into long format BEFORE joining
                                this.run.long = this.run.ind |> 
                                  dplyr::filter(year %in% timeRange) |> 
                                  tidyr::gather('Variable', 'state.value', -year, -dominant_group, -run.id, -run.name, -eof_threshold, -dominance_factor, -catch.scalar)
                                
                                # 1. Find EXACT match intersection between run variables and ref state variables
                                common.vars = base::intersect(base::unique(this.run.long$Variable), ref.state$Variable)
                                
                                if (base::length(common.vars) == 0) {
                                  base::stop(base::paste0("No matching variables found for run ", run.names[i]))
                                }
                                
                                # 2. Prepare Run Data: Filter to common vars, join, scale, and force sorting
                                this.run.ref = this.run.long |> 
                                  dplyr::filter(Variable %in% common.vars) |> 
                                  dplyr::left_join(ref.state, by = "Variable") |> 
                                  dplyr::mutate(state.value.scaled = state.value / mean.value,
                                                state.value.scaled = base::ifelse(!base::is.finite(state.value.scaled), 0, state.value.scaled)) |> 
                                  dplyr::arrange(year, Variable) # CRITICAL: Sorts alphabetically so it matches reference bounds
                                
                                # 3. Prepare Reference Bounds: Filter to common vars and force exact same sorting
                                ref.state.matched = ref.state |> 
                                  dplyr::filter(Variable %in% common.vars) |> 
                                  dplyr::arrange(Variable) # CRITICAL: Matches the alphabetical sort from above
                                
                                cur.desired.lower = ref.state.matched$desired.min.scaled
                                cur.desired.upper = ref.state.matched$desired.max.scaled
                                cur.ind.names     = ref.state.matched$Variable
                                
                                # --- DEBUG BLOCK ---
                                if (debug) {
                                  len_upper = base::length(cur.desired.upper)
                                  len_lower = base::length(cur.desired.lower)
                                  len_ind   = base::length(cur.ind.names)
                                  len_var   = base::length(base::unique(this.run.ref$Variable))
                                  
                                  if (len_upper != len_var || len_lower != len_var || len_ind != len_var) {
                                    base::stop(base::paste0(
                                      "DEBUG FAIL in run ", run.names[i], ":\n",
                                      "  - Unique Variables in run: ", len_var, "\n",
                                      "  - Length of upper bounds: ", len_upper, "\n",
                                      "  - Length of lower bounds: ", len_lower, "\n",
                                      "  - Length of ind.names: ", len_ind
                                    ))
                                  }
                                }
                                # -------------------
                                
                                dist.ref <- base::tapply(
                                  this.run.ref$state.value.scaled,
                                  this.run.ref$year,
                                  # This is the new wrapper function
                                  function(x) {
                                    atlantiseof::make_state_distance_rect(
                                      desired.upper = cur.desired.upper,
                                      desired.lower = cur.desired.lower,
                                      observed.state = x
                                    )$distance # We immediately extract the $distance element here
                                  }
                                )
                                
                                this.run.ind.timeRange = this.run.ind |> dplyr::filter(year %in% timeRange)
                                
                                this.run.distance = base::data.frame(run.name = run.names[i],
                                                                     run.id = run.id[i], 
                                                                     year = this.run.ind.timeRange$year,
                                                                     catch.tot = this.run.ind.timeRange$catch.tot,
                                                                     rel.ref.catch = this.run.ind.timeRange$catch.tot / ref.state.matched$mean.value[base::which(ref.state.matched$Variable == 'catch.tot')],
                                                                     rel.threshold = this.run.ind.timeRange$catch.tot / ref.threshold.mean, 
                                                                     distance.ref = dist.ref, 
                                                                     stringsAsFactors = FALSE) |> 
                                  dplyr::left_join(setup.df, by = "run.id")
                                
                                # calc run statistics
                                # Save closest point
                                closest.ref <- base::tapply(
                                  this.run.ref$state.value.scaled,
                                  this.run.ref$year,
                                  # This is the new wrapper function
                                  function(x) {
                                    atlantiseof::make_state_distance_rect(
                                      desired.upper = cur.desired.upper,
                                      desired.lower = cur.desired.lower,
                                      observed.state = x
                                    )$closest_point # We immediately extract the $closest_point element here
                                  }
                                )
                                
                                closest.val.df <- base::lapply(base::names(closest.ref), function(current_item_name) {
                                  base::data.frame(
                                    year = current_item_name,
                                    variable = cur.ind.names,
                                    closest.ref = closest.ref[[current_item_name]]
                                  )
                                }) |> 
                                  dplyr::bind_rows() |> 
                                  dplyr::mutate(run = run.id[i]) |> 
                                  dplyr::select(run, year, variable, closest.ref)
                                
                                # Return everything as a list for this iteration
                                base::list(
                                  run.ind.ref = this.run.ref,
                                  run.distance = this.run.distance,
                                  run.closest = closest.val.df
                                )
                              }
  
  # --- Unpack Parallel Results ---
  run.ind.ref.ls <- base::lapply(results, `[[`, "run.ind.ref")
  run.distance.ls <- base::lapply(results, `[[`, "run.distance")
  run.closest.ls <- base::lapply(results, `[[`, "run.closest")
  
  # Calculate incremental distance.ref for each catch.scalar
  run.distance.df = dplyr::bind_rows(run.distance.ls) |> 
    dplyr::mutate(catch.scalar.rel = catch.scalar / base::max(catch.scalar, na.rm = TRUE)) |> 
    dplyr::arrange(catch.scalar.rel) |> 
    dplyr::mutate(incremental.distance.ref = distance.ref - dplyr::lag(distance.ref, default = dplyr::first(distance.ref)))
  
  
  # write output
  base::saveRDS(run.distance.df, file = base::paste0(out.dir, run.prefix, '_distance.rds'))
  
  run.ind.ref = dplyr::bind_rows(run.ind.ref.ls) 
  base::saveRDS(run.ind.ref, file = base::paste0(out.dir, run.prefix, '_run_eco_ind.rds'))
  
  run.closest = dplyr::bind_rows(run.closest.ls)
  base::saveRDS(run.closest, file = base::paste0(out.dir, run.prefix, '_run_closest.rds'))
  
  base::saveRDS(ref.threshold, file = base::paste0(out.dir, run.prefix, '_eof_threshold.rds'))
}