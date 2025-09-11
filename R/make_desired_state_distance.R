#'Distance fromd Desired State
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
#'
#'
#'@return dataframe of eco indicators by year
#'
#'@export
#'

make_desired_state_distance = function(param.dir,atl.dir,dietSource,ref.state.file,data.dir,out.dir,run.prefix,setup.file){
  
  #Read in reference 
  ref.state = readRDS(ref.state.file) |> 
    dplyr::mutate(desired.min.scaled = ifelse(!is.finite(desired.min.scaled),-Inf,desired.min.scaled),
                  desired.max.scaled = ifelse(!is.finite(desired.max.scaled),Inf,desired.max.scaled))
  
  ind.names = ref.state$Variable
  
  #pull desired state lower and upper bounds
  desired.lower.ref = ref.state$desired.min.scaled
  desired.upper.ref = ref.state$desired.max.scaled
  
  #Get run set directory names and IDs

  run.names = list.files(data.dir,pattern = "^(.*)_(\\d+)_(\\d+)$",full.names = F, recursive = FALSE, include.dirs = T)
  run.id = gsub(paste0(run.prefix,'_'), '', run.names) |> as.numeric()
  
  
  #Get link thresholds from reference run
  ref.threshold = atlantiseof::est_link_threshold(param.dir = param.dir,
                                                  atl.dir = atl.dir,
                                                  dietSource = dietSource,
                                                  TL = NA,
                                                  TE = c(0.1,0.15),
                                                  alpha = c(0.15,0.2),
                                                  year = c(2000,2010))
  ref.threshold.range = range(ref.threshold$threshold)
  ref.threshold.mean = mean(ref.threshold$t)
  
  #Initialize lists to store results
  run.ind.ref.ls = list()
  run.closest.ls = list()
  run.distance.ls = list()

  #Loop through runs and calculate distance from ref run after scaling data to be on same interval
  for(i in 1:length(run.names)){
    
  #read in eco indicators for this run, using make_eco_indicators on each run
    this.run.ind =  readRDS(paste0(data.dir,run.names[i],'/eco_indicators_ts.rds')) |> 
      dplyr::mutate(run.name = run.names[i],
                    run = run.id[i]) |> 
      dplyr::left_join(setup.file, by = 'run') |> 
      dplyr::select(run.name,run, everything())
    
    #scales data by mean of reference and replaces non-finite values with 0
    this.run.ref = this.run.ind |> 
      dplyr::filter(year %in% timeRange) |> 
      tidyr::gather('Variable','state.value',-year, -run,-run.name,-catch.force, -catch.threshold, -catch.scalar) |> 
      dplyr::left_join(ref.state) |> 
      dplyr::mutate(state.value.scaled = state.value / mean.value,
                    state.value.scaled = ifelse(!is.finite(state.value.scaled),0,state.value.scaled))
    run.ind.ref.ls[[i]] = this.run.ref
    
    #calcuate distance from ref
    
    dist.ref <- tapply(
      this.run.ref$state.value.scaled,
      this.run.ref$year,
      # This is the new wrapper function
      function(x) {
        make_state_distance_rect(
          desired.upper = desired.upper.ref,
          desired.lower = desired.lower.ref,
          observed.state = x
        )$distance # We immediately extract the $distance element here
      }
    )
    
    this.run.ind.timeRange = this.run.ind |> dplyr::filter(year %in% timeRange)
    
    this.run.distance = data.frame(run.name = run.names[i],
                                   run = run.id[i], 
                                   year = this.run.ind.timeRange$year,
                                   catch.tot =  this.run.ind.timeRange$catch.tot,
                                   rel.ref.catch = this.run.ind.timeRange$catch.tot/ref.state$mean.value[which(ref.state$Variable == 'catch.tot')],
                                   rel.threshold = this.run.ind.timeRange$catch.tot/ref.threshold.mean, 
                                   distance.ref = dist.ref, stringsAsFactors = FALSE) |> 
      dplyr::left_join(setup.file)
    
    run.distance.ls[[i]] = this.run.distance
    
    #calc run statistics
    #Save closest point
    closest.ref <- tapply(
      this.run.ref$state.value.scaled,
      this.run.ref$year,
      # This is the new wrapper function
      function(x) {
        make_state_distance_rect(
          desired.upper = desired.upper.ref,
          desired.lower = desired.lower.ref,
          observed.state = x
        )$closest_point # We immediately extract the $distance element here
      }
    )
    
    closest.val.df <- lapply(names(closest.ref), function(current_item_name) {
      data.frame(
        year = current_item_name,
        variable = ind.names,
        closest.ref = closest.ref[[current_item_name]]
      )
    }) |> 
      dplyr::bind_rows() |> 
      dplyr::mutate(run = run.id[i]) |> 
      dplyr::select(run,year,variable,closest.ref)
      
    run.closest.ls[[i]] = closest.val.df
  }
  
  #Calculate incremental distance.ref for each catch.scalar
  run.distance.df = dplyr::bind_rows(run.distance.ls) |> 
    dplyr::mutate(catch.scalar.rel = catch.scalar / max(catch.scalar, na.rm = TRUE)) |> 
    dplyr::arrange(catch.scalar.rel) |> 
    dplyr::mutate(incremental.distance.ref = distance.ref - dplyr::lag(distance.ref, default = dplyr::first(distance.ref)))
  
  
  #write output
  saveRDS(run.distance.df, file = paste0(out.dir,run.prefix,'_distance.rds'))
  run.ind.ref = dplyr::bind_rows(run.ind.ref.ls) 
  saveRDS(run.ind.ref, file = paste0(out.dir,run.prefix,'_run_eco_ind.rds'))
  run.closest = dplyr::bind_rows(run.closest.ls)
  saveRDS(run.closest, file = paste0(out.dir,run.prefix,'_run_closest.rds'))
  saveRDS(ref.threshold, file = paste0(out.dir,run.prefix,'_eof_threshold.rds'))
}
