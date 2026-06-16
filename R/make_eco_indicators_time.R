#' Generates a range of ecological indicators from atlantis output to be used as a summary of possible states
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param group.index Character String. Name of the group index with non-atlantis categories
#'@param fgs.file Character String. Name of the functional groups file
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param timeRange Numeric vector. Range of years to summarize
#'@param start.year Numeric. Year to start the time series. Default = 1964
#'@param survdat.data dataframe of survdat length,age,weight data
#'@param cloud Logical. If TRUE, run on cloud Default is FALSE.
#'@param ref.run.dir Character String. Path to reference run directory. If NULL, no divergence metrics are calculated.
#'@param debug Logical. If TRUE, prints progress messages. Default is FALSE.
#'
#'@return dataframe of ecological indicators over time
#'
#'@export

make_eco_indicators_time = function(param.dir, atl.dir, group.index, fgs.file, dietSource, timeRange, start.year = 1964, cloud = F, ref.run.dir = NULL, survdat.data = NA, debug = FALSE){
  
  if(debug) message("DEBUG: Starting make_eco_indicators_time function.")
  
  # Load the groups.csv file
  stock.ref = atlantiseof::get_bmsy_ss(fgs = fgs.file, default.bmsy.frac = 0.4) |>
    dplyr::group_by(Code) |>
    dplyr::summarise(bmsy = mean(Bmsy, na.rm = T))
  groups = read.csv(group.index, stringsAsFactors = F) |>
    dplyr::left_join(stock.ref, by = 'Code')
  
  # Total Biomass
  bio.file <- paste0(atl.dir, 'neus_outputBiomIndx.txt')
  if(!file.exists(bio.file)){
    stop(paste("Biomass file not found:", bio.file))
  }
  bio.df = read.table(bio.file, header = T, fill = T)|>
    tidyr::gather(Code, Biomass, -Time)|>
    dplyr::filter(Code %in% groups$Code)|>
    dplyr::mutate(year = floor(Time/365))|>
    dplyr::filter(year %in% timeRange) |>
    dplyr::group_by(Code, year)|>
    dplyr::summarise(biomass = mean(Biomass, na.rm = T))
  
  # Total Catch
  catch.file = paste0(atl.dir, 'neus_outputCatch.txt')
  if(file.exists(catch.file)){
    catch.df = read.table(catch.file, header = T, fill = T)|>
      tidyr::gather(Code, Catch, -Time)|>
      dplyr::filter(Code %in% groups$Code)|>
      dplyr::mutate(year = floor(Time/365))|>
      dplyr::filter(year %in% timeRange) |>
      dplyr::group_by(Code, year)|>
      dplyr::summarise(catch = mean(Catch, na.rm = T))
  } else {
    warning("Catch file not found. Catch-based metrics will be 0 or NA.")
    catch.df = expand.grid(year = timeRange, Code = groups$Code) |>
      dplyr::mutate(catch = 0)
  }
  
  if(debug) message("DEBUG: Biomass and Catch data loaded.")
  
  # species x year dataframe
  spp.df = bio.df |>
    dplyr::left_join(catch.df, by = c("Code", "year")) |>
    dplyr::mutate(catch.biomass = catch / biomass)|>
    dplyr::left_join(groups, by = "Code")
  # dplyr::select(-bmsy.y, -bmsy.x) # Remove bmsy columns to avoid confusion
  
  # Get all species with non-zero catch for BMSY estimation
  is.fished = spp.df |>
    dplyr::filter(!is.na(catch) & (catch > 0 | biomass > 0))
  spp.bmsy = data.frame(Code = unique(is.fished$Code), bmsy = NA)
  
  # Robustly estimate BMSY for each stock
  for(i in 1:nrow(spp.bmsy)){
    this.spp = spp.df |> dplyr::filter(Code == spp.bmsy$Code[i])
    bmsy_val <- try(atlantiseof::est_bmsy(biomass = this.spp$biomass, catch = this.spp$catch)$B_msy, silent = TRUE)
    
    if (inherits(bmsy_val, "try-error")) {
      warning(paste("BMSY estimation failed for Code:", spp.bmsy$Code[i], ". Setting BMSY to NA."))
      spp.bmsy$bmsy[i] <- NA
    } else {
      spp.bmsy$bmsy[i] <- bmsy_val
    }
  }
  
  if(debug) message("DEBUG: BMSY estimation complete.")
  
  # Add overfished status
  spp.df = spp.df |>
    dplyr::left_join(spp.bmsy, by = "Code") |>
    dplyr::mutate(bmsy = dplyr::coalesce(bmsy.x,bmsy.y),
                  overfished = ifelse(biomass > bmsy, 0, 1)) |>
    dplyr::select(-bmsy.x,-bmsy.y)
  
  # Biomass and catch metrics
  eco.df = spp.df |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      bio.tot = sum(biomass, na.rm = T),
      catch.tot = sum(catch, na.rm = T),
      prop.of = sum(overfished, na.rm = T) / sum(!is.na(overfished)),
      pelagic.bio = sum(biomass[Pelagic == 1], na.rm = TRUE),
      predator.bio = sum(biomass[Predator == 1], na.rm = TRUE)
    )|>
    dplyr::mutate(
      catch.bio = catch.tot / bio.tot,
      prop.bio.pelagic = pelagic.bio / bio.tot,
      prop.bio.predator = predator.bio / bio.tot
    )|>
    dplyr::select(-c(pelagic.bio, predator.bio))
  
  if(debug){
    message("DEBUG: Base ecological indicators (biomass, catch, overfished proportion) calculated.")
    print(head(eco.df))
  }
  
  # --- Robust Calculation Blocks ---
  
  # Calculate Trophic Level (generates tl.df)
  tl.df <- tryCatch({
    if(dietSource == 'detdiet'){
      new.diet.file = 'neus_outputDetDiet_processed.gz'
      if(!file.exists(paste0(atl.dir, new.diet.file))){
        atlantiseof::process_det_diet(atl.dir = atl.dir, detDietfile = 'neus_outputDetailedDietCheck.txt', outputname = new.diet.file, cloud = cloud)
      }
      atlantiseof::est_trophic_level_time(param.dir = param.dir, atl.dir = atl.dir, fgs = 'neus_groups.csv', detDietfile = new.diet.file, plottl = F, timeRange = NULL)$trophiclevel |>
        dplyr::left_join(groups, by = c('species' = 'Name', 'Code'))|>
        dplyr::select(year, Code, TL) |>
        dplyr::mutate(year = year - start.year)|>
        dplyr::rename('trophicLevel' = 'TL')|>
        dplyr::filter(year %in% timeRange)
    } else if(dietSource == 'diet'){
      warning("'diet' dietSource is not yet implemented. Trophic level metrics will be NA.")
      expand.grid(year = timeRange, Code = groups$Code) |> dplyr::mutate(trophicLevel = NA)
    } else if(dietSource == 'param'){
      tl_param <- atlantiseof::est_trophic_level_from_params(fgs = fgs.file, prm_biol = paste0(param.dir, 'at_biology.prm'))$trophiclevel
      tl_param_df <- data.frame(Name = names(tl_param), trophicLevel = tl_param)
      groups |>
        dplyr::select(Name, Code) |>
        dplyr::left_join(tl_param_df, by = "Name") |>
        tidyr::crossing(year = timeRange) |> # Expand for all years in range
        dplyr::select(year, Code, trophicLevel)
    } else {
      stop('dietSource must be one of: detdiet, diet, param')
    }
  }, error = function(e) {
    warning("Failed to calculate trophic levels. Subsequent TL-based metrics will be NA. Error: ", e$message)
    expand.grid(year = timeRange, Code = groups$Code) |> dplyr::mutate(trophicLevel = NA)
  })
  
  if(debug){
    message("DEBUG: Trophic level calculation complete.")
    print(head(tl.df))
  }
  

  # Calculate Mean Trophic level of catch by year
  mean.tl <- tryCatch({
    spp.df |>
      dplyr::left_join(tl.df, by = c("Code", "year"))|>
      dplyr::group_by(year) |>
      dplyr::summarise(
        mean.tl.catch = sum(catch * trophicLevel, na.rm = T) / sum(catch, na.rm = T),
        mean.tl.bio = sum(biomass * trophicLevel, na.rm = T) / sum(biomass, na.rm = T)
      )
  }, error = function(e) {
    warning("Failed to calculate mean trophic level. Returning NAs. Error: ", e$message)
    data.frame(year = timeRange, mean.tl.catch = NA, mean.tl.bio = NA)
  })
  
  if(debug){
    message("DEBUG: Mean trophic level of catch and biomass calculated.")
    print(head(mean.tl))
  }
  
  # Calculate biomass of consumers
  consumer.spp = tl.df |> 
    dplyr::group_by(Code) |> 
    dplyr::summarise(trophicLevel = mean(trophicLevel)) |> 
    dplyr::filter(trophicLevel >= 2) |> 
    dplyr::pull(Code)
  
  bio.consumer = spp.df |> 
    dplyr::left_join(tl.df) |> 
    dplyr::filter(Code %in% consumer.spp) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(bio.consumer = sum(biomass,na.rm=T))
  
  eco.df = eco.df |> 
    dplyr::left_join(bio.consumer)
  
  # Get spatial Gini index
  bio.spatial.spp = calc_spatial_biomass(atl.dir, param.dir, fgs.file, aggregate_total = F, keep_groups = consumer.spp) |> 
    dplyr::group_by(time) |> 
    dplyr::summarise(biomass.spatial.gini.mean = mean(biomass.spatial.gini,na.rm=T)) |> 
    dplyr::rename(year = 'time')
    
  bio.spatial.tot = calc_spatial_biomass(atl.dir, param.dir, fgs.file, aggregate_total = T, keep_groups = consumer.spp) |> 
    dplyr::rename(biomass.spatial.gini.tot = 'biomass.spatial.gini',
                  year = 'time') |> 
    dplyr::select(year, biomass.spatial.gini.tot)
  
  eco.df = eco.df |>
    dplyr::left_join(bio.spatial.spp) |> 
    dplyr::left_join(bio.spatial.tot)
  
  # Calculate cumulative biomass indices
  cum.bio <- tryCatch({
    atlantiseof::calc_cum_bio(bio.df = bio.df, tl.df = tl.df, show.plot = F)
  }, error = function(e) {
    warning("Failed to calculate cumulative biomass. Returning NAs. Error: ", e$message)
    data.frame(year = timeRange, steep = NA, bio_inf = NA, tl_inf = NA, b1_param = NA, b2_param = NA, c_param = NA, d_param  = NA, e_param = NA) # Empty df with year for joining
  })
  
  if(debug){
    message("DEBUG: Cumulative biomass indices calculated.")
    print(head(cum.bio))
  }
  
  # Calculate fish productivity
  fish.prop <- tryCatch({
    atlantiseof::calc_fish_prod(param.dir = param.dir, atl.dir = atl.dir,
                                show.plot = FALSE, survdat = survdat.data,
                                timeRange = timeRange)
  }, error = function(e) {
    warning("Failed to calculate fish productivity. Returning NAs. Error: ", e$message)
    data.frame(year.ref = timeRange,small.large.ratio.anom.mean  = NA) # Empty df with join key
  })
  
  if(debug){
    message("DEBUG: Fish productivity calculated.")
    print(head(fish.prop))
  }
  
  # Calculate divergence metrics
  diverge <- tryCatch({
    if(is.null(ref.run.dir)){
      ref.prop = data.frame(Code = unique(bio.df$Code), biomass.prop = rep(1/length(unique(bio.df$Code)), length(unique(bio.df$Code))))
    } else {
      ref.prop = atlantiseof::calc_bio_prop(ref.run.dir, fgs.file = fgs.file, timeRange = 30:80)
    }
    atlantiseof::calc_divergence(bio.df = bio.df,
                                 ref.prop = ref.prop,
                                 show.plot = F,
                                 fgs.file = fgs.file,
                                 atl.dir = atl.dir) |>
      dplyr::mutate(year = floor(Time/365)) |>
      dplyr::filter(year %in% timeRange) |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        KL_divergence = mean(KL_divergence, na.rm = TRUE),
        JS_divergence = mean(JS_divergence, na.rm = TRUE),
        Jeffreys_divergence = mean(Jeffreys_divergence, na.rm = TRUE)
      )
  }, error = function(e) {
    warning("Failed to calculate divergence metrics. Returning NAs. Error: ", e$message)
    data.frame(year = timeRange, KL_divergence = NA, JS_divergence = NA, Jeffreys_divergence = NA)
  })
  
  if(debug){
    message("DEBUG: Divergence metrics calculated.")
    print(head(diverge))
  }
  
  # Calculate foodweb metrics
  foodweb.df <- tryCatch({
    foodweb = atlantiseof::calc_foodweb(atl.dir = atl.dir,
                                        param.dir = param.dir,
                                        dietSource = dietSource,
                                        fgs.file = fgs.file,
                                        timeRange = timeRange,
                                        show.plot = F)
    foodweb$metric_ts |>
      dplyr::mutate(year = floor(time/365)) |>
      dplyr::filter(year %in% timeRange) |>
      dplyr::select(-time) |>
      dplyr::group_by(year) |>
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), \(x) mean(x,na.rm=T), .names = "{.col}"))
  }, error = function(e) {
    warning("Failed to calculate foodweb metrics. Returning NAs. Error: ", e$message)
    data.frame(year = timeRange,
               connectance = NA,
               mean_in_degree = NA,
               mean_out_degree= NA,
               mean_betweenness = NA,
               network_betweenness_centralization =NA,
               modularity_directed = NA,
               modularity_undirected = NA,
               redundancy_proxy = NA,
               avg_jaccard_similarity = NA,
               ascendancy = NA,
               capacity = NA,
               coherence = NA,
               overhead = NA,
               rel_ascendancy = NA,
               resilience_eigenvalue = NA,
               reactive_ascendancy = NA) # Empty df with year for joining
  })
  
  if(debug){
    message("DEBUG: Foodweb metrics calculated.")
    print(head(foodweb.df))
  }
  
  # --- Final Assembly ---
  
  # Join all indicator dataframes together
  eco.df = eco.df |>
    dplyr::left_join(mean.tl, by = "year") |>
    dplyr::left_join(cum.bio, by = "year") |>
    dplyr::left_join(fish.prop, by = c('year' = 'year.ref')) |>
    dplyr::left_join(diverge, by = "year") |>
    dplyr::left_join(foodweb.df, by = "year")
  
  if(debug){
    message("DEBUG: All indicator dataframes have been joined.")
    print(head(eco.df))
    message("DEBUG: Final dimensions of eco.df: ", paste(dim(eco.df), collapse = " x "))
    message("DEBUG: Function finished. Returning final dataframe.")
  }
  
  # Return list of all indicators
  return(eco.df)
}
  