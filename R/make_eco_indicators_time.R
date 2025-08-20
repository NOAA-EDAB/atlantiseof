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
#'@param cloud Logical. If TRUE, run on cloud Default is FALSE.
#'@param ref.run.dir Character String. Path to reference run directory. If NULL, no divergence metrics are calculated.
#'
#'@return dataframe of ecological indicators over time
#'
#'@export


make_eco_indicators_time = function(param.dir,atl.dir,group.index,fgs.file,dietSource,timeRange,start.year = 1964,cloud = F, ref.run.dir = NULL){
  
  #Load the groups.csv file
  stock.ref = atlantiseof::get_bmsy_ss(fgs = fgs.file, default.bmsy.frac = 0.4) |>
    dplyr::group_by(Code) |>
    dplyr::summarise(bmsy = mean(Bmsy,na.rm=T))
  groups = read.csv(group.index,stringsAsFactors = F) |>
    dplyr::left_join(stock.ref, by = 'Code')
  
  #Total Biomass
  bio.df = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'),header=T, fill = T)|>
    tidyr::gather(Code,Biomass,-Time)|>
    dplyr::filter(Code %in% groups$Code)|>
    dplyr::mutate(year = floor(Time/365))|>
    dplyr::filter(year %in% timeRange) |>
    dplyr::group_by(Code,year)|>
    dplyr::summarise(biomass = mean(Biomass,na.rm=T))
  
  #Total Catch
  catch.file = paste0(atl.dir,'neus_outputCatch.txt')
  if(file.exists(catch.file)){
    catch.df = read.table(catch.file,header =T,fill =T)|>
      tidyr::gather(Code,Catch,-Time)|>
      dplyr::filter(Code %in% groups$Code)|>
      dplyr::mutate(year = floor(Time/365))|>
      dplyr::filter(year %in% timeRange) |>
      dplyr::group_by(Code,year)|>
      dplyr::summarise(catch = mean(Catch,na.rm=T))
  }else{
    catch.df = expand.grid(year = timeRange, Code = groups$Code) |>
      dplyr::mutate(catch = 0)
  }
  
  #species x year dataframe
  spp.df = bio.df |>
    dplyr::left_join(stock.ref)|>
    dplyr::left_join(catch.df)|>
    dplyr::mutate(catch.biomass = catch/biomass)|>
    dplyr::left_join(groups)|>
    dplyr::select(-bmsy)
  
  #Get all species with non-zero catch
  is.fished = spp.df |>
    dplyr::filter(!is.na(catch) & (catch > 0 | biomass > 0))
  spp.bmsy = data.frame(Code = unique(is.fished$Code), bmsy = NA)
  for(i in 1:nrow(spp.bmsy)){
    this.spp = spp.df |> dplyr::filter(Code == spp.bmsy$Code[i])
    spp.bmsy$bmsy[i] = atlantiseof::est_bmsy(biomass = this.spp$biomass,catch = this.spp$catch)$B_msy
  }
    
  #Add overfished status
  spp.df = spp.df |>
    dplyr::left_join(spp.bmsy)|>
    dplyr::mutate(overfished = ifelse(biomass>bmsy,0,1))
  
  #Biomass and catch metrics
  eco.df = spp.df |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      bio.tot = sum(biomass,na.rm=T),
      catch.tot = sum(catch,na.rm=T),
      prop.of = sum(overfished,na.rm=T)/length(!is.na(overfished)),
      pelagic.bio = sum(biomass[Pelagic == 1], na.rm = TRUE),
      predator.bio = sum(biomass[Predator == 1], na.rm = TRUE)
      )|>
    dplyr::mutate(
      catch.bio = catch.tot/bio.tot,
      prop.bio.pelagic = pelagic.bio/bio.tot,
      prop.bio.predator = predator.bio/bio.tot
    )|>
    dplyr::select(-c(pelagic.bio,predator.bio))
  
  #Get trophic level
  if(dietSource == 'detdiet'){
    
    new.diet.file = 'neus_outputDetDiet_processed.gz'
    if(!file.exists(paste0(atl.dir,new.diet.file))){
      atlantiseof::process_det_diet(atl.dir = atl.dir,detDietfile =  'neus_outputDetailedDietCheck.txt',outputname =   new.diet.file, cloud = cloud)  
    }
    tl.df.orig = atlantiseof::est_trophic_level_time(param.dir = param.dir, atl.dir = atl.dir, fgs = 'neus_groups.csv', detDietfile = new.diet.file,plottl = F,timeRange = NULL)$trophiclevel
    tl.df = tl.df.orig |>
      dplyr::left_join(groups, by = c('species' = 'Name','Code'))|>
      dplyr::select(year,Code,TL) |>
      dplyr::mutate(year = year - start.year)|> 
      dplyr::rename('trophicLevel' = 'TL')|>
      dplyr::filter(year %in% timeRange)
    
    
    
  }else if(dietSource == 'diet'){
    
  }else if(dietSource == 'param'){
    tl.df = atlantiseof::est_trophic_level_from_params(fgs = fgs.file,prm_biol = paste0(param.dir,'at_biology.prm'))$trophiclevel
    tl.df = data.frame(Name = names(tl),trophicLevel = tl) 
    rownames(tl.df) = NULL
    
  }else{
    stop('dietSource must be one of: detdiet, diet, param')
  }
  
  #Mean Trophic level of catch by year
  mean.tl = spp.df |>
    dplyr::left_join(tl.df)|>
    dplyr::group_by(year) |>
    dplyr::summarise(mean.tl.catch = sum(catch * trophicLevel,na.rm=T) / sum(catch,na.rm=T),
                     mean.tl.bio = sum(biomass * trophicLevel,na.rm=T) / sum(biomass,na.rm=T))
  
  
  #Calculate cumulative biomass indices
  cum.bio = atlantiseof::calc_cum_bio(bio.df = bio.df, 
                            tl.df = tl.df, 
                            show.plot = F)
  
  #Calculate fish productivity
  fish.prop = atlantiseof::calc_fish_prod(param.dir = param.dir,
                                          atl.dir =atl.dir,
                                          show.plot = FALSE,
                                          survdat = readRDS(here::here('data-raw','survey_lenagewgt.rds')),
                                          timeRange =timeRange)
  
  #Calculate divergence metrics
  if(is.null(ref.run.dir)){
    ref.prop = data.frame(Code = bio.df$Code, biomass.prop =  rep(1/length(unique(bio.df$Code)), length(unique(bio.df$Code))))
  }else{
    ref.prop = atlantiseof::calc_bio_prop(ref.run.dir,fgs.file = fgs.file, timeRange = 30:80)
  }
  diverge = atlantiseof::calc_divergence(bio.df = bio.df, ref.prop = ref.prop, show.plot =F)  |> 
    dplyr::mutate(year = floor(Time/365)) |> 
    dplyr::filter(year %in% timeRange) |>
    dplyr::group_by(year) |> 
    dplyr::summarise(
      KL_divergence = mean(KL_divergence, na.rm = TRUE),
      JS_divergence = mean(JS_divergence, na.rm = TRUE),
      Jeffreys_divergence = mean(Jeffreys_divergence, na.rm = TRUE)
    )
  
  #Calculate foodweb metrics
  foodweb = atlantiseof::calc_foodweb(atl.dir = atl.dir,
                                      dietSource = dietSource,
                                      show.plot = F,
                                      out.dir = atl.dir)
  foodweb = foodweb$metric_ts |> 
    dplyr::mutate(year = floor(time/365)) |> 
    dplyr::filter(year %in% timeRange) |>
    dplyr::select(-time) |> 
    dplyr::group_by(year) |> 
    dplyr::summarise(dplyr::across(dplyr::everything(), mean,.names = "{.col}",na.rm=T))
    
    
    
  
  eco.df = eco.df |> 
    dplyr::left_join(mean.tl) |> 
    dplyr::left_join(cum.bio) |> 
    dplyr::left_join(fish.prop, by = c('year' = 'year.ref')) |> 
    dplyr::left_join(diverge) |>
    dplyr::left_join(foodweb)
  
  
  #Return list of all indicators
  return(eco.df)
}
