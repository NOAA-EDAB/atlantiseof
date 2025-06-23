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
#'
#'@return dataframe of ecological indicators over time
#'
#'@export


make_eco_indicators_time = function(param.dir,atl.dir,group.index,fgs.file,dietSource,timeRange,start.year = 1964){
  
  
  #Load the groups.csv file
  stock.ref = atlantiseof::get_bmsy_ss(fgs = fgs.file, default.bmsy.frac = 0.4) %>%
    dplyr::group_by(Code) %>%
    summarise(bmsy = mean(Bmsy,na.rm=T))
  groups = read.csv(group.index,stringsAsFactors = F) %>%
    left_join(stock.ref, by = 'Code')
  
  #Total Biomass
  bio.df = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'),header=T)%>%
    tidyr::gather(Code,Biomass,-Time)%>%
    dplyr::filter(Code %in% groups$Code)%>%
    dplyr::mutate(year = floor(Time/365))%>%
    dplyr::filter(year %in% timeRange) %>%
    dplyr::group_by(Code,year)%>%
    dplyr::summarise(biomass = mean(Biomass,na.rm=T))
  
  #Total Catch
  catch.file = paste0(atl.dir,'neus_outputCatch.txt')
  if(file.exists(catch.file)){
    catch.df = read.table(catch.file,header =T)%>%
      tidyr::gather(Code,Catch,-Time)%>%
      dplyr::filter(Code %in% groups$Code)%>%
      dplyr::mutate(year = floor(Time/365))%>%
      dplyr::filter(year %in% timeRange) %>%
      dplyr::group_by(Code,year)%>%
      dplyr::summarise(catch = mean(Catch,na.rm=T))
  }else{
    catch.df = expand.grid(year = timeRange, Code = groups$Code) %>%
      dplyr::mutate(catch = 0)
  }
  
  #species x year dataframe
  spp.df = bio.df %>%
    dplyr::left_join(stock.ref)%>%
    dplyr::left_join(catch.df)%>%
    dplyr::mutate(catch.biomass = catch/biomass,
                  overfished = ifelse(biomass>bmsy,0,1))%>%
    dplyr::left_join(groups)
    
  #Biomass and catch metrics
  eco.df = spp.df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      bio.tot = sum(biomass,na.rm=T),
      catch.tot = sum(catch,na.rm=T),
      prop.of = sum(overfished,na.rm=T)/length(!is.na(overfished)),
      pelagic.bio = sum(biomass[Pelagic == 1], na.rm = TRUE),
      predator.bio = sum(biomass[Predator == 1], na.rm = TRUE)
      )%>%
    dplyr::mutate(
      catch.bio = catch.tot/bio.tot,
      prop.bio.pelagic = pelagic.bio/bio.tot,
      prop.bio.predator = predator.bio/bio.tot
    )%>%
    dplyr::select(-c(pelagic.bio,predator.bio))
  
  #Get trophic level
  if(dietSource == 'detdiet'){
    
    new.diet.file = 'neus_outputDetDiet_processed.gz'
    if(!file.exists(paste0(atl.dir,new.diet.file))){
      atlantiseof::process_det_diet(atl.dir = atl.dir,detDietfile =  'neus_outputDetailedDietCheck.txt',outputname =   new.diet.file)  
    }
    tl.df = atlantiseof::est_trophic_level_time(param.dir = param.dir, atl.dir = atl.dir, fgs = 'neus_groups.csv', detDietfile = new.diet.file,plottl = F)$trophiclevel
    tl.df = tl.df %>%
      dplyr::left_join(groups, by = c('species' = 'Name'))%>%
      dplyr::select(year,Code,TL) %>%
      dplyr::mutate(year = year - start.year)%>% 
      dplyr::rename('trophicLevel' = 'TL')%>%
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
  mean.tl = spp.df %>%
    left_join(tl.df)%>%
    dplyr::group_by(year) %>%
    dplyr::summarise(mean.tl.catch = sum(catch * trophicLevel,na.rm=T) / sum(catch,na.rm=T),
                     mean.tl.bio = sum(biomass * trophicLevel,na.rm=T) / sum(biomass,na.rm=T))
  
  
  eco.df = dplyr::left_join(eco.df,mean.tl)
  
  #Return list of all indicators
  return(eco.df)
}
