#' Creates an ecological indicator dataset from atlantis output
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param group.index Character String. Name of the group index with non-atlantis categories
#'@param fgs.file Character String. Name of the functional groups file
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param timeRange Numeric Vector. Range of years to include in the analysis
#'
#'@return list
#'\item{trophiclevel}{dataframe. Species, year, trophic level}
#'\item{figuretl}{ggplot2 object. trophic level bar plot by species}
#'
#'@export



make_eco_indicators = function(param.dir,atl.dir,group.index,timeRange, fgs.file, dietSource){
  
  
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
    dplyr::group_by(Code)%>%
    dplyr::summarise(biomass = mean(Biomass,na.rm=T))
  
  #Total Catch
  catch.file = paste0(atl.dir,'neus_outputCatch.txt')
  if(file.exists(catch.file)){
    catch.df = read.table(catch.file,header =T)%>%
      tidyr::gather(Code,Catch,-Time)%>%
      dplyr::filter(Code %in% groups$Code)%>%
      dplyr::mutate(year = floor(Time/365))%>%
      dplyr::filter(year %in% timeRange) %>%
      dplyr::group_by(Code)%>%
      dplyr::summarise(catch = mean(Catch,na.rm=T))
  }else{
    catch.df = data.frame(Code = groups$Code,
                          catch = 0)
  }
  
  spp.bmsy = atlantiseof::get_bmsy_atl(atl.dir = atl.dir, param.dir =param.dir)
  
  spp.df = bio.df %>%
    dplyr::left_join(stock.ref)%>%
    dplyr::left_join(catch.df)%>%
    dplyr::left_join(spp.bmsy)%>%
    dplyr::mutate(catch.biomass = catch/biomass,
                  overfished = ifelse(biomass>bmsy,0,1))%>%
    dplyr::left_join(groups)
  
  #Total Biomass
  bio.tot = sum(spp.df$biomass,na.rm=T)
  
  #Total Catch
  catch.tot = sum(spp.df$catch,na.rm=T)
  
  #Catch/Biomass
  catch.bio = catch.tot/bio.tot
  
  #Proportion Overfished (NEEDS TO FIT A SURPLUS PRODUCTION MODEL)
  
  
  prop.of = sum(spp.df$overfished,na.rm=T)/length(!is.na(spp.df$overfished))

  #Proportion Biomass Pelagic
  prop.bio.pelagic = sum(spp.df$biomass[spp.df$Pelagic == 1],na.rm=T)/bio.tot
  
  #Proportion Biomass Predator
  prop.bio.predator = sum(spp.df$biomass[spp.df$Predator == 1],na.rm=T)/bio.tot
  
  #Get trophic level
  if(dietSource == 'detdiet'){
    
    new.diet.file = 'neus_outputDetDiet_processed.gz'
    if(!file.exists(paste0(atl.dir,new.diet.file))){
      atlantiseof::process_det_diet(atl.dir = atl.dir,detDietfile =  'neus_outputDetailedDietCheck.txt',outputname =   new.diet.file)  
    }
    tl = atlantiseof::est_trophic_level(param.dir = param.dir, atl.dir = atl.dir, fgs = 'neus_groups.csv', detDietfile = new.diet.file,plottl = F)$trophiclevel
    tl.df = data.frame(Name = names(tl),trophicLevel = tl) 
    rownames(tl.df) = NULL
  }else if(dietSource == 'diet'){
    
  }else if(dietSource == 'param'){
    tl = atlantiseof::est_trophic_level_from_params(fgs = fgs.file,prm_biol = paste0(param.dir,'at_biology.prm'))$trophiclevel
    tl.df = data.frame(Name = names(tl),trophicLevel = tl) 
    rownames(tl.df) = NULL
  }else{
    stop('dietSource must be one of: detdiet, diet, param')
  }
  
  #Mean Trophic level of catch
  mean.tl.catch = sum(spp.df$catch * tl.df$trophicLevel[match(spp.df$Name,tl.df$Name)],na.rm=T) / sum(spp.df$catch,na.rm=T)

  
  #Mean trophic level of biomass
  mean.tl.bio = sum(spp.df$biomass * tl.df$trophicLevel[match(spp.df$Name,tl.df$Name)],na.rm=T) / sum(spp.df$biomass,na.rm=T)
  
  #Return dataframe of all indicators
  
  return(data.frame(
    bio.tot = bio.tot,
    catch.tot = catch.tot,
    catch.bio = catch.bio,
    prop.of = prop.of,
    prop.bio.pelagic = prop.bio.pelagic,
    prop.bio.predator = prop.bio.predator,
    mean.tl.catch = mean.tl.catch,
    mean.tl.bio = mean.tl.bio
  ))
  # return(list(
  #   bio.tot = bio.tot,
  #   catch.tot = catch.tot,
  #   catch.bio = catch.bio,
  #   prop.of = prop.of,
  #   prop.bio.pelagic = prop.bio.pelagic,
  #   prop.bio.predator = prop.bio.predator,
  #   mean.tl.catch = mean.tl.catch,
  #   mean.tl.bio = mean.tl.bio
  # ))
}
