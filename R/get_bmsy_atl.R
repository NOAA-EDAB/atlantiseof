#' Generates a range of ecological indicators from atlantis output to be used as a summary of possible states
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'
#'@return dataframe of ecological indicators over time
#'
#'@export

get_bmsy_atl = function(param.dir,atl.dir){
  
  bio.df = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'),header=T)%>%
    tidyr::gather(Code,Biomass,-Time)%>%
    dplyr::filter(Code %in% groups$Code)%>%
    dplyr::mutate(year = floor(Time/365))%>%
    dplyr::filter(year %in% timeRange) %>%
    dplyr::group_by(Code,year)%>%
    dplyr::summarise(biomass = mean(Biomass,na.rm=T))
  
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
   stop("Catch file does not exist: ", catch.file)
  }
  
  #get code in catch.df that have nonzero catch
  fished.spp = catch.df %>%
    dplyr::filter(catch > 0) %>%
    dplyr::pull(Code)%>%
    unique()
  
  spp.bmsy = data.frame(Code =fished.spp, bmsy = NA)
  for(i in 1:nrow(spp.bmsy)){
    this.biomass = bio.df %>%
      dplyr::filter(Code == spp.bmsy$Code[i]) %>%
      dplyr::pull(biomass)
    
    this.catch = catch.df %>%
      dplyr::filter(Code == spp.bmsy$Code[i]) %>%
      dplyr::pull(catch)
    
    spp.bmsy$bmsy[i] = atlantiseof::est_bmsy(biomass = this.biomass,catch = this.catch)$B_msy
  }

  return(spp.bmsy)
}