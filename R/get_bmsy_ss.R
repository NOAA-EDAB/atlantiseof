#' Function to get stockSMART Bmsy for species and supply a default if not found
#' 
#' @param fgs Character String. Path to the functional groups file
#' @param default.bmsy.frac Numeric. Default fraction of biomass to use if Bmsy is not found in stockSMART
#' 
#' @return data.frame. Species, Bmsy
#' 
#' @export

get_bmsy_ss = function(fgs,default.bmsy.frac) {
  # Load the functional groups file
  fgs_data <- read.csv(fgs, stringsAsFactors = FALSE)
  
  #read data from url
  atl.index = read.csv(url('https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/master/data/functionalGroupNames.csv')) %>%
    dplyr::select(-NESPP3) %>% 
    dplyr::distinct()
  
  
  ss_data = atl.index %>%
    dplyr::inner_join(stocksmart::stockAssessmentSummary,by=c("Species_Itis"="ITIS Taxon Serial Number"),na_matches = "never") %>% 
    dplyr::filter(`Regional Ecosystem` %in% c("Northeast Shelf","Atlantic Highly Migratory","Northeast Shelf / Southeast Shelf"))%>%
    dplyr::select(Code,Species,`Assessment Year`,`Estimated B`,Bmsy,Fmsy)
    
  ss_bio = ss_data %>%
    dplyr::group_by(Code)%>%
    dplyr::summarise(Biomass = mean(`Estimated B`,na.rm=T))
  
  ss_bmsy = ss_data %>%
    dplyr::group_by(Code,Species)%>%
    dplyr::filter(`Assessment Year` == max(`Assessment Year`,na.rm=T))%>%
    dplyr::left_join(ss_bio)%>%
    dplyr::mutate(missing_bmsy = is.na(Bmsy),
                  Bmsy = ifelse(is.na(Bmsy), Biomass * 0.4, Bmsy))
  
  return(ss_bmsy)
}


