#' Retreives diet proportions from parameter files
#'
#' Gets diet information from pPREY values in biology.prm
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param fgs Character String. Name of groups.csv file
#'@param detDietfile Character String. Name of processed zipped detailed diet file
#'@param dietSource string. Source of diet matrix, either 'realized' or 'prm'. Default is 'prm'.
#'@param plottl Boolean. Plot the Trophic level to window. Default = F
#'
#'@return dataframe 
#'\item{realized diet}{data.frame time,pred,prey,consumption, prop.consumption}
#'\item{prm diet}{pred,prey,consumption, prop.consumption}
#'
#'@export

get_diet_prop = function(param.dir,atl.dir,fgs,detDietfile = F,plottl = F, dietSource){
  
  if(detDietfile == T){
    stop("Detailed diet file not yet implemented in get_diet_prop function")
  }
  
  speciesnames <- atlantistools::load_fgs(fgs) %>%
    dplyr::select(Code,Name)
  
  if(dietSource == 'prm'){
    pprey <- atlantistools::load_dietmatrix(prm_biol = paste0(param.dir,'at_biology.prm'),
                                            fgs = fgs) %>%
      dplyr::mutate(prey = dplyr::case_when(grepl("sed",prey) ~ substr(prey,1,2),
                                            TRUE ~ prey)) %>%
      dplyr::left_join(.,speciesnames,by = c("pred"="Code")) %>%
      dplyr::rename(predName = "Name") %>%
      dplyr::left_join(.,speciesnames,by = c("prey"="Code")) %>%
      dplyr::rename(preyName = "Name") %>%
      dplyr::select(-pred,-prey) %>%
      dplyr::rename(pred = "predName",prey = "preyName") %>%
      
      tibble::as_tibble()
  
    # sums up pprey values over pred,prey stanzas
    consump <- pprey %>%
      dplyr::group_by(pred,pred_stanza,prey) %>%
      dplyr::summarise(consumption = sum(avail),
                       .groups = "drop")  
    
    # total pprey values for predator
    predtotalconsump <- consump %>%
      dplyr::group_by(pred,pred_stanza) %>%
      dplyr::summarise(totconsumption = sum(consumption),
                       .groups = "drop")
    
    # Get proportional consumption
    propconsump <- consump %>%
      dplyr::left_join(.,predtotalconsump,by = c("pred",'pred_stanza')) %>%
      dplyr::mutate(prop.consumption = consumption/totconsumption) %>%
      dplyr::select(pred,pred_stanza,prey,consumption,prop.consumption)
    
    
  }else{
    
    consump = atlantistools::load_dietcheck(paste0(atl.dir,'neus_outputDietCheck.txt'),
                                         prm_run= paste0(param.dir,'at_run.prm'),
                                         fgs = fgs) %>%
      # dplyr::group_by(time,pred,prey)%>%
      # summarise(consumption = sum(atoutput), .groups = 'drop') %>%
      dplyr::rename(consumption = 'atoutput')%>%
      dplyr::left_join(.,speciesnames,by = c("pred"="Code")) %>%
      dplyr::select(-pred)%>%
      dplyr::rename(pred = "Name") %>%
      dplyr::left_join(.,speciesnames,by = c("prey"="Code")) %>%
      dplyr::select(-prey) %>%
      dplyr::rename(prey = "Name")
    
    predtotalconsump = consump %>%
      dplyr::group_by(time,pred,agecl) %>%
      dplyr::summarise(totconsumption = sum(consumption),
                       .groups = "drop")
    
    propconsump <- consump %>%
      dplyr::left_join(.,predtotalconsump,by = c("pred",'agecl','time')) %>%
      dplyr::mutate(prop.consumption = consumption/totconsumption) %>%
      dplyr::select(time,pred,agecl,prey,consumption,prop.consumption)
  }
 
  return(propconsump)
  
}