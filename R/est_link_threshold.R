#' Read in the Phyto_Forcing_xxx.nc files to get estimates of PP
#'
#' Get primary production data from nc forcing files
#' NEUS atlantis is forced with 3 primary producer species (Diatoms, Dinoflagellates,Picophytoplankton)
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param year Numeric Vector. Year to use for estimating thresholds
#'@param TE Numeric Vector Trophic efficiency to use for estimating  thresholds, generally 0.1 - 0.16
#'@param alpha Numeric Vector. regional scalar for threshold (generally 0.15 - 0.2)
#'@param TL Numeric Vector. Trophic level to use for estimating thresholds, generally 2.5 - 3.5, if NA will use the mean trophic level of fished species
#'@param pp.type Character String. Whether to use "biomass" or "production" from the nc files. If "production", will use the merged annual PPD estimates from VGPM2 with chlorophyll a data from A-CCI. Note that these are not the same as the production estimates from the nc files, which are based on the Atlantis model state and may be more variable.
#'@param ppd.files Character String. Path to directory containing the merged annual PPD estimates from VGPM2 with chlorophyll a data from A-CCI. Only used if pp.type = "production"
#'
#' @return dataframe
#'
#'@export



est_link_threshold = function(atl.dir, param.dir, dietSource = NA, year, TE, alpha, TL, pp.type, ppd.files = NA){
  
  fgs = read.csv(paste0(param.dir, 'neus_groups.csv'), stringsAsFactors = FALSE)
  fished.spp = fgs$Name[which(fgs$isFished ==1)]
  
  bgm.file = file.path(param.dir, "neus_tmerc_RM2.bgm")
  neus.shp = NEFSCspatial::Neus_atlantis |> sf::st_as_sf() |> 
    dplyr::arrange(BOX_ID)
  active.box = neus.shp$BOX_ID[which(neus.shp$boundary== 0)]
  
  if(pp.type == 'biomass'){
    ppdata <- atlantiseof::get_pp(bgm = bgm.file,pathToForcing = paste0(param.dir,'tsfiles/Annual_Files/'))  
    ppdata = ppdata$dailyspeciesbox
    
    pp.neus <- ppdata |>
      dplyr::left_join(neus.shp,by = c("box"="BOX_ID")) |>
      dplyr::filter(boundary == 0) |> #remove boundary boxes
      dplyr::group_by(year,variable) |>
      dplyr::summarise(N = sum(value,na.rm=T),.groups="drop") |>
      dplyr::mutate(C = 5.7*N) #20 wet:dry from atlantis
    
  }else if(pp.type == 'production'){
    # ppd.file = here::here('data-raw','data','MERGED_ANNUAL_SUM-NES_EPU_STATISTICAL_AREAS_NOEST-PPD-VGPM2_CHLOR_A-CCI-STATS-V2024.CSV')
    ppdata = atlantiseof::get_ppd(ppd.files = ppd.files)
    ppdata = ppdata$dailyboxmT
    
    pp.neus <- ppdata |>
      dplyr::left_join(neus.shp,by = c("box"="BOX_ID")) |>
      dplyr::filter(boundary == 0) |> #remove boundary boxes
      dplyr::group_by(year,variable) |>
      dplyr::summarise(C = sum(value,na.rm=T),.groups="drop") |>
      dplyr::mutate(N = C/5.7,
                    units = 'mTC yr^-1')#20 wet:dry from atlantis, 5.7 C:N from atlantis
  }else{
    warning('pp.type must be either "biomass" or "production"')
  }
  

  if(pp.type == 'biomass'){
    pp.neus = pp.neus |>
      dplyr::filter(variable == "Diatom_N",
                    year >=1998)
  }else{
    pp.neus = pp.neus |> 
      dplyr::filter(year >=1998)
  }
  
  
  if(all(is.na(TL))) {
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
    max.tl= ceiling(max(tl.df$trophicLevel))
    
    tl.df = tl.df |>
      dplyr::filter(Name %in% fished.spp)
    
    TL = mean(tl.df$trophicLevel,na.rm=T)
  }
  
  param.combs = expand.grid(TE = TE, alpha = alpha,year = year,TL = TL,stringsAsFactors = FALSE)|>
    dplyr::mutate(pp.mt.c = NA,
                  threshold = NA)
  
  for(i in 1:nrow(param.combs)){
    
    pp = pp.neus$C[which(year == param.combs$year[i])]
    
    param.combs$threshold[i] = param.combs$alpha[i] * pp * param.combs$TE[i] ^(TL-1)
    param.combs$pp.mt.c[i] = pp
  }
  
  #Get box area
  box.stats = rbgm::bgmfile(bgm.file)$boxes |> 
    dplyr::filter(.bx0 %in% active.box)
  
  #sum total area and convert from m^2 to km^2
  totalArea = sum(box.stats$area) * 1E-6
  
  param.combs = param.combs |> 
    dplyr::mutate(ryther = threshold / totalArea,
                  fogarty = threshold/ pp.mt.c)
    
  
  return(param.combs)

}

# est_link_threshold(param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
#                    atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/master_06162025/',
#                    dietSource = 'detdiet',
#                    TL = NA,
#                    TE = c(0.1,0.15),
#                    alpha = c(0.15,0.2),
#                    year = c(2000,2010))