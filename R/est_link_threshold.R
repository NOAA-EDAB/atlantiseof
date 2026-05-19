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
#'
#' @return dataframe
#'
#'@export



est_link_threshold = function(atl.dir, param.dir, dietSource = NA, year, TE, alpha, TL){
  
  fgs = read.csv(paste0(param.dir, 'neus_groups.csv'), stringsAsFactors = FALSE)
  fished.spp = fgs$Name[which(fgs$isFished ==1)]
  
  bgm.file = file.path(param.dir, "neus_tmerc_RM2.bgm")
  
  if(pp.type == 'biomass'){
    ppdata <- atlantiseof::get_pp(bgm = bgm.file,pathToForcing = paste0(param.dir,'tsfiles/Annual_Files/'))  
  }else if(pp.type == 'production'){
    ppdata.raw = read.csv(here::here('data-raw','data','MERGED_ANNUAL_SUM-NES_EPU_STATISTICAL_AREAS_NOEST-PPD-VGPM2_CHLOR_A-CCI-STATS-V2024.CSV'))
  }else{
    warning('pp.type must be either "biomass" or "production"')
  }
  
  
  neus.shp = NEFSCspatial::Neus_atlantis |> sf::st_as_sf() |> 
    dplyr::arrange(BOX_ID)
  active.box = neus.shp$BOX_ID[which(neus.shp$boundary== 0)]
  
  pp.neus <- ppdata$dailyspeciesbox |>
    dplyr::left_join(neus.shp,by = c("box"="BOX_ID")) |>
    dplyr::filter(boundary == 0) |> #remove boundary boxes
    dplyr::group_by(year,variable) |>
    dplyr::summarise(N = sum(value),.groups="drop") |>
    dplyr::mutate(C = 5.7*N) |> #20 wet:dry from atlantis
    dplyr::filter(variable == "Diatom_N",
                  year >=1998)
  
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