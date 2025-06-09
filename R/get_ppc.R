#' Read in the output to get primary production consumed
#'
#' Get primary production data from nc forcing files
#' NEUS atlantis is forced with 3 primary producer species (Diatoms, Dinoflagellates,Picophytoplankton)
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param fgs Character String. Name of groups.csv file
#'@param timeRange Vector
#'@param dietSource string. Source of diet matrix, either 'realized' or 'prm'. Default is 'prm'.
#'@param plottl Boolean. Plot the Trophic level to window. Default = F
#'
#'@return data.frame
#'
#'@export

# param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
# atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/Dev_11032022/'
# fgs = paste0(param.dir,'neus_groups.csv')
# dietSource = 'realized'
# timeRange = 30:52

get_ppc <- function(param.dir, atl.dir, fgs,dietSource, timeRange, plottl = F){
  
  #Get main.nc, prod.nc, and diet matrix
  main.file = paste0(atl.dir, 'neus_output.nc')
  prod.file = paste0(atl.dir,'neus_outputPROD.nc')
  bio.file = paste0(param.dir,'at_biology.prm')
  diet.file = paste0(atl.dir, 'neus_outputDietCheck.txt')
  bgm.file = paste0(param.dir,'neus_tmerc_RM2.bgm')
  
  fgs.df = read.csv(fgs,as.is =T)
  
  #Define primary producers
  phyto.spp = fgs.df$Name[which(fgs.df$GroupType %in% c('LG_PHY','DINOFLAG','SM_PHY','SEAGRASS','MICROPHYTOBENTHOS','PHYTOBEN'))]
  
  #Define maturity age for species
  source(url('https://raw.githubusercontent.com/NEFSC/READ-EDAB-neusAtlantis/refs/heads/master/R/Calibration_Tools/get_age_mat.R'))
  age.mat = get_age_mat(bio.file = bio.file, write =F)
  
  bgm =rbgm::bgmfile(bgm.file)$boxes

  #filter diet to just primary producer prey
  diet = atlantiseof::get_diet_prop(param.dir = param.dir,
                                    atl.dir = atl.dir,
                                    fgs = fgs,detDietfile = F,plottl = F, dietSource = dietSource) %>%
    dplyr::filter(prey %in% phyto.spp & consumption > 0) %>%
    dplyr::left_join(select(fgs.df,Code,Name,NumCohorts,NumAgeClassSize), by = c('pred' = 'Name')) %>%
    dplyr::left_join(age.mat, by = c('Code'= 'spp'))%>%
    dplyr::mutate(age.mat = ifelse(is.na(age.mat),1,age.mat))
  
  
  if(dietSource == 'realized'){
    diet =diet %>%
      dplyr::mutate(pred_stanza = ifelse(age.mat > age.mat,2,1))%>%
      dplyr::select(time,pred,agecl,pred_stanza,prey,consumption, prop.consumption)%>%
      dplyr::filter(time >= min(timeRange) & time <= max(timeRange))
  }
  
  grazer.spp = sort(unique(diet$pred))
  
  #Get consumption from Prod.nc
  prod.nc = ncdf4::nc_open(prod.file)
  prod.names = names(prod.nc$var)
  prod.eat.names = grep('*_Eat|*Grazing',prod.names,value =T)
  prod.time.d = prod.nc$dim$t$vals/86400
  prod.time.yr = prod.time.d / 365
  prod.time.match = which(prod.time.yr >= min(timeRange) & prod.time.yr <= max(timeRange))
  
  grazer.all.ls = list()
  i=j=1
  for(i in 1:length(grazer.spp)){
    
    #get eat vars that start with grazer.spp
    this.grazer = grep(paste0('\\b',grazer.spp[i]),prod.eat.names,value =T)
    #pull ageclass from this.grazer that starts with grazer.pp and ends with _Eat
    this.agecl = as.numeric(unlist(regmatches(this.grazer,gregexpr("\\d{1,2}",this.grazer))))
    this.grazer.mat = as.numeric(age.mat$age.mat[which(age.mat$spp == fgs.df$Code[which(fgs.df$Name == grazer.spp[i])])])
    
    this.grazer.ls = list()
    for(j in 1:length(this.grazer)){
      
      #get data from each grazer variable
      this.grazer.diet = diet %>%
        dplyr::filter(pred == grazer.spp[i])
      this.grazer.data = colSums(ncdf4::ncvar_get(prod.nc, this.grazer[j])[,prod.time.match] * bgm$area * abs(bgm$botz) )
      
      if(length(this.agecl) == 0){
        this.grazer.diet = this.grazer.diet %>%
          dplyr::group_by(pred,prey)%>%
          dplyr::summarise(prop.consumption = mean(prop.consumption,na.rm=T))
        
        this.grazer.consumed =data.frame(lapply(this.grazer.diet$prop.consumption, function(x) {this.grazer.data * x}) )
        colnames(this.grazer.consumed) = this.grazer.diet$prey
        this.grazer.ls[[j]] = this.grazer.consumed %>%
          dplyr::mutate(time = prod.time.yr[prod.time.match]) %>%
          tidyr::gather('prey','consumption',-time)%>%
          dplyr::mutate(pred = grazer.spp[i],
                        agecl =1,
                        pred_stanza = 1,
          )
      }else{
        this.grazer.stanza = ifelse(this.agecl[j] > this.grazer.mat,2,1)
        this.grazer.diet = diet %>%
          dplyr::filter(pred == grazer.spp[i] & pred_stanza == this.grazer.stanza)
        
        if(nrow(this.grazer.diet) == 0){next()}
        
      
        
        if(dietSource == 'prm'){

          this.grazer.consumed =data.frame(lapply(this.grazer.diet$prop.consumption, function(x) {this.grazer.data * x}) )
          colnames(this.grazer.consumed) = this.grazer.diet$prey
          
          this.grazer.ls[[j]] = this.grazer.consumed %>%
            dplyr::mutate(time = prod.time.yr[prod.time.match]) %>%
            tidyr::gather('prey','consumption',-time)%>%
            dplyr::mutate(pred = grazer.spp[i],
                          agecl = this.agecl[j],
                          pred_stanza = this.grazer.stanza,
            )
        }else{
          
          this.grazer.diet %>%
            dplyr::filter(pred == grazer.spp[i], agecl == this.agecl[j])
            dplyr::select(time,pred,agecl,pred_stanza,prey,prop.consumption) %>%
            dplyr::left_join(data.frame(time = prod.time.yr[prod.time.match], eat = this.grazer.data))
          
          data.frame(pred = grazer.spp[i],
                     agecl = this.agecl[j],
                     time = prod.time.yr[prod.time.match],
                     pred_stanza = this.grazer.stanza,
                     eat = this.grazer.data) %>%
            dplyr::left_join()
        }

      }

      
    }
    grazer.all.ls[[i]] = dplyr::bind_rows(this.grazer.ls)%>%
      dplyr::group_by(time,pred,prey)%>%
      dplyr::summarise(consumption = sum(consumption,na.rm=T))%>%
      dplyr::mutate(units = 'mgN d-1')
  }
  
  grazer.all.df = dplyr::bind_rows(grazer.all.ls)
  
  grazer.all.annual = grazer.all.df %>%
    dplyr::mutate(year = floor(time))%>%
    dplyr::filter(consumption > 0) %>%
    dplyr::group_by(year,pred,prey)%>%
    dplyr::summarise(consumption.mgCd = mean(consumption,na.rm=T))%>%
    dplyr::mutate(consumption.mTCyr = consumption.mgCd * 365* 5.7 * 1E-9)
  
  if(plottl){
    
    ggplot2::ggplot(grazer.all.annual,ggplot2::aes(x = year, y = consumption.mTCyr))+
      ggplot2::geom_line()+
      ggplot2::facet_grid(prey~pred, scales = 'free_y')
    
    grazer.all.prey = grazer.all.annual %>%
      dplyr::group_by(year,prey)%>%
      dplyr::summarise(consumption.mTCyr = sum(consumption.mTCyr,na.rm=T))
    
    ggplot2::ggplot(grazer.all.prey,ggplot2::aes(x = year, y = consumption.mTCyr))+
      ggplot2::geom_line()+
      ggplot2::facet_wrap(~prey)
  }
  
  ncdf4::nc_close(prod.nc)
  
  return(grazer.all.annual)
}
