#'Fish productivity indicators
#'Function to calculate smallfish-per-largefish ratio based on Atlantis output
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param survdat Dataframe. Survey data with columns: Code, AGE, LENGTH
#'@param out.dir character string. Path to output file
#'@param show.plot logical. If TRUE, will plot the results
#'@param timeRange numeric vector. Time range to filter the data (e.g., c(1980, 2020)). If NULL, uses all available data.
#'
#'@export

# param.dir = '/home/jcaracappa/NEUS-Atlantis/Joe_Proj/currentVersion/'
# atl.dir = '/home/jcaracappa/EDAB_Dev/jcaracappa/base_run_eof/'
# survdat = readRDS(here::here('data-raw','survey_lenagewgt.rds'))
# out.dir = here::here("data-raw","fish_prod.rds")
# show.plot = T
# timeRange = 30:60

calc_fish_prod = function(param.dir, atl.dir, out.dir, survdat, show.plot = FALSE, timeRange) {
  
  #Read functional groups file
  param.ls = atlantisdiagnostics::get_atl_paramfiles(param.dir = param.dir, atl.dir = atl.dir, run.prefix = 'neus_output',include_catch = T)
  
  fgs = read.csv(param.ls$groups.file) |> dplyr::select('Code','LongName','NumAgeClassSize')
  
  
  #Get age at maturity
  age.mat = atlantiseof::get_age_mat(param.ls$biol.prm)|>
    dplyr::mutate(age.mat = as.numeric(age.mat))
  
  #Get FSPB
  spp.fspb = atlantiseof::get_param_FSPB(param.ls$biol.prm) |>
    dplyr::rename(Code = 'group') |>
    tidyr::pivot_longer(-Code,values_to = 'fspb') |>
    tidyr::separate(name, c('dud','agecl'),sep = '\\.')|>
    dplyr::mutate(agecl = as.numeric(agecl),
                  fspb = as.numeric(fspb))|>
    dplyr::select(-dud)
  
  #filter survdat to only observations with ages and lengths
  #find mean size of age1 species to define "small fish"
  survdat.juv = survdat |>
   dplyr::filter(!is.na(AGE) & !is.na(LENGTH)) |>
   dplyr::filter(AGE == 1) |>
   dplyr::group_by(Code) |>
   dplyr::summarise(length.small = mean(LENGTH, na.rm = TRUE), .groups = 'drop')
  
  #Read in Atlantis lengths from atlantisprocessing post-processing
 length.file = list.files(path = atl.dir, pattern = 'length_age.rds', full.names =T, recursive =T)
 if(!file.exists(length.file)){
   atlantisdiagnostics::process_atl_output(param.dir= param.dir, atl.dir = atl.dir,run.prefix = 'neus_output',param.ls = param.ls, plot.length.age = T)
 }
 length.df = readRDS(length.file) |> 
   dplyr::rename(atl.length = 'atoutput')
 
 #Read in Atlantis abundance from atlantisprocessing post-processing
 num.file = list.files(path = atl.dir, pattern = 'numbers_age.rds', full.names =T, recursive =T)
 if(!file.exists(num.file)){
   atlantisdiagnostics::process_atl_output(param.dir= param.dir, atl.dir = atl.dir,run.prefix = 'neus_output',param.ls = param.ls, plot.numbers.timeseries = T)
 }
 num.df = readRDS(num.file) |> 
   dplyr::rename(atl.num = 'atoutput')
 
 #Read in Atlantis biomass from atlantisprocessing post-processing
 bio.file = list.files(path = atl.dir, pattern = 'biomass_age.rds', full.names =T, recursive =T)
 if(!file.exists(bio.file)){
   atlantisdiagnostics::process_atl_output(param.dir= param.dir, atl.dir = atl.dir,run.prefix = 'neus_output',param.ls = param.ls, plot.biomass.timeseries = T)
 }
 bio.df = readRDS(bio.file) |> 
   dplyr::rename(atl.bio = 'atoutput')
 
 #Combine length and abundance
 spp.df = length.df |> 
   dplyr::left_join(num.df) |> 
   dplyr::left_join(bio.df) |>
   dplyr::left_join(fgs,by = c('species' = 'LongName')) |> 
   dplyr::left_join(survdat.juv) |> 
   dplyr::left_join(age.mat) |>
   dplyr::left_join(spp.fspb) |>
   dplyr::mutate(size = ifelse(atl.length < length.small, 'small', 'large'),
                 is.adult = ifelse(agecl >= age.mat, T,F),
                 weight.ind = atl.bio/atl.num)
                
 
 #Identify abundance of small fish and large fish
 bio.small = spp.df |> 
   dplyr::filter(is.adult == F) |> 
   dplyr::mutate(juv.biomass = (atl.num/NumAgeClassSize)*weight.ind) |>
   dplyr::group_by(time, Code) |> 
   dplyr::summarise(juv.biomass = sum(juv.biomass, na.rm = TRUE), .groups = 'drop') |> 
   dplyr::mutate(year.ref = time + 1) |>
   dplyr::select(-time)
 
 bio.large = spp.df |> 
   dplyr::filter(is.adult == T) |> 
   dplyr::mutate(adult.biomass = weight.ind * atl.num * fspb * is.adult) |> 
   dplyr::group_by(time, Code) |> 
   dplyr::summarise(adult.biomass = sum(adult.biomass,na.rm=T))|>
   dplyr::rename(year.ref = 'time')
   
 
 #calculate abundance anomaly time series for small and large fish

 
 #Join small and large fish abundances
 bio.all = bio.small |> 
   dplyr::left_join(bio.large, by = c('Code','year.ref')) |> 
   dplyr::filter(year.ref %in% timeRange) |>
   dplyr::mutate(small.large.ratio = juv.biomass / adult.biomass) |>
   dplyr::filter(!is.na(small.large.ratio) & !is.infinite(small.large.ratio)) |> 
   dplyr::group_by(Code) |> 
   dplyr::mutate(small.large.ratio.mean = mean(small.large.ratio,na.rm=T)) |> 
   dplyr::ungroup() |> 
   dplyr::mutate(small.large.ratio.anom = small.large.ratio - small.large.ratio.mean) 
   
   
  
 #Optional plot
 if(show.plot){
   
   #Stacked bar plot
   ggplot2::ggplot(bio.all, ggplot2::aes(x = year.ref, y = small.large.ratio.anom, fill = Code)) +
     ggplot2::geom_bar(position = 'stack',stat = 'identity') +
     ggplot2::labs(title = "Smallfish-to-Largefish Ratio Over Time",
                  x = "Year",
                  y = "Smallfish-to-Largefish Ratio") +
     ggplot2::theme_minimal()
   ggplot2::ggsave(paste0(out.dir,'fish_productivity_species.png'))
 }
 
 #anomaly ratio aggregated by system
 out.df = bio.all |> 
   dplyr::group_by(year.ref) |> 
   dplyr::summarise(small.large.ratio.anom.mean = sum(small.large.ratio.anom, na.rm = TRUE), .groups = 'drop')
   
 

 if(show.plot){
   #Plot the anomaly ratio over time
   ggplot2::ggplot(out.df, ggplot2::aes(x = year.ref, y = small.large.ratio.anom.mean)) +
     ggplot2::geom_line() +
     ggplot2::labs(title = "Mean Smallfish-to-Largefish Ratio Anomaly Over Time",
                  x = "Year",
                  y = "Mean Smallfish-to-Largefish Ratio Anomaly") +
     ggplot2::theme_minimal()
   ggplot2::ggsave(paste0(out.dir,'fish_productivity_anomaly.png'))
 }


 
 if(!missing(out.dir)){
   #Write out
   if(!dir.exists(dirname(out.dir))) {
     dir.create(dirname(out.dir), recursive = TRUE)
   }
   
   saveRDS(out.df, file = paste0(out.dir, '/fish_prod.rds'))
 }

 return(out.df)
 
}
