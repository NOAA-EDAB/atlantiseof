#'@description Function to calculate smallfish-per-largefish ratio based on Atlantis output
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param survdat dataframe. Returned from survdat::get_survdat() 
#'@param out.dir character string. Path to output file
#'@param show.plot logical. If TRUE, will plot the results
#'@param timeRange numeric vector. Time range to filter the data (e.g., c(1980, 2020)). If NULL, uses all available data.
#'

param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/base_run_eof/'
survdat = readRDS(here::here('data-raw','survey_lenagewgt.rds'))
out.dir = here::here("data-raw","fish_prod.rds")
show.plot = T
timeRange = 30:60

calc_fish_prod = function(param.dir, atl.dir, survdat, out.dir, show.plot = FALSE, timeRange) {
  
  #Read functional groups file
  param.ls = atlantisprocessing::get_atl_paramfiles(param.dir = param.dir, atl.dir = atl.dir, run.prefix = 'neus_output',include_catch = T)
  
  fgs = read.csv(param.ls$groups.file) |> dplyr::select('Code','LongName')
 #filter survdat to only observations with ages and lengths
 #find mean size of age1 species to define "small fish"
 survdat.juv = survdat |>
   dplyr::filter(!is.na(AGE) & !is.na(LENGTH)) |>
   dplyr::filter(AGE == 1) |> 
   dplyr::group_by(Code) |>
   dplyr::summarise(length.small = mean(LENGTH, na.rm = TRUE), .groups = 'drop')
  
  #Read in Atlantis lengths from atlantisprocessing post-processing
 length.file = paste0(atl.dir,'Post_Processed/Data/length_age.rds')
 if(!file.exists(length.file)){
   atlantisprocessing::process_atl_output(param.dir= param.dir, atl.dir = atl.dir,run.prefix = 'neus_output',param.ls = param.ls, plot.length.age = T)
 }
 length.df = readRDS(length.file) |> 
   dplyr::rename(atl.length = 'atoutput')
 
 #Read in Atlantis abundance from atlantisprocessing post-processing
 num.file = paste0(atl.dir,'Post_Processed/Data/numbers_age.rds')
 if(!file.exists(num.file)){
   atlantisprocessing::process_atl_output(param.dir= param.dir, atl.dir = atl.dir,run.prefix = 'neus_output',param.ls = param.ls, plot.numbers.timeseries = T)
 }
 num.df = readRDS(num.file) |> 
   dplyr::rename(atl.num = 'atoutput')
 
 #Combine length and abundance
 length.abund.df = length.df |> 
   dplyr::left_join(num.df) |> 
   dplyr::left_join(fgs,by = c('species' = 'LongName')) |> 
   dplyr::left_join(survdat.juv) |> 
   dplyr::mutate(size = ifelse(atl.length < length.small, 'small', 'large'))

 #Identify abundance of small fish and large fish
 abund.small = length.abund.df |> 
   dplyr::filter(size == 'small') |> 
   dplyr::group_by(time, Code) |> 
   dplyr::summarise(abund.small = sum(atl.num, na.rm = TRUE), .groups = 'drop') |> 
   dplyr::mutate(year.ref = time + 1) |>
   dplyr::select(-time)
 
 abund.small = abund.small |> 
   dplyr::group_by(Code) |> 
   dplyr::mutate(abund.small.anom = abund.small - mean(abund.small, na.rm = TRUE)) |> 
   dplyr::ungroup()
 
 abund.large = length.abund.df |> 
   dplyr::filter(size == 'large') |> 
   dplyr::group_by(time, Code) |> 
   dplyr::summarise(abund.large = sum(atl.num, na.rm = TRUE), .groups = 'drop') |> 
   dplyr::mutate(year.ref = time) |> 
   dplyr::select(-time)
 
 #calculate abundance anomaly time series for small and large fish

 
 #Join small and large fish abundances
 abund.all = abund.small |> 
   dplyr::left_join(abund.large, by = c('Code','year.ref')) |> 
   dplyr::filter(year.ref %in% timeRange) |>
   dplyr::mutate(small.large.ratio = abund.small / abund.large) |>
   dplyr::filter(!is.na(small.large.ratio) & !is.infinite(small.large.ratio)) |> 
   dplyr::group_by(Code) |> 
   dplyr::mutate(small.large.ratio.mean = mean(small.large.ratio,na.rm=T)) |> 
   dplyr::ungroup() |> 
   dplyr::mutate(small.large.ratio.anom = small.large.ratio - small.large.ratio.mean) |> 
   dplyr::rename(time = 'year.ref')
   
  
 #Optional plot
 if(show.plot){
   
   #Stacked bar plot
   ggplot2::ggplot(abund.all, ggplot2::aes(x = time, y = small.large.ratio.anom, fill = Code)) +
     ggplot2::geom_bar(position = 'stack',stat = 'identity') +
     ggplot2::labs(title = "Smallfish-to-Largefish Ratio Over Time",
                  x = "Year",
                  y = "Smallfish-to-Largefish Ratio") +
     ggplot2::theme_minimal()
 }
 
 #anomaly ratio aggregated by system
 out.df = abund.all |> 
   dplyr::group_by(time) |> 
   dplyr::summarise(small.large.ratio.anom.mean = sum(small.large.ratio.anom, na.rm = TRUE), .groups = 'drop') |> 
   dplyr::rename(year = 'time')
 

 if(show.plot){
   #Plot the anomaly ratio over time
   ggplot2::ggplot(out.df, ggplot2::aes(x = year, y = small.large.ratio.anom.mean)) +
     ggplot2::geom_line() +
     ggplot2::labs(title = "Mean Smallfish-to-Largefish Ratio Anomaly Over Time",
                  x = "Year",
                  y = "Mean Smallfish-to-Largefish Ratio Anomaly") +
     ggplot2::theme_minimal()
 }

 #Write out
 if(!dir.exists(dirname(out.dir))) {
   dir.create(dirname(out.dir), recursive = TRUE)
 }
 saveRDS(out.df, file = paste0(out.dir, '/fish_prod.rds'))
 
}
