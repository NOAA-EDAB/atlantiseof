#'Biomass Proportion
#'Calculates the mean biomass proportion from an atlantis run
#'
#'@param atl.dir character, directory containing atlantis output files
#'@param fgs.file character, path to the functional groups file
#'@param timeRange numeric vector, range of years to summarize
#'
#'@return vector of mean biomass proportions for each group in the specified time range
#'
#'@export
#'

calc_bio_prop = function(atl.dir,fgs.file,timeRange){
  
  fgs = read.csv(fgs.file, stringsAsFactors = FALSE) |> 
    dplyr::filter(IsTurnedOn == 1) |> 
    dplyr::select(Code,Name)
  
  bio.df = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'),header=T, fill = T) |> 
    dplyr::select(Time,dplyr::all_of(fgs$Code)) |> 
    dplyr::mutate(year = floor(Time/ 365)) |>
    dplyr::filter(year %in% timeRange) |>
    tidyr::pivot_longer(names_to = "Code", 
                         values_to = "biomass", 
                        -Time) |>
    dplyr::group_by(Code) |> 
    dplyr::summarize(biomass = mean(biomass,na.rm=T)) |> 
    dplyr::mutate(biomass.total = sum(biomass,na.rm=T),
                  biomass.prop = biomass / biomass.total)
  
  out = dplyr::select(bio.df, Code, biomass.prop)
  
  return(out)
}