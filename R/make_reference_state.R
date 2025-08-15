# Calculates ecosystem indicators from a reference run and converts them to a desired state based on quantiles of timeseries
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param group.index Character String. Name of the group index with non-atlantis categories
#'@param fgs.file Character String. Name of the functional groups file
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param timeRange Numeric Vector. Range of years to include in the analysis
#'@param cloud Logical. If TRUE, run on cloud Default is FALSE.
#'@param out.name Character String. Name of the output file to save the results
#'@param desired.p Numeric Vector. Quantiles to use for desired min/max values, default is c(0.25, 0.75)
#'
#'@return dataframe of eco indicators by year
#'
#'@export
#'

make_reference_state = function(param.dir, atl.dir, group.index, fgs.file,
                                dietSource, timeRange, cloud = FALSE, desired.p = c(0.25,0.75)) {
  
  # Get mean status from reference run
  eco.ind = atlantiseof::make_eco_indicators(
    param.dir = param.dir,
    atl.dir = atl.dir,
    group.index = group.index,
    fgs.file = fgs.file,
    dietSource = dietSource,
    timeRange = timeRange
  )
  
  # Get time series indicators from reference run
  eco.ind.year = atlantiseof::make_eco_indicators_time(
    param.dir = param.dir,
    atl.dir = atl.dir,
    group.index = group.index,
    fgs.file = fgs.file,
    dietSource = dietSource,
    timeRange = timeRange
  )
  saveRDS(eco.ind.year, here::here('data-raw',paste0(out.name,'eco_state_year.rds')))
  #Calculate summary statistics on annual eco indicators and desired min/max based on quantiles
  summary.base.state = eco.ind.year %>%
    tidyr::gather(Variable,Value,-year)%>%
    dplyr::group_by(Variable)%>%
    dplyr::summarise(mean.value = mean(Value[is.finite(Value)], na.rm = TRUE),
                     median.value = median(Value[is.finite(Value)], na.rm = TRUE),
                     sd.value = sd(Value[is.finite(Value)]),
                     abs.min.value = min(Value[is.finite(Value)], na.rm = TRUE),
                     abs.max.value = max(Value[is.finite(Value)], na.rm = TRUE),
                     desired.min = quantile(Value[is.finite(Value)], desired.p[1], na.rm = TRUE),
                     desired.max = quantile(Value[is.finite(Value)], desired.p[2], na.rm = TRUE))%>%
    dplyr::mutate(desired.min.scaled = (desired.min-abs.min.value)/(abs.max.value - abs.min.value),
                  desired.max.scaled = (desired.max-abs.min.value)/(abs.max.value - abs.min.value),
                  desired.center = (desired.min.scaled + desired.max.scaled)/2,
                  desired.range = desired.max.scaled - desired.min.scaled
    )
  
  #Convert any Inf to NA
  summary.base.state[] <- lapply(summary.base.state, function(col) {
    if (is.numeric(col)) col[!is.finite(col)] <- NA
    col
  })
  
  #Write reference run indicators to file
  saveRDS(summary.base.state, here::here('data-raw',paste0(out.name,'eco_state.rds')))
  
}