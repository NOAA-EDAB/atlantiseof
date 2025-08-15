# Calculates ecosystem indicators as multivariate time series of observations
#'
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param group.index Character String. Name of the group index with non-atlantis categories
#'@param fgs.file Character String. Name of the functional groups file
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param timeRange Numeric Vector. Range of years to include in the analysis
#'@param cloud Logical. If TRUE, run on cloud Default is FALSE.
#'
#'@return dataframe of eco indicators by year
#'
#'@export

make_run_state_paired = function(param.dir,atl.dir,group.index,fgs.file,dietSource,timeRange,cloud =F){
  
  year.start = timeRange[1]:timeRange[length(timeRange) - 1]
  year.end = timeRange[2]:timeRange[length(timeRange)]
  
  output.ls = list()
  for(i in 1:length(year.start)){
    
    this.time = atlantiseof::make_eco_indicators(param.dir,
                                     atl.dir,
                                     group.index,
                                     timeRange = year.start[i]:year.end[i],
                                     fgs.file = fgs.file,
                                     dietSource = dietSource,
                                     cloud = cloud)
    
    this.time$year.start = year.start[i]
    this.time$year.end = year.end[i]
    output.ls[[i]] = this.time
  } 

  output.df = dplyr::bind_rows(output.ls)
  
  return(output.df)
}
