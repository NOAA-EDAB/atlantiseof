#'Atlantis Age at Maturity
#'Function to retrieve age at maturity parameters
#'
#'@param bio.file character. Path to biomass.prm file
#'@param write logical. whether to write output to file
#'
#'@returns dataframe of species and age at maturity
#'
#'@export

get_age_mat = function(bio.file,write =F){
  
  bio.lines = readLines(bio.file)
  age.mat.line.num = grep('_age_mat',bio.lines)
  age.mat.line.val = bio.lines[age.mat.line.num]
  #remove comments
  which.comment = grep("#",age.mat.line.val)
  age.mat.line.num = age.mat.line.num[-which.comment]
  age.mat.line.val = age.mat.line.val[-which.comment]
  #pull group names
  grp.names = unname(sapply(age.mat.line.val,function(x)return(strsplit(x,'_')[[1]][1])))
  #pull mature ages
  mat.age = unname(sapply(age.mat.line.val,function(x)return(strsplit(x,' |  |]t')[[1]][2])))
  #format to table and write
  age.mat.df = data.frame(Code = grp.names,age.mat = mat.age)
  return(age.mat.df)
  if(write){
    write.csv(age.mat.df,here::here('diagnostics','group_mature_age.csv'),row.names = F)  
  }
  
}

#'Edit Atlantis FSPB
#'Function to retrieve FSPB paramters
#'
#'@param bio.file character. Path to biomass.prm file
#'@param write logical. whether to write output to file
#'
#'@returns dataframe of species and FSPB
#'
#'@export


get_param_FSPB = function(bio.prm){
  
  bio.lines = readLines(bio.prm)
  
  FSPB.line = grep(paste0('^FSPB_'),bio.lines)
  
  groups = sapply(bio.lines[FSPB.line],function(x) return(strsplit(x,'\t| |_')[[1]][2]),USE.NAMES = F)
  
  out.df = as.data.frame(matrix(NA,nrow = length(groups), ncol = 13))
  colnames(out.df) = c('group',paste0('age.',1:12))
  out.df$group = groups
  
  for(i in 1:length(groups)){
    vals = strsplit(bio.lines[FSPB.line[i]+1],' |\t')[[1]]
    out.df[i,2:(length(vals)+1)] = vals
  }
  
  return(out.df)
}
