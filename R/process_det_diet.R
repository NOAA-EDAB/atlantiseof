#' Processes detailed diet output for use in atlantisom
#' 
#'@param atl.dir Character String. Path to output directory
#'@param detDietfile Character String. Name of processed zipped detailed diet file
#'@param outputname character String. Name of output file
#'
#'@return DetDiet_Processed.gz
#'
#'@export

process_det_diet <- function(atl.dir, detDietfile, outputname) {
  
  #zip up file
  system2('gzip',args = c('-k',paste0(atl.dir,detDietfile)))
  #then remove zeros and save ans another zip
  script_lines <- c(
    "#!/bin/bash",
    paste0("zcat ",atl.dir,"neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > ",atl.dir,"neus_outputDetDiet_nz.gz")
  )
  writeLines(script_lines, "run_pipeline.sh", sep = "\n")
  system("chmod +x run_pipeline.sh")
  system("bash run_pipeline.sh")
  #strip headers from original file
  script_lines2 = c(
    "#!/bin/bash",
    paste0("zcat ",atl.dir,"neus_outputDetailedDietCheck.txt.gz | head -n1 | gzip > ",atl.dir,"neus_outputDetDietHead.gz")
  )
  writeLines(script_lines2, "run_pipeline2.sh", sep = "\n")
  system("chmod +x run_pipeline2.sh")
  system("bash run_pipeline2.sh")
  #concatenate them
  system2('cat',args = c(paste0(atl.dir,'neus_outputDetDietHead.gz'),paste0(atl.dir,'neus_outputDetDiet_nz.gz')),stdout = paste0(atl.dir,outputname))
  
}