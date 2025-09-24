#' Processes detailed diet output for use in atlantisom
#' 
#'@param atl.dir Character String. Path to output directory
#'@param detDietfile Character String. Name of processed zipped detailed diet file
#'@param outputname character String. Name of output file
#'@param cloud Logical. If TRUE, run on cloud Default is FALSE.
#'
#'@return DetDiet_Processed.gz
#'
#'@export

process_det_diet <- function(atl.dir, detDietfile, outputname, cloud =F) {
  
  #zip up file
  if(cloud){
    system(paste0('sudo chmod 775 ',atl.dir))
    system2("sudo", args = c("gzip", "-k", paste0(atl.dir, 
                                                  detDietfile)))
    script_lines <- c("#!/bin/bash", paste0("zcat ", atl.dir, 
                                            "neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > ", 
                                            atl.dir, "neus_outputDetDiet_nz.gz"))
    writeLines(script_lines, "run_pipeline.sh", sep = "\n")
    system("sudo chmod +x run_pipeline.sh")
    # system(paste0('sudo chmod +x ',atl.dir,'neus_outputDetDietHead.gz')
    system("sudo bash run_pipeline.sh")
    script_lines2 = c("#!/bin/bash", paste0("sudo zcat ", atl.dir, 
                                            "neus_outputDetailedDietCheck.txt.gz | head -n1 | sudo gzip > ", 
                                            atl.dir, "neus_outputDetDietHead.gz 2>/dev/null"))
    writeLines(script_lines2, "run_pipeline2.sh", sep = "\n")
    system("sudo chmod +x run_pipeline2.sh")
    system("sudo bash run_pipeline2.sh")
    system2("sudo", args = c("cat", paste0(atl.dir, "neus_outputDetDietHead.gz"), 
                             paste0(atl.dir, "neus_outputDetDiet_nz.gz")), stdout = paste0(atl.dir, 
                                                                                           outputname))
    cmd <- paste(
      "sudo zcat",
      shQuote(paste0(atl.dir, "neus_outputDetDietHead.gz")),
      shQuote(paste0(atl.dir, "neus_outputDetDiet_nz.gz")),
      "| sudo tee",
      shQuote(paste0(atl.dir, outputname)),
      "| gzip > /dev/null"
    )
    
    system(cmd)
    
    system(paste0('sudo chomd 775 ',atl.dir, outputname))
  }else{
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
  
}