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

process_det_diet <- function(atl.dir, detDietfile, outputname, cloud = FALSE) {
  
  # Ensure directory path ends with a slash to prevent malformed paths
  if (!grepl("/$", atl.dir)) atl.dir <- paste0(atl.dir, "/")
  
  # Generate unique temporary script paths for this specific parallel worker
  pipeline_nz_sh <- tempfile(pattern = "pipeline_nz_", fileext = ".sh")
  pipeline_head_sh <- tempfile(pattern = "pipeline_head_", fileext = ".sh")
  
  # Define file paths
  input_file <- paste0(atl.dir, detDietfile)
  nz_output <- paste0(atl.dir, "neus_outputDetDiet_nz.gz")
  head_output <- paste0(atl.dir, "neus_outputDetDietHead.gz")
  final_output <- paste0(atl.dir, outputname)
  
  if (cloud) {
    system(paste0('sudo chmod 775 ', atl.dir))
    system2("sudo", args = c("gzip", "-k", input_file))
    
    # Remove zeros and save as another zip
    script_lines <- c(
      "#!/bin/bash", 
      paste0("zcat ", input_file, ".gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > ", nz_output)
    )
    writeLines(script_lines, pipeline_nz_sh, sep = "\n")
    system(paste("sudo chmod +x", pipeline_nz_sh))
    system(paste("sudo bash", pipeline_nz_sh))
    
    # Strip headers from original file
    script_lines2 <- c(
      "#!/bin/bash", 
      paste0("sudo zcat ", input_file, ".gz | head -n1 | sudo gzip > ", head_output, " 2>/dev/null")
    )
    writeLines(script_lines2, pipeline_head_sh, sep = "\n")
    system(paste("sudo chmod +x", pipeline_head_sh))
    system(paste("sudo bash", pipeline_head_sh))
    
    # Concatenate
    system2("sudo", args = c("cat", head_output, nz_output), stdout = final_output)
    
    cmd <- paste(
      "sudo zcat", shQuote(head_output), shQuote(nz_output),
      "| sudo tee", shQuote(final_output), "| gzip > /dev/null"
    )
    system(cmd)
    system(paste0('sudo chmod 775 ', final_output))
    
  } else {
    
    system2('gzip', args = c('-k', input_file))
    
    # Remove zeros and save as another zip
    script_lines <- c(
      "#!/bin/bash",
      paste0("zcat ", input_file, ".gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > ", nz_output)
    )
    writeLines(script_lines, pipeline_nz_sh, sep = "\n")
    system(paste("chmod +x", pipeline_nz_sh))
    system(paste("bash", pipeline_nz_sh))
    
    # Strip headers from original file
    script_lines2 <- c(
      "#!/bin/bash",
      paste0("zcat ", input_file, ".gz | head -n1 | gzip > ", head_output)
    )
    writeLines(script_lines2, pipeline_head_sh, sep = "\n")
    system(paste("chmod +x", pipeline_head_sh))
    system(paste("bash", pipeline_head_sh))
    
    # Concatenate them
    system2('cat', args = c(head_output, nz_output), stdout = final_output)
  }
  
  # Clean up the temporary bash scripts
  unlink(c(pipeline_nz_sh, pipeline_head_sh))
}