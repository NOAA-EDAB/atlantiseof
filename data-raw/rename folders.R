# Set the target directory
target_dir <- "D:/catch_thresholds_eof_2b/"  # Replace with your actual path

# List all folder names
folders <- list.dirs(path = target_dir, full.names = FALSE, recursive = FALSE)

# New pattern: prefix followed by 1 or more digits
pattern <- "^(catch_thresholds_eof_2)(_)([0-9]+)$"

# Loop and rename if matching the pattern
for (folder in folders) {
  if (grepl(pattern, folder)) {
    new_name <- sub(pattern, "\\1b\\2\\3", folder)
    
    old_path <- file.path(target_dir, folder)
    new_path <- file.path(target_dir, new_name)
    
    file.rename(from = old_path, to = new_path)
  }
}
