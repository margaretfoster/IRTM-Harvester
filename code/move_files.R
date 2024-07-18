move_files <- function(source_dir, target_dir, ending) {
  # Check if source directory exists
  if (!file.exists(source_dir)) {
    stop("Source directory does not exist.")
  }
  
  # Create target directory if it doesn't exist
  if (!file.exists(target_dir)) {
    dir.create(target_dir)
  }
  
  # Get list of PNG files in source directory
  png_files <- list.files(source_dir, pattern = paste0("\\.", ending, "$"), full.names = TRUE)
  
  # Move PNG files to target directory
  file.copy(png_files, target_dir, overwrite = TRUE)
  
  cat(paste0(ending, " files moved successfully to",  target_dir, "directory.\n"))
}