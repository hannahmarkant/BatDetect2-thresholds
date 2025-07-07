####################################################################################################################################
# This script extracts metadata from all `.wav` audio files in a given directory and saves the information (file name, folder name, 
# file size, etc.) in an CSV file. Furthermore, it filters the audio files to a minimum size of 59 KB. Audio files that are larger
# contain high-frequency sound events. The other files are empty. The script copies the high frequencey .wav files based on paths 
# listed in an CSV file to a new destination folder.
####################################################################################################################################

library(readr)
library(data.table)

# Defining the directory containing audio files
directory <- "./"  # Folder path

# Function to extract information about `.wav` files
get_wav_file_info <- function(directory) {
  # List all files with the `.wav` extension (case-insensitive)
  files <- list.files(directory, recursive = TRUE, full.names = TRUE, pattern = "\\.WAV$", ignore.case = TRUE)
  
  # Creating a data frame with file metadata (File name, folder name, file directory and file size)
  file_info <- data.frame(
    FileName = basename(files),
    FolderName = basename(dirname(files)),
    FileDirectory = dirname(files),
    FileSize_KB = round(file.info(files)$size / 1024, 2)  # File size in kilobytes
  )
  
  return(file_info)
}

# Collecting file metadata
wav_file_data <- get_wav_file_info(directory)

# Defining output folder and file name
output_folder <- directory  # Saves CSV file in the same directory
if (!dir.exists(output_folder)) dir.create(output_folder)
output_file <- file.path(output_folder, "wav_file_data.csv") #Naming the CSV file

# Saving the metadata as an CSV file
fwrite(wav_file_data, output_file, sep = ";", dec = ".")

# Filtering the data set
wav_file_data <- read_delim("./",  #Path to the CSV file
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Function to filter files below a certain size (here: 59 KB)
filter_small_files <- function(data, size_threshold) {
  # Keep only files with size >= threshold (in KB)
  filtered_data <- data[data$FileSize_KB >= size_threshold, ]
  return(filtered_data)
}

size_threshold <- 59.00  # Minimum size of 59 KB

# Creating filtered dataset
filtered_wav_file_data <- filter_small_files(wav_file_data, size_threshold)

# Defining output folder
output_folder <- directory  # Saves CSV file in the same directory

# Creating output folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder)

# Defining output file path
output_file <- file.path(output_folder, "filtered_wav_file_data.csv")

# Saving filtered data as CSV
fwrite(filtered_wav_file_data, output_file, sep = ";", dec = ".")

# Loading CSV file with the all filtered .wav files
data <- read_delim("./", delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Defining the root destination folder (in order to sort the files per folder name,
# need these empty folders in the destination folder)
destination_root <- "./" 

# Copying process
for (i in 1:nrow(data)) {
  # Get the source file path
  source_path <- file.path(data$FileDirectory[i], data$FileName[i])
  
  # Defining destination folder and file path
  destination_folder <- file.path(destination_root, data$FolderName[i])
  destination_path <- file.path(destination_folder, data$FileName[i])
  
  # Creating destination folder if it doesn't exist
  dir.create(destination_folder, showWarnings = FALSE, recursive = TRUE)
  
  # Copying file if it exists
  if (file.exists(source_path)) {
    file.copy(source_path, destination_path, overwrite = FALSE)
  } else {
    warning(paste("File not found:", source_path))
  }
}



