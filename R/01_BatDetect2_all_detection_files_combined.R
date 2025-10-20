#############################################################################################
# Here, the individual CSV files of the output of BatDetect2 are merged with the metadata 
# (recording time) of the corresponding .wav files and saved as a single CSV file. In addition, 
# this code merges all the resulting CSV files of the individual recorders into one CSV files.
#############################################################################################

library(readr)
library(dplyr)
library(stringr)
library(data.table)

# Defining directories
wav_dir <-  "./"  # Directory containing the .wav files
csv_dir <-  "./"  # Directory containing BatDetect2 results (.csv)

# Loading all .wav file paths
wav_files <- list.files(wav_dir, pattern = "\\.WAV$", full.names = TRUE)

# Loading all .csv detection result files
csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)

# Combining detection data with metadata
combined_data <- lapply(csv_files, function(csv_file) {
  # Read the BatDetect2 output
  data <- read_csv(csv_file)
  
  # Adding a site ID (manually added)
  data <- data %>%
    mutate(id = "ID")
  
  # Extract .wav file name (without extension) from CSV file name
  wav_name <- str_extract(basename(csv_file), ".*(?=\\.csv)")
  
  # Match the .wav file with the name
  matched_wav <- wav_files[grepl(tolower(wav_name), tolower(basename(wav_files)))]
  
  # If no match is found, set values to NA
  if (length(matched_wav) == 0) {
    warning(paste("No WAV file found for:", wav_name))
    recording_time <- NA
    wav_path <- NA
  } else {
    # If a match is found, date and time from the filename (assumes format YYYYMMDD_HHMMSS) are extracted
    date_time <- str_match(wav_name, "(\\d{8})_(\\d{6})")
    if (is.na(date_time[1, 1])) {
      warning(paste("No timestamp found for:", wav_name))
      recording_time <- NA
    } else {
      recording_time <- as.POSIXct(paste0(date_time[1, 2], " ", date_time[1, 3]),
                                   format = "%Y%m%d %H%M%S")
    }
    wav_path <- matched_wav
  }
  
  # Add metadata columns to the dataset
  data <- data %>%
    mutate(WAV_Name = wav_name,
           Recording_Time = recording_time,
           WAV_Path = wav_path) %>%
    select(id, class, WAV_Name, Recording_Time, det_prob, 
           start_time, end_time, high_freq, low_freq, 
           class_prob, WAV_Path)
  
  return(data)
}) %>%
  bind_rows()  # Combining all rows into one data frame

# Adding separate date and time columns
combined_data <- combined_data %>%
  mutate(
    Date = as.Date(Recording_Time),
    Time = format(Recording_Time, "%H:%M:%S") #, Validation = NA #If the BatDetect2 files are merged for Human Validation this code snippet must be added
  )


# Defining the output folder and file path
output_folder <- "./"
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}
output_file <- file.path(output_folder, "all_detections.csv")

# Saving the final data table as a CSV. file
fwrite(combined_data, output_file, sep = ";", dec = ".", row.names = FALSE)

# Merging all the resulting CSV files of the individual recorders into one CSV file
# Setting the working directory
setwd("./")

# Creating a list of all CSV files in the working directory
files <- list.files(pattern = "*.csv")

# Reading all CSV files and combining them into a single data frame
data_combined <- lapply(files, function(file) {
  read.csv(file, sep = ";", header = TRUE, stringsAsFactors = FALSE)
}) %>%
  bind_rows()

# Defining the output folder
output_folder <- "./"

# Defining the path to the output file
output_file <- file.path(output_folder, "all_batdetect2.csv")

# Saving the combined data to a CSV file
fwrite(data_combined, output_file, sep = ";", dec = ",", row.names = FALSE)

