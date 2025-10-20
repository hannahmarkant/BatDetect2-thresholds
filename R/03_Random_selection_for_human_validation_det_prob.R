#######################################################################################
# Random sampling of audio files for the human validation process. 1 % of the audio files
# were randomly chosen from the entire data set (1450 audio files containing high frequency events).
# Furthermore, additional data for human validation can be added in 1 % steps. 
# Merging the human validation tables of the 1% steps together.
########################################################################################

library(data.table)
library(dplyr)

# Loading CSV file with the list of all audio files containing high frequency sound events
final_combined_files_path <- "./final_combined_data.csv"
final_combined_data <- fread(final_combined_files_path, sep = ";", dec = ".")

# Random selection of 483 rows (1 % of the audio files)
set.seed(123)  # Setting a seed for reproducibility
sample_size <- 483

sampled_files <- final_combined_data %>%
  sample_n(sample_size, replace = FALSE)

# Saving a CSV file with a list of the 483 dragged files
output_folder <- "./" 
output_csv_path <- file.path(output_folder, "sampled_files1.csv")

fwrite(sampled_files, output_csv_path, sep = ";", dec = ".", row.names = FALSE)

# Creating a destination folder for the copies of the audio files
destination_folder <-"./Random_Samples"
dir.create(destination_folder, showWarnings = FALSE)

# Copying audio files
copied_files <- list()

for (i in 1:nrow(sampled_files)) {
  source_path <- file.path(sampled_files$FileDirectory[i], sampled_files$FileName[i])
  
  # Creating new file name containing the folder name and the original file name
  new_file_name <- paste0(sampled_files$FolderName[i], "_", sampled_files$FileName[i])
  dest_path <- file.path(destination_folder, new_file_name)
  
  # Checking if a file exists, then copying the file
  if (file.exists(source_path)) {
    file.copy(source_path, dest_path, overwrite = FALSE)
    
    # Storing the copy information
    copied_files[[i]] <- data.frame(
      OriginalFolder = sampled_files$FolderName[i],
      FileName = sampled_files$FileName[i],
      NewFilePath = dest_path
    )
  } else {
    warning(paste("File not found:", source_path))
  }
}


# Saving a CSV file with the metadata of the copied files

copied_files_df <- rbindlist(copied_files)
output_csv_path <- file.path(destination_folder, "copied_files_metadata.csv")

fwrite(copied_files_df, output_csv_path, sep = ";", dec = ".", row.names = FALSE)



### random sampling of an additional 1 %
# Loading the main data set (`final_combined_data.csv`)
final_combined_files_path <- "./final_combined_data.csv"
final_combined_data <- fread(final_combined_files_path, sep = ";", dec = ".")


# Loading the data set with the first 1% (or 2% depending on how many were already added) from the file 'sampled_files1.csv'
sampled_files_path <- "./sampled_files1.csv"
selected_files <- fread(sampled_files_path, sep = ";", dec = ".")


# Removing already chosen files from the 'final_combined_data' data set
remaining_files <- anti_join(final_combined_data, selected_files, by = c("FolderName", "FileName"))

# Randomly dragging 483 (1 %) more files without duplicates
set.seed(123)  # Setting a seed for reproducibility
additional_sample <- remaining_files %>%
  sample_n(483, replace = FALSE)

# Saving a CSV file with a list of the new 483 dragged files (this can be used as information 
# for the next addition of 1 %, need to remove these then also from the "final_combined_data")
output_csv_path <- file.path(output_folder, "sampled_files2.csv")

fwrite(additional_sample, output_csv_path, sep = ";", dec = ".", row.names = FALSE)

# Creating destination folder for new audio files
destination_folder <- "./Random_Samples2"
dir.create(destination_folder, showWarnings = FALSE)

# Copying audio files

copied_files_new <- list()

for (i in 1:nrow(additional_sample)) {
  source_path <- file.path(additional_sample$FileDirectory[i], additional_sample$FileName[i]) 
  
  # Creating new file name containing the folder name and the original file name
  new_file_name <- paste0(additional_sample$FolderName[i], "_", additional_sample$FileName[i])
  dest_path <- file.path(destination_folder, new_file_name)
  
  # Checking if a file exists, then copying the file
  if (file.exists(source_path)) {
    file.copy(source_path, dest_path, overwrite = FALSE)
    
    # Storing the copy information
    copied_files_new[[i]] <- data.frame(
      FolderName = additional_sample$FolderName[i],
      FileName = additional_sample$FileName[i],
      NewFilePath = dest_path
    )
  } else {
    warning(paste("File not found:", source_path))
  }
}

# Saving a CSV file with the metadata of the copied files
copied_files_new_df <- rbindlist(copied_files_new)
output_csv_path_new <- file.path(destination_folder, "copied_files_new.csv")

fwrite(copied_files_new_df, output_csv_path_new, sep = ";", dec = ".", row.names = FALSE)


# After doing this validation process, there are more than one table. These can be merged like this: 
library(readxl)
library(dplyr)
library(openxlsx)

# Load the 2 or more excel files
df1 <- read_excel("./Det_threshold_manual_1.%_data.xlsx")
df2 <- read_excel("./Det_threshold_manual_2.%_data.xlsx")


# Ensuring that the 'Validation' column has the same data type (e.g., character)
df1$Validation <- as.character(df1$Validation)
df2$Validation <- as.character(df2$Validation)

df2$det_prob <- as.numeric(gsub(",", ".", df2$det_prob))
df2$start_time <- as.numeric(gsub(",", ".", df2$start_time))
df2$end_time <- as.numeric(gsub(",", ".", df2$end_time))
df2$class_prob <- as.numeric(gsub(",", ".", df2$class_prob))
df1$det_prob <- as.numeric(df1$det_prob)

# Removing the 'Time' column if it exists
df1$Time <- NULL
df2$Time <- NULL

# Combining the two data frames by rows
df_merged <- bind_rows(df1, df2)

# Creating a 'Time' column from 'Recording_Time' in HH:MM:SS format
df_merged$Time <- format(df_merged$Recording_Time, format = "%H:%M:%S")

# Saving the merged dataframe as a new Excel file
write.xlsx(df_merged, "./Det_threshold_manual_total_2%_data.xlsx")
