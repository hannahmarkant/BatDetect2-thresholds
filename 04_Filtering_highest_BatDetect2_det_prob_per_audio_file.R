#######################################################################################
# This script filters the highest det_prob value from the BatDetect2 analysis for each 
# audio file. It is also saved as a CSV file. This code can either be used during human 
# validation, in which case lines 25 and 26 should be activated. If the code is used 
# for the entire data set when the threshold is already fixed, then line 48 can be used 
# to filter the data set with the threshold value. This is 0.66 in the analysis.
#######################################################################################

library(readr)
library(dplyr)
library(data.table)
library(tidyr)

# Read in the combined BatDetect2 results (CSV file)
combined_data_bat_detect <- read_delim("C:/Users/Willkommen/Documents/Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6_2025/erste_analysen/all_0.6_batdetect2_2025.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Filter for the highest detection probability per WAV file and ID
filtered_data <- combined_data_bat_detect %>%
  group_by(id, WAV_Name) %>%
  slice_max(det_prob, n = 1, with_ties = FALSE) %>% # Keep only one row per group
  ungroup()

###for human validation I rearrange columns: moving id, WAV_Name, det_prob, and Validation to the front (activating the two following lines)
# filtered_data <- filtered_data %>%
#  select(id, WAV_Name, det_prob, Validation, everything()) 

# Define output folder
output_folder <- "C:/Users/Willkommen/Documents/Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6_2025/erste_analysen"

# Path to output CSV file
output_file <- file.path(output_folder, "highest_det_prob_2025.csv")

# Save as CSV with semicolon as separator and comma as decimal
fwrite(filtered_data, output_file, sep = ";", dec = ",", row.names = FALSE)

# Check how many unique combinations of WAV_Name and id exist in the original dataset
length(unique(paste(combined_data_bat_detect$WAV_Name, combined_data_bat_detect$id)))

# Check for missing combinations of WAV_Name and id
missing_combinations <- anti_join(combined_data_bat_detect, filtered_data, by = c("WAV_Name", "id"))

# Print missing combinations
print(missing_combinations)



# After defining the threshold, the entire data for det_prob was filtered to 0.66.

# Apply threshold filter (e.g. det_prob >= 0.66)
# Convert det_prob to numeric (replace , with .)
filtered_data$det_prob <- as.numeric(gsub(",", ".", filtered_data$det_prob))
filtered_threshold_data <- filtered_data %>%
  filter(det_prob >= 0.66)

# Extracting "site" and "date" from the "id" collumn. "Date" refers to the date of the collection of the SD cards.
# This handles both formats e.g. "pv9_05.23" and "pv9_ma_05.23". "site" is the name of the recorder and "date" 
# is the exact date of the collection.
filtered_threshold_data <- filtered_threshold_data %>%
  extract(id, into = c("site", "date"), regex = "^([^_]+).*_([0-9]{2}\\.[0-9]{2})$", remove = FALSE)

# Display the result
View(filtered_threshold_data)

# Save threshold-filtered data
output_file_threshold <- file.path(output_folder, "all_0.66_batdetect2_highest_det_prob_threshold_2025.csv")
fwrite(filtered_threshold_data, output_file_threshold, sep = ";", dec = ",", row.names = FALSE)


