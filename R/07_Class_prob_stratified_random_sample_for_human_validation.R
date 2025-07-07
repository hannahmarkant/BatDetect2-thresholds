######################################################################################
# This script does a stratified random sample of BatDetect2 detections by grouping species into
# categories and sampling 25 detections per group- confidence score (class_prob).
# It exports metadata for manual validation.
######################################################################################

# Load required libraries
library(data.table)
library(dplyr)
library(tuneR)
library(openxlsx)
library(readr)

# Loading all audio files with a bat detection (threshold 0.66, det_prob)
batdetect2<- read_delim("C:/Users/Willkommen/Documents/Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6/all_0.66_batdetect2_highest_det_prob_threshold.csv", 
                                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Convert comma to dot and transform to numeric
batdetect2$class_prob <- as.numeric(gsub(",", ".", batdetect2$class_prob))
batdetect2$start_time <- as.numeric(gsub(",", ".", batdetect2$start_time))
batdetect2$end_time <- as.numeric(gsub(",", ".", batdetect2$end_time))

# Assigning species to groups and create new columns
batdetect2 <- batdetect2 %>%
 mutate(
    group = case_when(
      class %in% c("Myotis bechsteinii", "Myotis brandtii", "Myotis daubentonii", "Myotis nattereri") ~ "Myotis_species",
      class %in% c("Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus") ~ "Pipistrellus_species",
      class == "Barbastellus barbastellus" ~ "Barbastellus_barbastellus",
      class %in% c("Nyctalus noctula", "Nyctalus leisleri", "Eptesicus serotinus", "Vespertilio murinus") ~ "Nyctaloid_group",
      class %in% c("Plecotus auritus", "Plecotus austriacus") ~ "Plecotus_species",
      class == "Rhinolophus hipposideros" ~ "Rhinolophus_hipposideros",
      TRUE ~ NA_character_ 
    )
  ) %>%
  filter(!is.na(group)) %>%  # Keep only rows that belong to defined groups
  mutate(
    conf_class = floor(class_prob * 10) / 10,
    sample_class = paste0(group, "_", conf_class), 
    sample_id = paste0(id, "_", WAV_Name, "_", group) 
  )

# Summarizing number of detections per group and confidence confidence score
dets_all <- batdetect2 %>%
  group_by(group, conf_class) %>%
  summarize(
    ndets = n(),
    size = ifelse(ndets < 25, ndets, 25),
    .groups = "drop"
  )

# Summarize total number of detections per group
dets_group_summary <- dets_all %>%
  group_by(group) %>%
  summarize(
    total_ndets = sum(ndets),
    total_size = sum(size),
    .groups = "drop"
  )

# Creating empty dataframe for sampled detections
validets <- data.frame()

# Randomly sampling up to 25 detections per group-confidence score interval
set.seed(123)
for (i in 1:nrow(dets_all)) {
  mysample <- batdetect2 %>%
    filter(group == dets_all$group[i], conf_class == dets_all$conf_class[i]) %>%
    sample_n(size = dets_all$size[i], replace = FALSE)
  
  validets <- rbind(validets, mysample)
  print(i)
}

# Final formatting of sampled data
validets <- validets %>%
  arrange(group, conf_class, class_prob, id) %>%
  mutate(
    start_time = as.numeric(start_time),
    end_time = as.numeric(end_time),
    class_prob = as.numeric(class_prob),
    Validation = NA,
    start = paste0("s", start_time),
    file_name = paste(sample_class, class_prob, WAV_Name, start, sep = "_"),
    landuse = case_when(
      grepl("^pv", id) ~ "pv",
      grepl("^g", id) ~ "grassland",
      TRUE ~ NA_character_
    )
  )

str(validets)

# Saving data as a CSV file
output_folder <- "D:/"  
output_file <- file.path(output_folder, "validets.csv")
fwrite(validets, output_file, sep = ";")

# Loading saved metadata
validets <- read_delim("E:/validets.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Saving as an excel file
write.xlsx(validets, "E:/Human_validation_speciesspecificthreshold.xlsx")

# Creating sub folders per species group
unique_groups <- unique(validets$group)
destination_folder <- "E:/Species_Files3" # need to be created manually


# If data is saved on different locations e.g. hard drives, the filtering is necessary
# to get the audio files from each location after each other, if all on the same location
# this next step is not necessary.
validets <- filter(validets, id == "pv2_so")

# Defining the source folder where WAV files are stored
source_folder <- "E:/pv2_so_hifreq"

for (i in 1:nrow(validets)) {
  # Reading from full path stored in the data
  source_file <- validets$WAV_Path[i]
  
  print(paste("Processing file:", source_file))
  
  if (file.exists(source_file)) {
    myWave <- readWave(source_file)
    
    } else {
    warning(paste("File not found:", source_file))
    next
  }
  
  # Naming and saving
  dest_folder <- file.path(destination_folder, validets$group[i])
  
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder, recursive = TRUE)
  }
  
  # Constructing destination file name
  dest_file <- file.path(dest_folder, paste0(
    validets$conf_class[i], "_", validets$group[i], "_", validets$id[i], "_",
    sub(".WAV$", "", validets$WAV_Name[i]), ".WAV"
  ))
  
  writeWave(myWave, filename = dest_file, extensible=FALSE)
}
