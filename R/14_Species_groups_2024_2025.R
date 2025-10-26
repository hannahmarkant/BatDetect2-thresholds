#############################################################################################################
# Applying species group–specific thresholds to the defined species groups and visualizing the results 
# (recorded minutes of bat activity per night across different land-use types).
# Furthermore, the audio files of species groups for which no threshold could be determined were saved 
# in separate folders for human validation.
#############################################################################################################

library(data.table)
library(dplyr)
library(tuneR)
library(openxlsx)
library(readr)
library(ggplot2)
library(lubridate)
library(ggtext)

setwd("./")

###2024
audio_data_2024 <- read_delim("~/Batdetect2_Analyse_0.6/all_0.66_batdetect2_highest_det_prob_threshold.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
audio_data_2024$Date <- as.Date(audio_data_2024$Date, format = "%Y-%m-%d")
# adding a "landuse" column
audio_data_2024 <- audio_data_2024 %>%
  mutate(landuse = case_when(
    site %in% c("g1","g3") ~ "Intensively used grassland + drained peatland",
    site %in% c("pv1","pv2","pv3") ~ "PV on rewetted peatland",
    TRUE ~ NA_character_
  ),
  site_abbr = case_when(
    site %in% c("g1") ~ "IG-D1",
    site %in% c("g3") ~ "IG-D3",
    
    site %in% c("pv1") ~ "PV-RW1",
    site %in% c("pv2") ~ "PV-RW2",
    site %in% c("pv3") ~ "PV-RW3",
    
    TRUE ~ NA_character_
  ))

#2025
audio_data_2025 <- read_delim("~/Batdetect2_Analyse_0.6_2025/all_0.66_batdetect2_highest_det_prob_threshold_2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
audio_data_2025$Date <- as.Date(audio_data_2025$Date, format = "%Y-%m-%d")
audio_data_2025 <- audio_data_2025 %>%
  filter(Date >= as.Date("2025-04-24"))

# adding a "landuse" column
audio_data_2025 <- audio_data_2025 %>%
  mutate(landuse = case_when(
    site %in% c("g1","g3","g7","g11","099","095") ~ "Intensively used grassland + drained peatland",
    site %in% c("pv5","pv9","pv13","pv15","111","105") ~ "PV on rewetted peatland",
    site %in% c("091","004","102","115","wm2","059") ~ "PV on mineral soil",
    TRUE ~ NA_character_
  ),
  site_abbr = case_when(
    site %in% c("g1") ~ "IG-D25",
    site %in% c("g3") ~ "IG-D23",
    site %in% c("g7") ~ "IG-D7",
    site %in% c("g11") ~ "IG-D3",
    site %in% c("099") ~ "IG-D17",
    site %in% c("095") ~ "IG-D11",
    
    site %in% c("pv5") ~ "PV-RW1",
    site %in% c("pv9") ~ "PV-RW3",
    site %in% c("pv13") ~ "PV-RW13",
    site %in% c("pv15") ~ "PV-RW15",
    site %in% c("111") ~ "PV-RW21",
    site %in% c("105") ~ "PV-RW19",
    
    site %in% c("091") ~ "PV-M1",
    site %in% c("004") ~ "PV-M2",
    site %in% c("102") ~ "PV-M3",
    site %in% c("115") ~ "PV-M4",
    site %in% c("wm2") ~ "PV-M5",
    site %in% c("059") ~ "PV-M6",
    
    TRUE ~ NA_character_
  ))

# 2024
audio_data_2024 %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


audio_data_2024 <- audio_data_2024 %>%
  mutate(species_group = case_when(
    class %in% c("Myotis bechsteinii", "Myotis brandtii", "Myotis daubentonii", "Myotis nattereri") ~ "Myotis_species",
    class %in% c("Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus") ~ "Pipistrellus_species",
    class == "Barbastellus barbastellus" ~ "Barbastellus_barbastellus",
    class %in% c("Nyctalus noctula", "Nyctalus leisleri", "Eptesicus serotinus", "Vespertilio murinus") ~ "Nyctaloid_group",
    class %in% c("Plecotus auritus", "Plecotus austriacus") ~ "Plecotus_species",
    class == "Rhinolophus hipposideros" ~ "Rhinolophus_hipposideros",
    TRUE ~ "Other"
  ))

# Check for species that are not included in the defined species groups
audio_data_2024 %>%
  filter(species_group == "Other") %>%
  distinct(class)

# 2025
audio_data_2025 %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


audio_data_2025 <- audio_data_2025 %>%
  mutate(species_group = case_when(
    class %in% c("Myotis bechsteinii", "Myotis brandtii", "Myotis daubentonii", "Myotis nattereri") ~ "Myotis_species",
    class %in% c("Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus") ~ "Pipistrellus_species",
    class == "Barbastellus barbastellus" ~ "Barbastellus_barbastellus",
    class %in% c("Nyctalus noctula", "Nyctalus leisleri", "Eptesicus serotinus", "Vespertilio murinus") ~ "Nyctaloid_group",
    class %in% c("Plecotus auritus", "Plecotus austriacus") ~ "Plecotus_species",
    class == "Rhinolophus hipposideros" ~ "Rhinolophus_hipposideros",
    TRUE ~ "Other"
  ))

# Check for species that are not included in the defined species groups
audio_data_2025 %>%
  filter(species_group == "Other") %>%
  distinct(class)

# Für 2024
audio_data_2024 %>%
  group_by(species_group) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Für 2025
audio_data_2025 %>%
  group_by(species_group) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

##################################################################
# Saving audio files for human validation 
# Filter species groups for which no threshold could be determined 
subset_2025 <- audio_data_2025 %>%
  filter(species_group %in% c("Barbastellus_barbastellus", "Plecotus_species"))
subset_2024 <- audio_data_2024  %>%
  filter(species_group %in% c("Barbastellus_barbastellus", "Plecotus_species", "Rhinolophus_hipposideros"))

setwd("./")

# safe xlsx files
#write.xlsx(subset_2025, "subset_2025.xlsx")
#write.xlsx(subset_2024, "subset_2024.xlsx")

# destination folder
destination_folder <- "C:/Users/Willkommen/Documents/Uni_Greifswald/Masterarbeit/Species_Files_2025"

# folders for the species groups (if they do not exist)
unique_groups <- unique(subset_2025$species_group)
for (g in unique_groups) {
  dir.create(file.path(destination_folder, g), showWarnings = FALSE, recursive = TRUE)
}

# Copy audio files
for (i in 1:nrow(subset_2025)) {
  source_file <- subset_2025$WAV_Path[i]
  
  if (!file.exists(source_file)) {
    warning(paste("Datei nicht gefunden:", source_file))
    next
  }
  
  dest_file <- file.path(
    destination_folder,
    subset_2025$species_group[i],
    paste0(subset_2025$id[i], "_", tools::file_path_sans_ext(subset_2025$WAV_Name[i]), ".WAV")
  )
  
  file.copy(source_file, dest_file, overwrite = TRUE)
  cat("Kopiert:", dest_file, "\n")
}
####################################################################################

# apply species-group specific thresholds (class_prob) to the detections
audio_data_2024$class_prob <- as.numeric(gsub(",", ".", audio_data_2024$class_prob))
audio_data_2025$class_prob <- as.numeric(gsub(",", ".", audio_data_2025$class_prob))

apply_thresholds <- function(data) {
  data %>%
    filter(  # Filter mit individuellen Thresholds je nach species_group
      (species_group == "Myotis_species" & class_prob >= 0.01) |
        (species_group == "Pipistrellus_species" & class_prob >= 0.37) |
        (species_group == "Nyctaloid_group" & class_prob >= 0.38)
    )
}

audio_data_2024_filtered <- apply_thresholds(audio_data_2024)
audio_data_2025_filtered <- apply_thresholds(audio_data_2025)

#2024
minutes_per_night_2024 <- audio_data_2024_filtered %>%
  filter(landuse %in% c("PV on rewetted peatland", 
                        "Intensively used grassland + drained peatland", 
                        "PV on mineral soil"),
         species_group != "Plecotus_species") %>%
  group_by(Date, landuse, species_group) %>%
  summarise(minutes_count = n(), .groups = "drop")

# different colors (for each land use)
landuse_colors <- c(
  "PV on rewetted peatland" = "steelblue",
  "Intensively used grassland + drained peatland" = "tomato",
  "PV on mineral soil" = "darkgreen"
)

# Boxplot
p_2024 <- ggplot(minutes_per_night_2024, aes(x = species_group, y = minutes_count, color = landuse)) +
  geom_boxplot(position = position_dodge(width = 0.8), fill = NA) +
  scale_color_manual(values = landuse_colors) +
  scale_x_discrete(labels = c(
    "Myotis_species" = expression(italic("Myotis")~"species"),  # Kursiv
    "Nyctaloid_group" = "Nyctaloid group",  # Normale Schrift
    "Pipistrellus_species" = expression(italic("Pipistrellus")~"species")  # Kursiv
  )) +
  labs(
    x = "Species group",
    y = "Recorded bat activity minutes per night",
    color = "Land use"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11)
  )
print(p_2024)


summary_stats <- minutes_per_night_2024 %>%
  group_by(landuse, species_group) %>%
  summarise(
    mean = mean(minutes_count),
    median = median(minutes_count),
    IQR = IQR(minutes_count),
    n = n(),
    .groups = "drop"
  )
print(summary_stats)

# 2025 (without IG-D23 and IG-D25)
minutes_per_night_2025_filtered <- audio_data_2025_filtered %>%
  filter(
    landuse %in% c(
      "PV on rewetted peatland", 
      "Intensively used grassland + drained peatland", 
      "PV on mineral soil"
    ),
    species_group != "Plecotus_species",
    !site_abbr %in% c("IG-D23", "IG-D25")  # Diese Sites entfernen
  ) %>%
  group_by(Date, landuse, species_group) %>%
  summarise(minutes_count = n(), .groups = "drop") %>%
  # Faktor-Level für landuse festlegen (Reihenfolge)
  mutate(landuse = factor(landuse, levels = c(
    "Intensively used grassland + drained peatland",
    "PV on rewetted peatland",
    "PV on mineral soil"
  )))

# define colors for different land use
landuse_colors <- c(
  "Intensively used grassland + drained peatland" = "tomato",
  "PV on rewetted peatland" = "steelblue",
  "PV on mineral soil" = "darkgreen"
)

# Plot
p_2025 <- ggplot(minutes_per_night_2025_filtered, aes(x = species_group, y = minutes_count, color = landuse)) +
  geom_boxplot(position = position_dodge(width = 0.8), fill = NA) +
  scale_color_manual(values = landuse_colors) +
  scale_x_discrete(labels = c(
    "Myotis_species" = expression(italic("Myotis")~"species"),
    "Nyctaloid_group" = "Nyctaloid group",  # Normale Schrift
    "Pipistrellus_species" = expression(italic("Pipistrellus")~"species") 
  )) + 
  labs(
    x = "Species group",
    y = "Recorded bat activity minutes per night",
    color = "Land use"
  ) +
  theme_minimal() +  # Verwende theme_minimal()
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
    axis.text.x = element_text(size = 10),  # Achsenticks auf 10
    axis.text.y = element_text(size = 10),  # Y-Achsenticks auf 10
    axis.title.x = element_text(size = 11),  # X-Achsentitel auf 11
    axis.title.y = element_text(size = 11)  # Y-Achsentitel auf 11
  )

# Plot anzeigen
print(p_2025)


summary_stats <- minutes_per_night_2025_filtered %>%
  group_by(landuse, species_group) %>%
  summarise(
    mean = mean(minutes_count),
    median = median(minutes_count),
    IQR = IQR(minutes_count),
    n = n(),
    .groups = "drop"
  )

print(summary_stats)

# Zwei gewünschten Zeilen aus dem Original-Dataset herausziehen
extra_rows <- audio_data_2024 %>%
  filter(
    (id == "g3_mj" & WAV_Name == "20240509_204000T.WAV") |
      (id == "pv1_so" & WAV_Name == "20240908_221500T.WAV")
  )

# Mit dem gefilterten Dataset kombinieren
audio_data_2024_filtered <- bind_rows(audio_data_2024_filtered, extra_rows)


audio_data_2024_filtered <- audio_data_2024_filtered %>%
  # sicherstellen, dass Date als Datum erkannt wird
  mutate(
    monthinfo = format(Date, "%Y-%m"),   # Jahr-Monat extrahieren, z.B. "2024-06"
    site_monthinfo = paste(site_abbr, monthinfo, sep = "_") # Kombination aus site + monthinfo
  )

audio_data_2025_filtered <- audio_data_2025_filtered %>%
  # sicherstellen, dass Date als Datum erkannt wird
  mutate( 
    monthinfo = format(Date, "%Y-%m"),  
    site_monthinfo = paste(site_abbr, monthinfo, sep = "_") # Kombination aus site + monthinfo
  )

#setwd("~/Uni_Greifswald/Masterarbeit")

#write.csv(audio_data_2024_filtered, "audio_data_2024_filtered.csv", row.names = FALSE)
#write.csv(audio_data_2025_filtered, "audio_data_2025_filtered.csv", row.names = FALSE)

#write.xlsx(audio_data_2024_filtered, "audio_data_2024_filtered.xlsx")
#write.xlsx(audio_data_2025_filtered, "audio_data_2025_filtered.xlsx")


# Number of recorded minutes per species group
# 2024
audio_data_2024_filtered %>%
  group_by(species_group) %>%
  summarise(count = n())

# 2025
audio_data_2025_filtered %>%
  group_by(species_group) %>%
  summarise(count = n())

# 2024 distribution per species group and land use
distribution_2024 <- audio_data_2024_filtered %>%
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))

# 2025 distribution per species group and land use (with IG-D23 and IG-D25)
distribution_2025 <- audio_data_2025_filtered %>%
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))

# 2025 distribution per species group and land use (without IG-D23 and IG-D25)
distribution_2025_filtered <- audio_data_2025_filtered %>%
  filter(!site %in% c("g1", "g3")) %>%  # IG-D23 and IG-D25
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))

print(distribution_2024)
print(distribution_2025)
print(distribution_2025_filtered)
