####getting the audio files of the species groups without threshold
library(data.table)
library(dplyr)
library(tuneR)
library(openxlsx)
library(readr)
library(ggplot2)
library(lubridate)
library(ggtext)



setwd("C:/Users/Willkommen/Documents")
###2024
audio_data_2024 <- read_delim("~/Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6/all_0.66_batdetect2_highest_det_prob_threshold.csv", 
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
audio_data_2025 <- read_delim("~/Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6_2025/all_0.66_batdetect2_highest_det_prob_threshold_2025.csv", 
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


# Für 2024
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

# Prüfen, ob es Arten gibt, die nicht in den Gruppen definiert sind
audio_data_2024 %>%
  filter(species_group == "Other") %>%
  distinct(class)


# Für 2025
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

# Prüfen, ob es Arten gibt, die nicht in den Gruppen definiert sind
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

#####Audio Dateien für 2025 für die manuelle Bestimmung 
# 2. Filtern nur die beiden gewünschten Gruppen
subset_2025 <- audio_data_2025 %>%
  filter(species_group %in% c("Barbastellus_barbastellus", "Plecotus_species"))
subset_2024 <- audio_data_2024  %>%
  filter(species_group %in% c("Barbastellus_barbastellus", "Plecotus_species", "Rhinolophus_hipposideros"))

setwd("~/Uni_Greifswald/Masterarbeit")

# danach speicherst du einfach:
#write.xlsx(subset_2025, "subset_2025.xlsx")
#write.xlsx(subset_2024, "subset_2024.xlsx")



# 3. Zielordner definieren
destination_folder <- "C:/Users/Willkommen/Documents/Uni_Greifswald/Masterarbeit/Species_Files_2025"

# Ordner für Gruppen erstellen, falls nicht vorhanden
unique_groups <- unique(subset_2025$species_group)
for (g in unique_groups) {
  dir.create(file.path(destination_folder, g), showWarnings = FALSE, recursive = TRUE)
}

# Audio-Dateien kopieren
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


####thresholds
audio_data_2024$class_prob <- as.numeric(gsub(",", ".", audio_data_2024$class_prob))
audio_data_2025$class_prob <- as.numeric(gsub(",", ".", audio_data_2025$class_prob))


# Funktion zum Filtern mit Thresholds
apply_thresholds <- function(data) {
  data %>%
    filter(  # Filter mit individuellen Thresholds je nach species_group
      (species_group == "Myotis_species" & class_prob >= 0.01) |
        (species_group == "Pipistrellus_species" & class_prob >= 0.37) |
        (species_group == "Nyctaloid_group" & class_prob >= 0.38)
    )
}

# Gefilterte Datensätze mit gesetzten Thresholds
audio_data_2024_filtered <- apply_thresholds(audio_data_2024)
audio_data_2025_filtered <- apply_thresholds(audio_data_2025)

#2024
# boxplots species groups on x-axis
minutes_per_night_2024 <- audio_data_2024_filtered %>%
  filter(landuse %in% c("PV on rewetted peatland", 
                        "Intensively used grassland + drained peatland", 
                        "PV on mineral soil"),
         species_group != "Plecotus_species") %>%
  group_by(Date, landuse, species_group) %>%
  summarise(minutes_count = n(), .groups = "drop")

# Farben für Linien (Landnutzung)
landuse_colors <- c(
  "PV on rewetted peatland" = "steelblue",
  "Intensively used grassland + drained peatland" = "tomato",
  "PV on mineral soil" = "darkgreen"
)

#"Myotis_species" = expression(italic(Myotis)~species),
#"Nyctaloid_group" = "Nyctaloid group",
#"Pipistrellus_species" = expression(italic(Pipistrellus)~species)

# Boxplot mit farbigen Linien (kein Füll-Farbton)
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

# 2025 boxplot
minutes_per_night_2025 <- audio_data_2025_filtered %>%
  filter(landuse %in% c("PV on rewetted peatland", 
                        "Intensively used grassland + drained peatland", 
                        "PV on mineral soil"),
         species_group != "Plecotus_species") %>%
  group_by(Date, landuse, species_group) %>%
  summarise(minutes_count = n(), .groups = "drop")

# Farben für Linien (Landnutzung)
landuse_colors <- c(
  "PV on rewetted peatland" = "steelblue",
  "Intensively used grassland + drained peatland" = "tomato",
  "PV on mineral soil" = "darkgreen"
)

# Boxplot mit farbigen Linien (kein Füll-Farbton)
ggplot(minutes_per_night_2025, aes(x = species_group, y = minutes_count, color = landuse)) +
  geom_boxplot(position = position_dodge(width = 0.8), fill = NA) +
  scale_color_manual(values = landuse_colors) +
  scale_x_discrete(labels = c(
    "Myotis_species" = "Myotis species",
    "Nyctaloid_group" = "Nyctaloid group",
    "Pipistrellus_species" = "Pipistrellus species"
  ))+
  labs(
    x = "Species group",
    y = "Recorded bat activity minutes per night",
    color = "Land use"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


######ohne IG-D23 und IG-D25
# Vorverarbeitung: Filterung & Gruppierung
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

# Farben für Linien
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


# Anzahl der verbleibenden Klassen pro Jahr
audio_data_2024_filtered %>%
  group_by(species_group) %>%
  summarise(count = n())


# Anzahl der verbleibenden Klassen pro Jahr
audio_data_2025_filtered %>%
  group_by(species_group) %>%
  summarise(count = n())



# Für 2024
distribution_2024 <- audio_data_2024_filtered %>%
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))

# Für 2025
distribution_2025 <- audio_data_2025_filtered %>%
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))

distribution_2025_filtered <- audio_data_2025_filtered %>%
  filter(!site %in% c("g1", "g3")) %>%  # Entfernt die Kategorien g1 und g3 aus der Spalte site
  group_by(landuse, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(landuse, desc(count))


print(distribution_2024)
print(distribution_2025)
print(distribution_2025_filtered)

# Balkendiagramm für 2024
distribution_2024_filtered <- distribution_2024 %>%
  filter(species_group != "Plecotus_species")

# with the numbers on top of the bars 
ggplot(distribution_2024, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +  
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",      
    "Nyctaloid_group" = "#ADD8E6",      
    "Pipistrellus_species" = "#FFc911"  
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()

# no numbers on top of the graphs
ggplot(distribution_2024, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       
    "Nyctaloid_group" = "#ADD8E6",    
    "Pipistrellus_species" = "#FFc911" 
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()

activity_per_site_2024 <- audio_data_2024_filtered %>%
  group_by(landuse, site_abbr, site, species_group) %>%
  summarise(recorded_minutes = n(), .groups = "drop")

# Balkendiagramm für 2025
# no numbers on top of the graphs
ggplot(distribution_2025, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       
    "Nyctaloid_group" = "#ADD8E6",      
    "Pipistrellus_species" = "#FFc911" 
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()

# with the numbers on top of the bars 
ggplot(distribution_2024, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +  
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       
    "Nyctaloid_group" = "#ADD8E6",      
    "Pipistrellus_species" = "#FFc911"  
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()


activity_per_site_2025 <- audio_data_2025 %>%
  group_by(landuse, site_abbr, site, species_group) %>%
  summarise(recorded_minutes = n(), .groups = "drop")

# 2025 ohne g1 und g3
ggplot(distribution_2025_filtered, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       
    "Nyctaloid_group" = "#ADD8E6",     
    "Pipistrellus_species" = "#FFc911" 
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()

ggplot(distribution_2025_filtered, aes(x = landuse, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       
    "Nyctaloid_group" = "#ADD8E6",     
    "Pipistrellus_species" = "#FFc911" 
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()


# 2025 nur mit g1 und g3
# Nur g1 und g3 aus 2025
audio_data_2025_filtered_3 <- audio_data_2025_filtered %>%
  filter(site %in% c("g1", "g3")) %>%
  mutate(site_group = "Intensively used grassland")

distribution_2025_3 <- audio_data_2025_filtered_3 %>%
  group_by(site_group, species_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

ggplot(distribution_2025_3, aes(x = site_group, y = count, fill = species_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(
    "Myotis_species" = "#FFA07A",       # hellrot
    "Nyctaloid_group" = "#ADD8E6",      # hellblau
    "Pipistrellus_species" = "#FFc911" # dunkelgelb
  )) +
  labs(x = "Land use", y = "Recorded minutes of bat activity") +
  theme_minimal()



# Gehölze sind für Pipistrellus-Arten sehr wichtig!
# nutzen Bäume, Hecken, Sträucher oder Baumreihen vor allem als
## Jagdbahn und Orientierungshilfen
## Verstecke und Schutz während der Jagd
## Ruhestätten oder Quartiere (z. B. in Baumhöhlen oder Rindenspalten)
# Ohne Gehölzstrukturen fehlt ihnen oft die nötige Struktur, um effizient zu jagen und sich sicher zu bewegen.
# Pipistrellus-Arten brauchen Gehölze als wichtige Lebensraum-Komponente — auch wenn sie nicht ausschließlich in dichten Wäldern vorkommen.
# Standorte mit Gehölzen: dort vermutlich auch mehr Pipistrellus-Rufe zu erwarten als auf komplett offenen, baumfreien Flächen.


library(ggplot2)
library(ggtext)

df <- data.frame(
  x = c("Myotis", "Pipistrellus"),
  y = c(5, 7)
)

ggplot(df, aes(x = x, y = y)) +
  geom_col() +
  scale_x_discrete(labels = c("Myotis" = "*Myotis* species", "Pipistrellus" = "*Pipistrellus* species")) +
  theme_minimal() +
  theme(axis.text.x = element_markdown(size = 12))

library(ggplot2)
library(ggtext)

df <- data.frame(
  x = c("Myotis", "Pipistrellus"),
  y = c(5, 7)
)

ggplot(df, aes(x = x, y = y)) +
  geom_col() +
  scale_x_discrete(labels = c("Myotis" = "*Myotis* species", "Pipistrellus" = "*Pipistrellus* species")) +
  theme_minimal() +
  theme(axis.text.x = ggtext::element_markdown(size = 12))

