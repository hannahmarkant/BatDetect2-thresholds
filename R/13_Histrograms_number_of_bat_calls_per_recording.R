library(dplyr)
library(ggplot2)
library(readr)

batdetect2_2024 <- read_delim("Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6/all_0.6_batdetect2.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

batdetect2_2025 <- read_delim("Uni_Greifswald/Masterarbeit/Batdetect2_Analyse_0.6_2025/all_0.6_batdetect2_2025.csv", 
                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

batdetect2_2024 <- batdetect2_2024 %>% mutate(year = 2024)
batdetect2_2025 <- batdetect2_2025 %>% mutate(year = 2025)

batdetect_all <- bind_rows(batdetect2_2024, batdetect2_2025)

batdetect_all <- batdetect_all %>%
  mutate(det_prob = as.numeric(gsub(",", ".", det_prob)))

# det_prob >= 0.66 
batdetect_filtered <- batdetect_all %>%
  filter(det_prob >= 0.66)

# Number of calls per WAV_Name and Recording_Time
calls_per_recording <- batdetect_filtered %>%
  group_by(WAV_Name, Recording_Time, id, year) %>%
  summarise(n_calls = n(), .groups = "drop")

calls_per_recording <- calls_per_recording %>%
  mutate(site = sub("_.*", "", id))

calls_per_recording <- calls_per_recording %>%
  mutate(landuse = case_when(
    site %in% c("g1", "g3", "g7", "g11", "099", "095") ~ "Intensively used grassland + drained peatland",
    site %in% c("pv1", "pv2", "pv3", "pv5", "pv9", "pv13", "pv15", "111", "105") ~ "PV on rewetted peatland",
    site %in% c("091", "004", "102", "115", "wm2", "059") ~ "PV on mineral soil",
    TRUE ~ "Other"
  ))


# Histogram: Distribution of calls per land use
# 2024
ggplot(filter(calls_per_recording, year == 2024), 
       aes(x = n_calls, fill = landuse)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "Intensively used grassland + drained peatland" = "steelblue",
    "PV on rewetted peatland" = "tomato"
  )) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    x = "Number of calls per recording",
    y = "Number of recorded minutes",
    fill = "Land use"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
    
  )


# 2025 without IG-D23 and IG-D25
calls_per_recording$landuse <- factor(
  calls_per_recording$landuse,
  levels = c(
    "Intensively used grassland + drained peatland",
    "PV on rewetted peatland",
    "PV on mineral soil"
  )
)

ggplot(
  filter(calls_per_recording, year == 2025 & !site %in% c("g1", "g3")),
  aes(x = n_calls, fill = landuse)
) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  scale_fill_manual(values = c(
    "Intensively used grassland + drained peatland" = "steelblue",
    "PV on rewetted peatland" = "tomato",
    "PV on mineral soil" = "darkgreen"
  )) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(
    x = "Number of calls per recording",
    y = "Number of recorded minutes",
    fill = "Land use"
  ) +
  theme_minimal()+
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10) )


table_for_plot_2025 <- calls_per_recording %>%
  filter(year == 2025, !site %in% c("g1", "g3")) %>%
  group_by(n_calls, landuse) %>%
  summarise(
    recorded_minutes = n(),   
    .groups = "drop"
  ) %>%
  arrange(landuse, n_calls)

table_for_plot_2024 <- calls_per_recording %>%
  filter(year == 2024) %>%
  group_by(n_calls, landuse) %>%
  summarise(
    recorded_minutes = n(),  
    .groups = "drop"
  ) %>%
  arrange(landuse, n_calls)


recordings_over_60 <- calls_per_recording %>%
  filter(n_calls > 60, !(year == 2025 & site %in% c("g1", "g3"))) %>%
  group_by(year, landuse) %>%
  summarise(
    recordings_over_60_calls = n(),
    .groups = "drop"
  ) %>%
  arrange(year, landuse)

recordings_over_60

recorded_minutes_summary <- calls_per_recording %>%
  filter(!(year == 2025 & site %in% c("g1", "g3"))) %>%
  group_by(year, landuse) %>%
  summarise(
    recorded_minutes = n(),  
    .groups = "drop"
  ) %>%
  arrange(year, landuse)

recorded_minutes_summary