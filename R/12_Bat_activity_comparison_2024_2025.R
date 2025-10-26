library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(readxl)

Sys.setlocale("LC_TIME", "C")

#Loading data
setwd("C:/Users/Willkommen/Documents")
bat_activity_filtered_2024 <- read_excel("Uni_Greifswald/Masterarbeit/bat_activity_filtered_2024.xlsx")
head(bat_activity_filtered_2024)

bat_activity_filtered_2025 <- read_excel("Uni_Greifswald/Masterarbeit/bat_activity_filtered_2025.xlsx")
head(bat_activity_filtered_2025)

# 2025 Without IG_D1 & IG-D3
bat_activity_filtered_2025_2 <- bat_activity_filtered_2025 %>%
  filter(!site %in% c("g1", "g3"))

# 2024 and 2025 same time window (14.04 - 18.07)
bat_activity_filtered_2024$Date <- as.Date(bat_activity_filtered_2024$Date, format = "%Y-%m-%d")

# start and end time
start_date <- as.Date("2024-04-24")
end_date <- as.Date("2024-07-18")

bat_activity_filtered_2024_filtered <- bat_activity_filtered_2024[
  bat_activity_filtered_2024$Date >= start_date & 
    bat_activity_filtered_2024$Date <= end_date, 
]
head(bat_activity_filtered_2024_filtered)

# Adding a column for the year (2024 & 2025)
bat_activity_filtered_2024_filtered <- bat_activity_filtered_2024_filtered %>%
  mutate(Year = "2024")
bat_activity_filtered_2025_2 <- bat_activity_filtered_2025_2 %>%
  mutate(Year = "2025")

# Merging the datasets of the two years
bat_activity_filtered <- bind_rows(bat_activity_filtered_2024_filtered, bat_activity_filtered_2025_2)
bat_activity_filtered$Date <- as.Date(bat_activity_filtered$Date)

bat_activity_daily_corrected <- bat_activity_filtered %>%
  group_by(julian_day, landuse, Year) %>%
  summarise(
    total_activity = sum(Activity_Minutes),
    recorder_count = n(),  # Number of recordings (per recorder that recorded that day)
    .groups = "drop"
  ) %>%
  mutate(
    Activity_Minutes_standardized_landuse = total_activity / recorder_count
  )

# without PV on mineral soil 
bat_activity_daily_corrected_filtered <- bat_activity_daily_corrected %>%
  filter(landuse != "PV on mineral soil")

bat_activity_daily_corrected_filtered <- bat_activity_daily_corrected_filtered %>%
  mutate(
    dummy_date = as.Date(julian_day - 1, origin = "2000-01-01") # julian_day=1 is 2000-01-01, 2 is 2000-01-02 ...
  )

bat_activity_daily_corrected <- bat_activity_daily_corrected %>%
  mutate(
    dummy_date = as.Date(julian_day - 1, origin = "2000-01-01") 
    # julian_day=1 is 2000-01-01, 2 is 2000-01-02 ...
  )

# without PV on mineral soil
ggplot(bat_activity_daily_corrected_filtered, aes(x = dummy_date, y = Activity_Minutes_standardized_landuse, colour = landuse)) + 
  geom_line(aes(group = interaction(landuse, Year), linetype = Year), alpha = 0.4) +
  geom_point(aes(shape = Year), size = 1.5, alpha = 0.6) +
  geom_smooth(aes(linetype = Year), method = "loess", se = TRUE) +
  scale_color_manual(values = c(
    "Intensively used grassland + drained peatland" = "tomato",
    "PV on rewetted peatland" = "steelblue"
  )) +
  scale_shape_manual(values = c("2024" = 16, "2025" = 17)) +
  scale_linetype_manual(values = c("2024" = "solid", "2025" = "dashed")) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2000-03-15"), as.Date("2000-11-15"), by = "1 month")
  ) +
  guides(
    shape = guide_legend(override.aes = list(colour = "grey", size= 2.5 ))
  ) +
  labs(
    x = "Month", 
    y = "Recorded bat activity minutes per night", 
    colour = "Land use", 
    shape = "Year", 
    linetype = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "wrap",     
    legend.key.width = unit(1, "lines"),  
    legend.spacing.x = unit(0.2, "lines"), 
    legend.spacing.y = unit(0.1, "lines")
    )

monthly_activity_mean <- bat_activity_daily_corrected %>%
  mutate(
    Month = month(dummy_date, label = TRUE)
  ) %>%
  group_by(Year, Month, landuse) %>%
  summarise(Total_Activity = mean(Activity_Minutes_standardized_landuse, na.rm = TRUE)) %>%
  ungroup()

#with PV on mineral soil
ggplot(bat_activity_daily_corrected, aes(x = dummy_date, y = Activity_Minutes_standardized_landuse, colour = landuse)) + 
  geom_line(aes(group = interaction(landuse, Year), linetype = Year), alpha = 0.4) +
  geom_point(aes(shape = Year),  alpha = 0.6) +
  geom_smooth(aes(linetype = Year), method = "loess", se = TRUE) +
  scale_color_manual(values = c(
    "Intensively used grassland + drained peatland" = "tomato",
    "PV on rewetted peatland" = "steelblue",
    "PV on mineral soil" = "darkgreen"
  )) +
  scale_shape_manual(values = c("2024" = 16, "2025" = 17)) +
  scale_linetype_manual(values = c("2024" = "solid", "2025" = "dashed")) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2000-03-15"), as.Date("2000-11-15"), by = "1 month")
  )+  # nur Monat als Label anzeigen
  labs(
    x = "Month", 
    y = "Recorded bat activity minutes per night", 
    colour = "Land use", 
    shape = "Year", 
    linetype = "Year"
  ) +
  guides(
    shape = guide_legend(override.aes = list(colour = "grey", size= 2.5 ))
  )+
  theme_minimal() +
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        legend.position = "top",
          legend.direction = "horizontal",
          legend.box = "wrap",     
          legend.key.width = unit(1, "lines"),  
          legend.spacing.x = unit(0.2, "lines"), 
        legend.spacing.y = unit(0.1, "lines"),
          legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(margin = margin(l = 5, r = 10)))

subset_2024 <- subset(bat_activity_daily_corrected, Year == 2024)


#Loading weather data
weather_night_2024 <- read_excel("Uni_Greifswald/Masterarbeit/weather_night_2024.xlsx")
head(weather_night_2024)
weather_night_2025 <- read_excel("Uni_Greifswald/Masterarbeit/weather_night_2025.xlsx")
head(weather_night_2025)

# data format 
weather_night_2024$date <- as.Date(weather_night_2024$date, format = "%Y-%m-%d")
weather_night_2025$date <- as.Date(weather_night_2025$date, format = "%Y-%m-%d")

# start and end for the time window
start_date <- as.Date("2024-04-24")
end_date <- as.Date("2024-07-18")

weather_night_2024 <- weather_night_2024[
  weather_night_2024$date >= start_date & 
    weather_night_2024$date <= end_date, 
]

start_date <- as.Date("2025-04-24")
end_date <- as.Date("2025-07-18")
weather_night_2025 <- weather_night_2025[
  weather_night_2025$date >= start_date & 
    weather_night_2025$date <= end_date, 
]

#Adding a column for the year
weather_night_2024 <- weather_night_2024 %>%
  mutate(Year = "2024")
weather_night_2025 <- weather_night_2025 %>%
  mutate(Year = "2025")

# Combing the two years
bat_weather_filtered <- bind_rows(weather_night_2024, weather_night_2025)

weather_night_monthly <- bat_weather_filtered %>%
  mutate(
    date = as.Date(date),
    hour = hour(date),
    month = floor_date(date, unit = "month")  # erzeugt z.B. 2025-10-01
  ) %>%
  filter(hour %in% c(0:5, 18:23)) %>%
  group_by(month) %>%
  summarise(
    Total_night_rainfall = sum(`Rain_fall_night`, na.rm = TRUE),
    Mean_night_temperature = mean(Temperature_mean_night, na.rm = TRUE)
  )

#Adding julian date
bat_weather_filtered <- bat_weather_filtered %>%
  mutate(
    julian_day = yday(date) 
  )

# Dummy-Date
bat_weather_filtered <- bat_weather_filtered %>%
  mutate(
    dummy_date = as.Date(julian_day - 1, origin = "2000-01-01")
  )

ggplot(bat_weather_filtered, aes(x = dummy_date, y = Rain_fall_night, fill = Year)) +
  geom_col(position = "dodge") +
  labs(
    x = "Month",
    y = "Rainfall during night (mm)",
    fill = "Year"
  ) +
  scale_fill_manual(values = c("2024" = "lightblue", "2025" = "darkblue")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal()

ggplot(bat_weather_filtered, aes(x = dummy_date, y = Temperature_mean_night, colour = Year)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("2024" = "#D59E00", "2025" = "#D55E50")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(
    x = "Month",
    y = "Mean temperature (°C)",
    colour = "Year"
  ) +
  theme_minimal()

# Temperature and precipitation
ggplot(bat_weather_filtered, aes(x = dummy_date)) +
  geom_col(aes(y = Rain_fall_night, fill = Year), position = "dodge") +
  geom_line(aes(y = Temperature_mean_night , colour = Year), alpha = 0.5) +
  geom_point(aes(y = Temperature_mean_night , colour = Year), alpha = 0.6) +
  geom_smooth(aes(y = Temperature_mean_night , colour = Year), method = "loess", se = FALSE) +
  scale_y_continuous(
    name = "Total nighttime precipitation (mm)",
    sec.axis = sec_axis(~., name = "Mean nighttime temperature (°C)")
  ) +
  scale_fill_manual(values = c("2024" = "lightblue", "2025" = "darkblue")) +
  scale_color_manual(values = c("2024" = "#D59E00", "2025" = "#D55E50")) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2000-03-15"), as.Date("2000-11-15"), by = "1 month")
  ) +
  labs(
    x = "Month",
    fill = "Year",
    colour = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(color = "darkblue", size = 11),
    axis.title.y.right = element_text(color = "#D55E50", size = 11),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10))
  )

