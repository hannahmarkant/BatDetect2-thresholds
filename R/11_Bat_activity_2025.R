################################################################################
# An analysis of bat activity recorded with AudioMoth devices in 2025 was performed.
# The workflow includes identifying missing or incomplete recording days,
# importing and aggregating bat activity data and integrating weather data (total 
# night-time precipitation and mean night-time temperature). The data is visualized 
# over time, per site and per land-use type.Generalized Linear Mixed Models were 
# fitted with step wise removal of non-significant terms.
#################################################################################

library(readxl)
library(readr)
library(openxlsx)
library(writexl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(glmmTMB)
library(performance)
library(ggeffects)
library(stringr)

# Setting the working directory
setwd("./")

# Loading 'missing' table form the trackingsheet
missing_2025 <- read.xlsx("~/Audio_Moth_Tracking_Sheet_Hannah_2025.xlsx", 
                          sheet = "missing_data") %>%
  mutate(
    date_last = as.Date(as.numeric(date_last), origin = "1899-12-30"),
    data_collection_date = as.Date(as.numeric(data_collection_date), origin = "1899-12-30"),
    date_first = as.Date(as.numeric(date_first), origin = "1899-12-30"),
    install_date = as.Date(install_date, origin = "1899-12-30")
  )

# Adjusting the naming of the recorder ID (site)
skip_sites <- c("g3", "g7", "g1")
missing_2025 <- missing_2025 %>%
  mutate(site = if_else(!site %in% skip_sites,
                        str_pad(as.character(site), width = 3, pad = "0"),
                        site))

# Create a row for each day with missing recordings before collectiing the recorders (data_collection_date)
missing_data_2025 <- missing_2025 %>%
  rowwise() %>%
  mutate(
    # only when date_last != data_collection_date
    Date = list(if(date_last != data_collection_date) seq(date_last, data_collection_date, by = "day") else NULL)
  ) %>%
  unnest(Date) %>%
  dplyr::select(site, Date)

# Adding 24.04.2025 and 18.07.2025 for each site (incomplete days as they are the start and end date of the sampling period and are not fully recorded days)
extra_dates <- missing_2025 %>%
  dplyr::select(site) %>%
  distinct() %>%
  mutate(Date = as.Date("2025-04-24")) %>%
  bind_rows(missing_2025 %>% dplyr::select(site) %>% distinct() %>% mutate(Date = as.Date("2025-07-18")))

# Combining all missing dates and removing the duplicates
missing_data_final_2025 <- bind_rows(missing_data_2025, extra_dates) %>%
  distinct(site, Date) %>%
  arrange(site, Date)

# Loading the bat activity data
audio_data <- read_delim("~/Batdetect2_Analyse_0.6_2025/all_0.66_batdetect2_highest_det_prob_threshold_2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
audio_data$Date <- as.Date(audio_data$Date, format = "%Y-%m-%d")
audio_data <- audio_data %>%
  filter(Date >= as.Date("2025-04-24"))

# Calculating the activity minutes per day (00:00-6:00 & 18:00-24:00)
activity_per_day <- audio_data %>%
  group_by(site, Date) %>%
  summarize(Activity_Minutes = n_distinct(Recording_Time), .groups = "drop")

# Generating a complete list of all calender days for each recorder
recording_info <- missing_2025 %>%
  rowwise() %>%
  mutate(Date = list(seq(date_first, data_collection_date, by = "day"))) %>%
  unnest(Date) %>%
  mutate(recording_status = ifelse(Date <= date_last, "Yes", "No")) %>%
  dplyr::select(site, Date, recording_status)

# avoid duplicates
recording_info<- recording_info %>%
  distinct(site, Date, .keep_all = TRUE)

# Merge: all days with activity data and status
final_data <- recording_info %>%
  left_join(activity_per_day, by = c("site", "Date")) %>%
  mutate(
    Activity_Minutes = ifelse(is.na(Activity_Minutes), 0, Activity_Minutes),
    Activity_Minutes = ifelse(recording_status == "No", NA, Activity_Minutes),
    site = factor(site, levels = c("004", "059", "091", "095", "099", "102", "105", "111", "115", "g1", "g3", "g7", "g11", "pv5", "pv9", "pv13", "pv15", "wm2"))
  ) %>%
  arrange(site, Date)

# Setting incomplete days to NA
final_data <- final_data %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(
    missing_data_final_2025 %>% mutate(Date = as.Date(Date), missing_flag = TRUE),
    by = c("site", "Date")
  ) %>%
  mutate(
    Activity_Minutes = ifelse(!is.na(missing_flag) & missing_flag, NA, Activity_Minutes)
    ) %>%
  dplyr::select(-missing_flag)

# Calculate number of unique recording days per recorder
# Days with recording issues are excluded
record_days <- final_data %>%
  anti_join(missing_data_final_2025, by = c("site", "Date")) %>%
  count(site, name = "days_total")

# adding weather data
Lottorf_Weather_2025 <- read_excel("~/Lottorf_Wetterdaten_April-Juli_2025.xlsx")

# hourly temperature Schleswig
data_OBS_DEU_PT1H_T2M_4466 <- read_csv("~/Deutscher Wetterdienst/data_OBS_DEU_PT1H_T2M_4466.csv")
head(data_OBS_DEU_PT1H_T2M_4466)

# hourly precipitation Schleswig
data_OBS_DEU_PT1H_RR_4466 <- read_csv("~/Deutscher Wetterdienst/data_OBS_DEU_PT1H_RR_4466.csv")
head(data_OBS_DEU_PT1H_RR_4466)
Sys.setlocale("LC_TIME", "C")

start_date <- as.Date("2025-04-24") # start of the recording period
end_date <- as.Date("2025-07-18") # end of the recording period

weather_night <- Lottorf_Weather_2025 %>%
  mutate(
    date = as.Date(dateLocale),
    hour = hour(dateLocale)
  ) %>%
  filter(date >= start_date & date <= end_date)

temp_dwd <- data_OBS_DEU_PT1H_T2M_4466 %>%
  mutate(
    date = as.Date(Zeitstempel),
    hour = hour(Zeitstempel)
  ) %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date, hour, Temperature_station = Wert)

rain_dwd <- data_OBS_DEU_PT1H_RR_4466 %>%
  mutate(
    date = as.Date(Zeitstempel),
    hour = hour(Zeitstempel)
  ) %>%
  filter(date >= start_date & date <= end_date) %>%
  select(date, hour, Rain_station = Wert)

weather_filled <- weather_night %>%
  left_join(temp_dwd, by = c("date", "hour")) %>%
  left_join(rain_dwd, by = c("date", "hour")) %>%
  mutate(
    Temperature_final = if_else(is.na(Temperature), Temperature_station, Temperature),
    `Rain fall_final` = if_else(is.na(`Rain fall`), Rain_station, `Rain fall`)
  )

weather_night_filtered <- weather_filled %>%
  mutate(date = as.Date(dateLocale)) %>%
  filter(hour(dateLocale) %in% c(0:5, 18:23)) %>%
  group_by(date) %>%
  summarise(
    Rain_fall_night = sum(`Rain fall_final`, na.rm = TRUE),
    Temperature_mean_night = mean(Temperature_final, na.rm = TRUE)
  )

# Calculating monthly mean temperature and total precipitation
weather_night_monthly_2025 <- weather_filled %>%
  mutate(
    date = as.Date(dateLocale),
    hour = hour(dateLocale),
    month = floor_date(date, unit = "month")  # erzeugt z.B. 2025-10-01
  ) %>%
  filter(hour %in% c(0:5, 18:23)) %>%
  group_by(month) %>%
  summarise(
    Total_night_rainfall = sum(`Rain fall_final`, na.rm = TRUE),
    Mean_night_temperature = mean(Temperature_final, na.rm = TRUE)
  )

# write_xlsx(weather_night_filtered, "~/weather_night_2025.xlsx")

# Plotting precipitation                
ggplot(weather_night_filtered, aes(x = date, y = Rain_fall_night)) +
  geom_col(fill = "blue") +
  labs(
    x = "Date",
    y = "Total nighttime precipitation (mm)"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting temperature
ggplot(weather_night_filtered, aes(x = date, y = Temperature_mean_night)) +
  geom_smooth(color = "#D55E00")+
  geom_line(color = "#D55E00") +
  geom_point(color = "#D55E00") +
  labs(
    x = "Date",
    y = "Mean nighttime temperature (°C)"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting temperature and precipitation
ggplot(weather_night_filtered, aes(x = date)) +
  geom_col(aes(y = Rain_fall_night), fill = "steelblue") +
  geom_line(aes(y = Temperature_mean_night), color = "#D55E00") +
  geom_point(aes(y = Temperature_mean_night), color = "#D55E00") +
  geom_smooth(aes(y = Temperature_mean_night), color = "#D55E00", se = FALSE) +
  scale_y_continuous(
    name = "Total nighttime precipitation (mm)",
    sec.axis = sec_axis(trans = ~ ., name = "Mean nighttime temperature (°C)")
  ) +
  labs(x = "Month") +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2025-03-15"), as.Date("2025-11-15"), by = "1 month")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),                    
    axis.text.x = element_text(hjust = 1, size = 10),   
    axis.text.y = element_text(size = 10),              
    axis.title.x = element_text(size = 11),             
    axis.title.y = element_text(color = "steelblue", size = 11),   
    axis.title.y.right = element_text(color = "#D55E00", size = 11))

# Ensuring that both date columns have the same date format
final_data$Date <- as.Date(final_data$Date)
weather_night$date <- as.Date(weather_night$date, format = "%d/%m/%Y")

bat_activity_weather <- left_join(final_data, weather_night_filtered, by = c("Date" = "date"))

# Removing NAs, but only from the Activity_Minutes column
bat_activity_filtered <- bat_activity_weather %>%
  filter(!is.na(Activity_Minutes))
bat_activity_filtered$site <- as.factor(bat_activity_filtered$site)
bat_activity_filtered$recording_status <- as.factor(bat_activity_filtered$recording_status)

# adding a "landuse" column and renaming Recorder ID
bat_activity_filtered <- bat_activity_filtered %>%
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
bat_activity_filtered <- bat_activity_filtered %>%
 filter(Date >= as.Date("2025-04-24"))

# Julian day
bat_activity_filtered$julian_day <- as.numeric(format(bat_activity_filtered$Date, "%j"))

# Julian day as factor
bat_activity_filtered$julian_day_factor <- as.factor(bat_activity_filtered$julian_day)

# Adding total number of days per site to each day in bat_activity_filtered
bat_activity_filtered <- left_join(
  bat_activity_filtered,
  dplyr::select(record_days, site, days_total),
  by = "site"
)

# Boxplot showing Bat activity per day per AudioMoth recorder
# Set the order of site_abbr
bat_activity_filtered$site_abbr <- factor(
  bat_activity_filtered$site_abbr,
  levels = c(
    "IG-D3", "IG-D7", "IG-D11", "IG-D17", "IG-D23", "IG-D25",
    "PV-RW1", "PV-RW3", "PV-RW13", "PV-RW15", "PV-RW19", "PV-RW21",
    "PV-M1", "PV-M2", "PV-M3", "PV-M4", "PV-M5", "PV-M6"
  )
)

ggplot(bat_activity_filtered, aes(x = site_abbr, y = Activity_Minutes, color= site_abbr)) +
  geom_boxplot() +
  scale_color_manual(values = c(
    "IG-D3" = "steelblue",
    "IG-D7"= "steelblue",
    "IG-D11"= "steelblue",
    "IG-D17"= "steelblue",
    "IG-D23"= "steelblue",
    "IG-D25" = "steelblue",
    "PV-M1" = "darkgreen",
    "PV-M2" = "darkgreen",
    "PV-M3" = "darkgreen",
    "PV-M4" = "darkgreen",
    "PV-M5" = "darkgreen",
    "PV-M6" = "darkgreen",
    "PV-RW1" = "tomato",
    "PV-RW3" = "tomato",
    "PV-RW13" = "tomato",
    "PV-RW15" = "tomato",
    "PV-RW19" = "tomato",
    "PV-RW21" = "tomato"
  )) +
  labs(x = "Recorder ID",
       y = "Recorded bat activity minututes per night") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 11),          
        axis.title = element_text(size = 11),    
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 11),   
        axis.text = element_text(size = 10) )+
  guides(color = "none") 

bat_activity_filtered %>%
  group_by(site_abbr) %>%
  summarise(
    n = n(),
    median = median(Activity_Minutes, na.rm = TRUE),
    mean = mean(Activity_Minutes, na.rm = TRUE),
    IQR = IQR(Activity_Minutes, na.rm = TRUE),
    min = min(Activity_Minutes, na.rm = TRUE),
    max = max(Activity_Minutes, na.rm = TRUE)
  )

bat_activity_filtered$landuse <- factor(
  bat_activity_filtered$landuse,
  levels = c(
    "Intensively used grassland + drained peatland",
    "PV on rewetted peatland",
    "PV on mineral soil"
  )
)

# Calculating the number of recorded days and the number of recorded minutes (total)
days_per_site <- bat_activity_filtered %>%
  mutate(Date = as.Date(Date)) %>%            
  group_by(site_abbr) %>%
  summarise(unique_days = n_distinct(Date))
print(days_per_site)

days_per_site <- days_per_site %>%
  mutate(total_minutes_possible = unique_days * 144) # 144 recordings per night
print(days_per_site)
total_minutes_per_landuse <- days_per_site %>%
  mutate(
    landuse = case_when(
      str_detect(site_abbr, "^IG-D") ~ "Intensively used grassland + drained peatland",
      str_detect(site_abbr, "^PV-RW") ~ "PV on rewetted peatland",
      str_detect(site_abbr, "^PV-M") ~ "PV on mineral soil",
      TRUE ~ NA_character_
  )) %>%
  group_by(landuse) %>%
  summarise(total_minutes = sum(total_minutes_possible, na.rm = TRUE))
print(total_minutes_per_landuse)

missing_data_final_2025 <- missing_data_final_2025 %>%
  mutate(site_abbr = case_when(
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

missing_data_final_2025$site_abbr <- factor(
  missing_data_final_2025$site_abbr,
  levels = c(
    "IG-D3", "IG-D7", "IG-D11", "IG-D17", "IG-D23", "IG-D25",
    "PV-RW1", "PV-RW3", "PV-RW13", "PV-RW15", "PV-RW19", "PV-RW21",
    "PV-M1", "PV-M2", "PV-M3", "PV-M4", "PV-M5", "PV-M6"
  )
)

# Calculating missing days per site
missing_days_per_site <- missing_data_final_2025 %>%
  mutate(Date = as.Date(Date)) %>%            
  group_by(site_abbr) %>%
  summarise(unique_days = n_distinct(Date))
print(missing_days_per_site)

# Number of days in total during the recorded time period (24.04.2025-18.07.2025)
start <- as.Date("2025-04-24")
end   <- as.Date("2025-07-18")
days_inclusive <- as.integer(end - start) + 1
days_inclusive
# Result: 86

# Table:  calculated monthly recorded bat activity per recorder ID
bat_summary_aggregated <- bat_activity_filtered %>%
  group_by(landuse, site_abbr) %>%
  summarise(
    total_activity = sum(Activity_Minutes, na.rm = TRUE),
    total_days = n_distinct(Date),   # recorded days per site
    .groups = "drop"
  ) %>%
  mutate(rate_per_month = (total_activity / total_days) * 30) %>% # approx. per month
  mutate(rate_per_month = round(rate_per_month, 1)) %>%
  dplyr::select(landuse, site_abbr, rate_per_month) %>%
  pivot_wider(
    names_from = site_abbr,
    values_from = rate_per_month,
    values_fill = NA # fill missing combinations with NA
  ) 

# Without IG-D23 & IG-D25
bat_activity_filtered2 <- bat_activity_filtered %>%
  filter(!site_abbr %in% c("IG-D23", "IG-D25"))

bat_activity_filtered2$landuse <- factor(
  bat_activity_filtered2$landuse,
  levels = c(
    "Intensively used grassland + drained peatland",
    "PV on rewetted peatland",
    "PV on mineral soil"
  )
)

# write_xlsx(bat_activity_filtered, path = "~/bat_activity_filtered_2025.xlsx")

# Standardizing the number of bat activity minutes (6,6 and 4 recorders in the different types of land use)
bat_activity_filtered_2 <- bat_activity_filtered2 %>%
  group_by(Date, landuse) %>%
  summarise(
    total_activity = sum(Activity_Minutes),
    recorder_count = n(),  # number of recorders
    .groups = "drop"
  ) %>%
  mutate(
    Activity_Minutes_standardized_landuse = total_activity / recorder_count
  )
                
# Time data
# standardized per land use 
bat_activity_filtered_2$landuse <- factor(
  bat_activity_filtered_2$landuse,
  levels = c(
    "Intensively used grassland + drained peatland",
    "PV on rewetted peatland",
    "PV on mineral soil"
  )
)

ggplot(bat_activity_filtered_2, aes(x = Date, y = Activity_Minutes_standardized_landuse, colour = landuse)) + 
  geom_line(alpha = 0.4) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) + 
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2025-03-15"), as.Date("2025-11-15"), by = "1 month")
  ) +
  scale_color_manual(values = c(
    "PV on rewetted peatland" = "steelblue",
    "Intensively used grassland + drained peatland" = "tomato",
    "PV on mineral soil" = "darkgreen"
  )) +
  labs(
    y = "Recorded bat activity minutes per night",
    x = "Month",
    colour = "Land use"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 11),                   
    axis.title = element_text(size = 11),              
    axis.text = element_text(size = 10),               
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)),
    legend.position = "top",
    legend.direction = "horizontal"
  )

monthly_mean_activity <- bat_activity_filtered_2 %>%
  mutate(Month = month(Date, label = TRUE)) %>%  
  group_by(landuse, Month) %>%
  summarise(Mean_Activity = mean(Activity_Minutes_standardized_landuse, na.rm = TRUE)) %>%
  ungroup()

# checking correlations
cor.test(bat_activity_filtered2$julian_day_scaled, bat_activity_filtered2$Temperature_mean_night, method = "pearson")
cor.test (bat_activity_filtered2$julian_day_scaled, bat_activity_filtered2$Rain_fall_night, method = "pearson")
cor.test(bat_activity_filtered2$Rain_fall_night, bat_activity_filtered2$Temperature_mean_night, method = "pearson")

bat_activity_filtered2$landuse <- factor(
  bat_activity_filtered2$landuse,
  levels = c("PV on rewetted peatland", "Intensively used grassland + drained peatland", "PV on mineral soil")
)

# without IG-D23 and IG-D25
# without julian date as fixed effect

mod2_3<- glmmTMB(
  Activity_Minutes ~ landuse + Rain_fall_night + Temperature_mean_night +  
    offset(log(days_total)) +
    (1 | site) +  (1 | julian_day_factor) ,
  data = bat_activity_filtered2,
  family = nbinom2(),
  dispformula = ~1,
  control = glmmTMBControl(optCtrl = list(iter.max = 10000))
)

summary(mod2_3)
check_overdispersion(mod2_3)
check_autocorrelation(mod2_3)
check_residuals(mod2_3)
check_model(mod2_3)


icc(mod2_3)

# with and without 'site' as a random effect
mod_full <- glmmTMB(Activity_Minutes ~ landuse + Rain_fall_night + Temperature_mean_night + offset(log(days_total)) + (1 | site) + (1 | julian_day_factor),
                    data = bat_activity_filtered2,
                    family = nbinom2())

mod_no_site <- update(mod_full, . ~ . - (1 | site))

anova(mod_no_site, mod_full)

# with and without 'julian_day_factor' as a random effect
mod_no_julian <- update(mod_full, . ~ . - (1 | julian_day_factor))

anova(mod_no_julian, mod_full)

############
# 1. Temperature effect
temp_effect <- ggpredict(mod2_3, terms = "Temperature_mean_night [all]", condition = c(days_total = 1))
p_temp <- ggplot(temp_effect, aes(x = x, y = predicted)) +
  geom_line(color = "#D55E00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Mean night temperature (°C)", y= NULL) +
  theme_minimal()+
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# 2. Precipitation effect
rain_effect <- ggpredict(mod2_3, terms = "Rain_fall_night [all]", condition = c(days_total = 1))
p_rain <- ggplot(rain_effect, aes(x = x, y = predicted)) +
  geom_line(color = "skyblue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Total night precipitation (mm)", y= NULL) +
  theme_minimal()+
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# 3. Landuse effect
landuse_effect <- ggpredict(mod2_3, terms = "landuse [all]", condition = c(days_total = 1))
landuse_effect$x <- factor(landuse_effect$x, levels = c(
  "Intensively used grassland + drained peatland",
  "PV on rewetted peatland",
  "PV on mineral soil"
))
p_landuse <- ggplot(landuse_effect, aes(x = x, y = predicted)) +
    geom_col( alpha = 0.7) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    labs(x = "Land use", y= NULL) +
    theme_minimal()+
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# label for y-axis
y_label <- ggplot() + 
  theme_void() +
  annotate("text", x = 0, y = 0.5, 
           label = "Predicted bat activity minutes per night", 
           angle = 90, size = 4.3)

# all together
plots <- list(p_landuse,p_temp, p_rain)

combined_plot <- y_label + wrap_plots(plots, ncol = 1) + 
  plot_layout(widths = c(0.05, 0.95))  
combined_plot




