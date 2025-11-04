########################################################################################
# An analysis of bat activity recorded with AudioMoth devices in 2024 was performed.
# The workflow includes identifying missing or incomplete recording days,
# importing and aggregating bat activity data and integrating weather data (total 
# night-time precipitation and mean night-time temperature). The data is visualized 
# over time, per site and per land-use type. Generalized Linear Mixed Models (GLMM) 
# were fitted with step wise removal of non-significant terms.
#######################################################################################

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
library(gt)

# Setting the working directory
setwd("./")

# Loading 'missing' table from the trackingsheet
missing <- read.xlsx("~/AudioMoth_TrackingSheet.12.11.2024.xlsx",
                     sheet = "missing_data") %>%
  mutate(
    date_last = as.Date(as.numeric(date_last), origin = "1899-12-30"),
    data_collection_date = as.Date(as.numeric(data_collection_date), origin = "1899-12-30"),
    date_first = as.Date(as.numeric(date_first), origin = "1899-12-30")
  )

# Create a row for each day with missing recordings before collecting the recorders (data_collection_date)
missing_data_2024 <- missing %>%
  rowwise() %>%
  mutate(
    # only when date_last != data_collection_date
    Date = list(if(date_last != data_collection_date) seq(date_last, data_collection_date, by = "day") else NULL)
  ) %>%
  unnest(Date) %>%
  dplyr::select(site, Date)

# Adding 12.03.2024 and 05.11.2024 for each site (are incomplete days as they are the start and end date of the sampling period and are not fully recorded days)
extra_dates <- missing %>%
  dplyr::select(site) %>%
  distinct() %>%
  mutate(Date = as.Date("2024-03-12")) %>%
  bind_rows(missing %>% dplyr::select(site) %>% distinct() %>% mutate(Date = as.Date("2024-11-05")))

# Combining all missing dates and removing the duplicates
missing_data_final_2024 <- bind_rows(missing_data_2024, extra_dates) %>%
  distinct(site, Date) %>%
  arrange(site, Date)

# Loading the bat activity data
audio_data <- read_delim("~/all_0.66_batdetect2_highest_det_prob_threshold.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
audio_data$Date <- as.Date(audio_data$Date, format = "%Y-%m-%d")

# Calculating the activity minutes per day (00:00-6:00 & 18:00-24:00)
activity_per_day <- audio_data %>%
  group_by(site, Date) %>%
  summarize(Activity_Minutes = n_distinct(Recording_Time), .groups = "drop")

# Generating a complete list of all calender days for each recorder
recording_info <- missing %>%
  rowwise() %>%
  mutate(Date = list(seq(date_first, data_collection_date, by = "day"))) %>%
  unnest(Date) %>%
  mutate(
    recording_status = ifelse(Date <= date_last, "Yes", "No")
  ) %>%
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
    site = factor(site, levels = c("g1", "g3", "pv1", "pv2", "pv3"))
  ) %>%
  arrange(site, Date)

# Setting incomplete days to NA
final_data <- final_data %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(
    missing_data_final_2024 %>% mutate(Date = as.Date(Date), missing_flag = TRUE),
    by = c("site", "Date")
  ) %>%
  mutate(
    Activity_Minutes = ifelse(!is.na(missing_flag) & missing_flag, NA, Activity_Minutes)
  ) %>%
  dplyr::select(-missing_flag)

# Calculate number of unique recording days per recorder
# Days with recording issues are excluded
record_days <- final_data %>%
  anti_join(missing_data_final_2024, by = c("site", "Date")) %>%
  count(site, name = "days_total")

# adding weather data
Lottorf_Weather_2024 <- read_excel("~/Lottorf_Wetterdaten_März-November_2024.xlsx")

# due to missing data in the weather data from the weather station in Lottorf, missing data is added from the DWD station in Schleswig
# hourly temperature from the station in Schleswig
data_OBS_DEU_PT1H_T2M_4466 <- read_csv("~/Deutscher Wetterdienst/data_OBS_DEU_PT1H_T2M_4466.csv")
head(data_OBS_DEU_PT1H_T2M_4466)

# hourly precipitation from the station in Schleswig
data_OBS_DEU_PT1H_RR_4466 <- read_csv("~/Deutscher Wetterdienst/data_OBS_DEU_PT1H_RR_4466.csv")
head(data_OBS_DEU_PT1H_RR_4466)
Sys.setlocale("LC_TIME", "C")

start_date <- as.Date("2024-03-12") # start of the recording period
end_date <- as.Date("2024-11-05") # end of the recording period

weather_night <- Lottorf_Weather_2024 %>%
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

# adding the data from the DWD station in Schleswig to the data from the station in Lottorf (when NA)
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
weather_night_monthly <- weather_filled %>%
  mutate(
    date = as.Date(dateLocale),
    hour = hour(dateLocale),
    month = floor_date(date, unit = "month")
  ) %>%
  filter(hour %in% c(0:5, 18:23)) %>%
  group_by(month) %>%
  summarise(
    Total_night_rainfall = sum(`Rain fall_final`, na.rm = TRUE),
    Mean_night_temperature = mean(Temperature_final, na.rm = TRUE)
  )

# write_xlsx(weather_night_filtered, "~/weather_night_2024.xlsx")

# Plotting precipiation
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
    breaks = seq(as.Date("2024-03-15"), as.Date("2024-11-15"), by = "1 month")
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),                    
    axis.text.x = element_text(hjust = 1, size = 10),   
    axis.text.y = element_text(size = 10),              
    axis.title.x = element_text(size = 11),             
    axis.title.y = element_text(color = "steelblue", size = 11),     
    axis.title.y.right = element_text(color = "#D55E00", size = 11)   
  )

# Ensuring that both date columns have the same date format
final_data$Date <- as.Date(final_data$Date)
weather_night_filtered$date <- as.Date(weather_night_filtered$date)

bat_activity_weather <- left_join(final_data, weather_night_filtered, by = c("Date" = "date"))

# Removing NAs, but only from the Activity_Minutes column
bat_activity_filtered <- bat_activity_weather %>%
  filter(!is.na(Activity_Minutes))
bat_activity_filtered$site <- as.factor(bat_activity_filtered$site)
bat_activity_filtered$recording_status <- as.factor(bat_activity_filtered$recording_status)

# adding a "landuse" column and renaming the Recorder ID
bat_activity_filtered <- bat_activity_filtered %>%
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

#Julian day
bat_activity_filtered$julian_day <- as.numeric(format(bat_activity_filtered$Date, "%j"))

# Julian day as factor
bat_activity_filtered$julian_day_factor <- as.factor(bat_activity_filtered$julian_day)

# Adding total number of days per site to each day in bat_activity_filtered
bat_activity_filtered <- left_join(
  bat_activity_filtered,
  dplyr::select(record_days, site, days_total),
  by = "site"
)

# Boxplot showing bat activity minutes per AudioMoth recorder
ggplot(bat_activity_filtered, aes(x = site_abbr, y = Activity_Minutes, color = site_abbr)) +
  geom_boxplot() +
  scale_color_manual(values = c(
    "IG-D1" = "steelblue",
    "IG-D3" = "steelblue",
    "PV-RW1" = "tomato",
    "PV-RW2" = "tomato",
    "PV-RW3" = "tomato"
  )) +
  labs(x = "Recorder ID",
       y = "Recorded bat activity minutes per night") +
  theme_minimal() +
  theme(
    text = element_text(size = 11),          
    axis.title = element_text(size = 11),    
    legend.title = element_text(size = 11),  
    legend.text = element_text(size = 11),   
    axis.text = element_text(size = 10)      
  )+
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

# Reference level- "Intensively used grassland + drained peatland" for illustrations
bat_activity_filtered$landuse <- factor(
  bat_activity_filtered$landuse,
  levels = c("Intensively used grassland + drained peatland", 
             setdiff(unique(bat_activity_filtered$landuse), 
                     "Intensively used grassland + drained peatland"))
)

# Calculating the total number of recorded days and the number of recorded minutes (total)
days_per_site <- bat_activity_filtered %>%
  mutate(Date = as.Date(Date)) %>%            
  group_by(site_abbr) %>%
  summarise(unique_days = n_distinct(Date))
print(days_per_site)

days_per_site <- days_per_site %>%
  mutate(total_minutes_possible = unique_days * 144) # 144 recordings per night
print(days_per_site)
total_minutes_per_landuse <- days_per_site %>%
  mutate(landuse = case_when(
    site_abbr %in% c("IG-D1", "IG-D3") ~ "Intensively used grassland + drained peatland",
    site_abbr %in% c("PV-RW1", "PV-RW2", "PV-RW3") ~ "PV on rewetted peatland",
    TRUE ~ NA_character_
  )) %>%
  group_by(landuse) %>%
  summarise(total_minutes = sum(total_minutes_possible, na.rm = TRUE))
print(total_minutes_per_landuse)

missing_data_final_2024 <- missing_data_final_2024 %>%
  mutate(site_abbr = case_when(
    site == "g1" ~ "IG-D1",
    site == "g3" ~ "IG-D3",
    site == "pv1" ~ "PV-RW1",
    site == "pv2" ~ "PV-RW2",
    site == "pv3" ~ "PV-RW3",
    TRUE ~ NA_character_
  ))

# Calculating missing days per site
missing_days_per_site <- missing_data_final_2024 %>%
  mutate(Date = as.Date(Date)) %>%            
  group_by(site_abbr) %>%
  summarise(unique_days = n_distinct(Date))
print(missing_days_per_site)

# Number of days in total during the recorded time period
start <- as.Date("2024-03-12")
end   <- as.Date("2024-11-05")
days_inclusive <- as.integer(end - start) + 1
days_inclusive
# Result: 239


#Table: calculated monthly recorded bat activity per recorder ID
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

# write_xlsx(bat_activity_filtered, path = "~/bat_activity_filtered_2024.xlsx")

# Standardizing the number of bat activity minutes (2 vs 3 recorders in the different types of land use)
bat_activity_filtered_2 <- bat_activity_filtered %>%
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
ggplot(bat_activity_filtered_2, aes(x = Date, y = Activity_Minutes_standardized_landuse, colour = landuse)) + 
  geom_line(alpha = 0.4) +
  geom_point(size = 1.5, alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) + 
  scale_x_date(
    date_labels = "%b",
    breaks = seq(as.Date("2024-03-15"), as.Date("2024-11-15"), by = "1 month")
  ) +
  scale_color_manual(values = c(
    "PV on rewetted peatland" = "steelblue",
    "Intensively used grassland + drained peatland" = "tomato"
  )) +
  scale_y_continuous(
    expand = expansion(add = c(0.1, 1)) 
  ) +
  labs(
    y = "Recorded bat activity minutes per night",
    x = "Month",
    colour = "Land use"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 11),                      
    axis.title = element_text(size = 11),                 
    axis.text.x = element_text(size = 10),                
    axis.text.y = element_text(size = 10),                
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),  
    legend.text = element_text(size = 11, margin = margin(l = 5, r = 10)) 
  )

monthly_mean_activity <- bat_activity_filtered_2 %>%
  mutate(Month = month(Date, label = TRUE)) %>%   
  group_by(landuse, Month) %>%
  summarise(Mean_Activity = mean(Activity_Minutes_standardized_landuse, na.rm = TRUE)) %>%
  ungroup()

# offset adds the known quantity of recorded days to the model, to help normalize
# the unequal exposure time due to the different number of recording days
# number of iteration for optimization set to 10000 (instead of 150 or 200)
# helps when max iterations reached, or model will not converge

bat_activity_filtered$landuse <- relevel(bat_activity_filtered$landuse, ref = "PV on rewetted peatland")

# checking correlation
cor.test(bat_activity_filtered$julian_day_scaled, bat_activity_filtered$Temperature_mean_night, method = "pearson")
cor.test (bat_activity_filtered$julian_day_scaled, bat_activity_filtered$Rain_fall_night, method = "pearson")
cor.test(bat_activity_filtered$Rain_fall_night, bat_activity_filtered$Temperature_mean_night, method = "pearson")

# without julian day as fixed effect
mod1_2 <- glmmTMB(Activity_Minutes ~ landuse + Rain_fall_night+ Temperature_mean_night 
                + offset(log(days_total)) + (1 | site)+ (1 | julian_day_factor),
                data = bat_activity_filtered,
                family = nbinom2(),
                dispformula = ~1,
                control = glmmTMBControl(optCtrl = list(iter.max=10000)))

summary(mod1_2)
check_overdispersion(mod1_2)
check_autocorrelation(mod1_2)
check_residuals(mod1_2)
check_model(mod1_2)

icc(mod1_2)

# with and without 'site' as a random effect
mod_full <- glmmTMB(Activity_Minutes ~ landuse + Rain_fall_night + Temperature_mean_night + offset(log(days_total)) + (1 | site) + (1 | julian_day_factor),
                    data = bat_activity_filtered,
                    family = nbinom2())

mod_no_site <- update(mod_full, . ~ . - (1 | site))

anova(mod_no_site, mod_full)

# with and without 'julian_day_factor' as a random effect
mod_no_julian <- update(mod_full, . ~ . - (1 | julian_day_factor))

anova(mod_no_julian, mod_full)


###########
# 1. Temperature effect
temp_effect <- ggpredict(mod1_2, terms = "Temperature_mean_night [all]", condition = c(days_total = 1))
p_temp <- ggplot(temp_effect, aes(x = x, y = predicted)) +
  geom_line(color = "#D55E00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Mean nighttime temperature (°C)", y= NULL) +
  theme_minimal()+
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# 2. Precipitation effect
rain_effect <- ggpredict(mod1_2, terms = "Rain_fall_night [all]", condition = c(days_total = 1))
p_rain <- ggplot(rain_effect, aes(x = x, y = predicted)) +
  geom_line(color = "skyblue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Total nighttime precipitation (mm)", y= NULL) +
  theme_minimal()+
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# 3. Landuse effect
landuse_effect <- ggpredict(mod1_2, terms = "landuse", condition = c(days_total = 1))
landuse_effect$x <- factor(landuse_effect$x, levels = c(
  "Intensively used grassland + drained peatland",
  "PV on rewetted peatland"
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
plots <- list(p_landuse, p_temp, p_rain)

combined_plot <- y_label + wrap_plots(plots, ncol = 1) + 
  plot_layout(widths = c(0.05, 0.95))

combined_plot



