#################################################################################
# In this script a non-metric multidimensional scaling (NMDS) was used to visualize
# differences in bat community composition across land-use types (intensively used 
# grassland + drained peatland, PV on mineral soil, PV on rewetted peatland) in 2024.  
# Environmental vectors (precipitation, mean temperature, month) were fitted onto
# the ordination to explore potential drivers of community structure. 
# The resulting stress value (~0.02) indicates a reliable two-dimensional 
# representation of the community patterns.
#################################################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(labdsv)
library(vegan)
library(ggplot2)
library(ggrepel)
library(indicspecies)
library(grid)
library(lubridate)


# loading data
setwd("./")
audio_data_2024_filtered <- read_csv("~/audio_data_2024_filtered.csv")
head (audio_data_2024_filtered)

# filtering species which have too small amount of counts (2 counts)
audio_data_2024_filtered <- audio_data_2024_filtered %>%
  filter(species_group != "Plecotus_species")

# Community-matrix (species x site)
pivot_tab <- audio_data_2024_filtered %>%
  group_by(site_monthinfo, species_group, landuse) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = species_group,
    values_from = count,
    values_fill = 0
  )

# Community data (species only)
comm <- as.data.frame(pivot_tab[,-c(1:2)])   
rownames(comm) <- pivot_tab$site_monthinfo

# Stress test for different dimentsions (k)
stress_values <- sapply(2:5, function(k) {
  metaMDS(comm, distance = "bray", k = k, trymax = 50, autotransform = TRUE)$stress
})
stress_values

# NMDS
nmds1 <- metaMDS(comm, distance = "bray", k = 2, trymax = 100, autotransform = TRUE)

# site & species scores
nmds.sites <- as.data.frame(vegan::scores(nmds1, display = "sites"))
nmds.species <- as.data.frame(vegan::scores(nmds1, display = "species"))
nmds.sites$landuse <- pivot_tab$landuse
nmds.species <- as.data.frame(vegan::scores(nmds1, display="species"))
nmds.species$species_group <- rownames(nmds.species)

# Ellipse function
veganCovEllipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi / npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# Ellipses per land use
ellipse_df <- data.frame()
for(g in levels(as.factor(nmds.sites$landuse))){
  subset_data <- nmds.sites[nmds.sites$landuse == g, ]
  if(nrow(subset_data) > 2){
    cov_mat <- cov.wt(subset_data[, c("NMDS1","NMDS2")])$cov
    center <- colMeans(subset_data[, c("NMDS1","NMDS2")])
    ellipse_points <- as.data.frame(veganCovEllipse(cov_mat, center=center))
    colnames(ellipse_points) <- c("NMDS1","NMDS2")
    ellipse_points$landuse <- g
    ellipse_df <- rbind(ellipse_df, ellipse_points)
  }
}

# ANOSIM test (community difference by landuse)
pivot_tab2 <- separate(pivot_tab, col = site_monthinfo, into = c("site_id", "month"), sep = "_")
set.seed(123) 
ano_lu <- anosim(comm, pivot_tab$landuse, distance="bray", permutations=9999)
print(ano_lu)
ano_lu <- anosim(comm, pivot_tab2$site_id, distance="bray", permutations=9999)
print(ano_lu)
ano_lu <- anosim(comm, pivot_tab2$month, distance="bray", permutations=9999)
print(ano_lu)

# adding weather data
weather_night_2024 <- read_excel("Uni_Greifswald/Masterarbeit/weather_night_2024.xlsx")
head(weather_night_2024)

weather_month <- weather_night_2024 %>%
  mutate(date = as.Date(date)) %>%
    mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  summarise(
    Rain_fall_month = mean(Rain_fall_night, na.rm = TRUE), #sum
    Temperature_mean_month = mean(Temperature_mean_night, na.rm = TRUE)
  )

nmds.sites$site_monthinfo <- rownames(nmds.sites)
weather_month <- weather_month %>% rename(site_monthinfo = year_month)
nmds.sites$year_month <- sub(".*_(\\d{4}-\\d{2})", "\\1", nmds.sites$site_monthinfo)
nmds.sites <- nmds.sites %>%
  left_join(weather_month %>% rename(year_month = site_monthinfo), by = "year_month")
nmds.sites$month <- as.numeric(sub(".*-(\\d{2})$", "\\1", nmds.sites$site_monthinfo))

# Adding envfit with climta variables
clim_vars <- nmds.sites %>% select(Rain_fall_month, Temperature_mean_month, month) 
bat.fit <- envfit(nmds1, clim_vars, choices = 1:2)  # nmds1, nicht NMDS_plot
env_test_result<-envfit(nmds1, clim_vars, permutations = 999)
print(env_test_result)
env_scores <- as.data.frame(bat.fit$vectors$arrows * sqrt(bat.fit$vectors$r))
env_scores$variable <- rownames(env_scores)

# Landuse centroids
landuse_centroids <- nmds.sites %>%
  group_by(landuse) %>%
  summarise(
    NMDS1 = mean(NMDS1),
    NMDS2 = mean(NMDS2)
  )

# Spearman correlations between NMDS axes and environmental variables
# Using site scores from nmds1 
nmds_points <- as.data.frame(scores(nmds1, display = "sites"))

# selecting environmental variables from nmds.sites
clim_vars <- nmds.sites %>% select(month, Rain_fall_month, Temperature_mean_month, landuse)

# converting landuse to numeric
clim_vars$landuse <- as.numeric(factor(clim_vars$landuse))

# Dataframe to store correlation results
cor_results <- data.frame(
  Variable = c( "month", "Rain_fall_month", "Temperature_mean_month", "landuse"), 
  NMDS1_rho = NA,
  NMDS1_p = NA,
  NMDS2_rho = NA,
  NMDS2_p = NA
)

# Computing spearman correlations
for(i in seq_along(cor_results$Variable)){
  var <- cor_results$Variable[i]
  test1 <- cor.test(nmds_points[,1], clim_vars[[var]], method = "spearman")
  test2 <- cor.test(nmds_points[,2], clim_vars[[var]], method = "spearman")
  
  cor_results$NMDS1_rho[i] <- test1$estimate
  cor_results$NMDS1_p[i] <- test1$p.value
  cor_results$NMDS2_rho[i] <- test2$estimate
  cor_results$NMDS2_p[i] <- test2$p.value
}

# Round to 4 decimal places
cor_results[, 2:5] <- round(cor_results[, 2:5], 4)
print(cor_results)

# Month as factor with labels
nmds.sites$month <- factor(nmds.sites$month, 
                           levels = c(3, 4, 5, 6,7,8,9,10), 
                           labels = c("March", "April", "May", "June", "July", "August", "September", "October"))

# defining the shapes for each month
shape_values <- c(
  "March" = 0,  
  "April" = 1,  
  "May" = 2,    
  "June" = 3,   
  "July" = 4,   
  "August" = 5, 
  "September" = 6,
  "October" = 7
)

env_scores <- env_scores %>%
  mutate(variable = recode(variable,
                           "Temperature_mean_month" = "Mean night temperature",
                           "Rain_fall_month" = "Total night precipitation",
                           "month" = "Month")       )
nmds.species <- nmds.species %>%
  mutate(species_group = recode(species_group,
                                "Pipistrellus_species" = "Pipistrellus species",
                                "Nyctaloid_group" = "Nyctaloid group",
                                "Myotis_species" = "Myotis species"))


nmds.species$species_label <- case_when(
  nmds.species$species_group == "Pipistrellus species" ~ "italic('Pipistrellus')~species",
  nmds.species$species_group == "Myotis species" ~ "italic('Myotis')~species",
  nmds.species$species_group == "Nyctaloid group" ~ "'Nyctaloid group'",
  TRUE ~ paste0("'", nmds.species$species_group, "'")  # alles andere als normaler Text
)

NMDS_plot_combined <- ggplot(nmds.sites, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = landuse, shape = month), size = 3) +
  geom_path(data = ellipse_df, aes(x = NMDS1, y = NMDS2, color = landuse), linewidth = 1) +
  geom_text_repel(
    data = nmds.species,
    aes(label = species_label),
    parse = TRUE,  # wichtig!
    size = 4,
    max.overlaps = 200
  ) +
  geom_segment(data = env_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_text_repel(data = env_scores,
                  aes(x = NMDS1, y = NMDS2, label = variable),
                  color = "black", size = 4,
                  nudge_x = env_scores$nudge_x,
                  nudge_y = env_scores$nudge_y)+
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values=c("PV on rewetted peatland" = "steelblue",
                              "Intensively used grassland + drained peatland" = "tomato"))+
    theme_bw() +
  labs(color = "Land use", shape = "Month",
       caption = paste("Stress:", round(nmds1$stress, 3))) + 
    guides(
      color = guide_legend(order = 1, nrow = 1, byrow = TRUE, label.hjust = 0),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.spacing.y = unit(0.1, "cm"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 6, r = 10)),
    axis.title = element_text(size = 11),   
    axis.text = element_text(size = 10)
  )

print(NMDS_plot_combined)

# FINAL: Combined NMDS plot with envfit and centroids 
NMDS_plot_combined <- ggplot(nmds.sites, aes(NMDS1, NMDS2)) +
  geom_point(aes(color = landuse), size = 3) +
  geom_path(data = ellipse_df, aes(x = NMDS1, y = NMDS2, color = landuse), linewidth = 1) +
  geom_text_repel(
    data = nmds.species,
    aes(label = species_label),
    parse = TRUE,  
    size = 4,
    max.overlaps = 200
  ) +
  geom_segment(data = env_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_text_repel(data = env_scores,
                  aes(x = NMDS1, y = NMDS2, label = variable),
                  color = "black", size = 4,
                  nudge_x = env_scores$nudge_x,
                  nudge_y = env_scores$nudge_y)+
  theme_bw() +
  labs(color = "Landuse",
       caption = paste("Stress:", round(nmds1$stress, 3)))+
  scale_color_manual(values=c("PV on rewetted peatland" = "steelblue",
                              "Intensively used grassland + drained peatland" = "tomato"))+
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11, margin = margin(l = 6, r = 10)),
    axis.title = element_text(size = 11),   
    axis.text = element_text(size = 10))

print(NMDS_plot_combined)
