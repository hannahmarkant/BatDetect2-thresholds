##########################################################################################
# This script evaluated species-specific thresholds of confidence values from BatDetect2 for different bat species groups.
# For confidence score thresholds in 0.01 increments it computes precision, recall and a weighted
# performance score. A weighted combination of precision (p) and recall (r) 
# (performance = (p *w + r *(1- w))) was calculated (Singer et al., 2024). However, precision was weighted 
# higher than recall with a weight (w) of 0.75. The optimal threshold is chosen based 
# on the maximum performance. Three species groups were analyzed: Myotis species, 
# Nyctaloid group and Pipistrellus species.
##########################################################################################
library(caret)
library(readxl)
library(dplyr)      

# "class_prob" (Batdetect2 class confidence score) and "validation" (0 = no bat, 1 = bat)
setwd("~/Uni_Greifswald/Masterarbeit")  # working directory

data2 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

# "class_prob" only two decimal places
data2$class_prob <- trunc(data2$class_prob * 100) / 100

# List of thresholds from 0-0.73 (to avoid NAs), in 0.01 steps
thresholds <- seq(0.00, 0.73, by = 0.01)

# Empty vectors for precision and recall
precision_values <- numeric()
recall_values <- numeric()

# Filter for Myotis_species
data_myotis <- data2 %>%
  filter(species_group == "Myotis_species")

# Computing precision and recall for each threshold
for (t in thresholds) {
  predicted <- ifelse(data_myotis$class_prob >= t, 1, 0)
  
  levels_predicted <- levels(as.factor(data_myotis$validation))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix
  cm <- confusionMatrix(predicted, as.factor(data_myotis$validation), positive = "1")
  
  # Storing precision and recall
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

# Creating results data frame
precision_recall_myotis <- data.frame(
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)
print(precision_recall_myotis)

# Defining weighting factor
w <- 0.75

# Computing performance score and rounding to 4 decimal places
precision_recall_myotis <- precision_recall_myotis %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

View(precision_recall_myotis)

# Finding threshold with highest performance score
best_threshold_row <- precision_recall_myotis %>%
  filter(Performance == max(Performance, na.rm = TRUE))

print(best_threshold_row)

#### Nyctaloid group
setwd("~/Uni_Greifswald/Masterarbeit")  

data2 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

data2$class_prob <- trunc(data2$class_prob * 100) / 100

thresholds <- seq(0, 0.88, by = 0.01)

data_nyctaloid <- data2 %>%
  filter(species_group == "Nyctaloid_group")

precision_values <- c()
recall_values <- c()

for (t in thresholds) {
  predicted <- ifelse(data_nyctaloid$class_prob >= t, 1, 0)
  
  levels_predicted <- levels(as.factor(data_nyctaloid$validation))
  predicted <- factor(predicted, levels = levels_predicted)
  
  cm <- confusionMatrix(predicted, as.factor(data_nyctaloid$validation), positive = "1")
  
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

precision_recall_nyctaloid <- data.frame(
  Group = "Nyctaloid_group",
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)

print(precision_recall_nyctaloid)

w <- 0.75

precision_recall_nyctaloid <- precision_recall_nyctaloid %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

View(precision_recall_nyctaloid)

best_threshold_row <- precision_recall_nyctaloid %>%
  filter(Performance == max(Performance, na.rm = TRUE))

print(best_threshold_row)

#### Pipistrellus_species
setwd("~/Uni_Greifswald/Masterarbeit")  

data2 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

data2$class_prob <- trunc(data2$class_prob * 100) / 100

thresholds <- seq(0, 0.92, by = 0.01)

data_pipistrellus <- data2 %>%
  filter(species_group == "Pipistrellus_species")

precision_values <- c()
recall_values <- c()

for (t in thresholds) {
  predicted <- ifelse(data_pipistrellus$class_prob >= t, 1, 0)
  
  levels_predicted <- levels(as.factor(data_pipistrellus$validation))
  predicted <- factor(predicted, levels = levels_predicted)
  
  cm <- confusionMatrix(predicted, as.factor(data_pipistrellus$validation), positive = "1")
  
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

precision_recall_pipistrellus <- data.frame(
  Group = "Pipistrellus_species",
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)

print(precision_recall_pipistrellus)

w <- 0.75

precision_recall_pipistrellus <- precision_recall_pipistrellus %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

View(precision_recall_pipistrellus)

best_threshold_row <- precision_recall_pipistrellus %>%
  filter(Performance == max(Performance, na.rm = TRUE))

print(best_threshold_row)

