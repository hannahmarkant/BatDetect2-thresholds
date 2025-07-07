# Load packages
library(caret)
library(ggplot2)
library(PRROC)
library(readxl)
library(dplyr)      


# "det_prob" (Batdetect2 score) and "Validation" (0 = no bat, 1 = bat)
setwd("~/Uni_Greifswald/Masterarbeit")  # Your work directory

data2 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

# "det_prob"  only two decimal places
data2$det_prob <- trunc(data2$confidence_index * 100) / 100


# List of thresholds from 0-0.89 (to avoid NAs), in 0.01 steps
thresholds <- seq(0.00, 0.91, by = 0.01)

# Empty vectors for precision and recall
precision_values <- numeric()
recall_values <- numeric()

library(caret)  # Für confusionMatrix()

# Filter für Myotis_species
data_myotis <- data2 %>%
  filter(simp_species == "Myotis_species")


# Berechnung für jeden Schwellenwert
for (t in thresholds) {
  # Vorhersage basierend auf Threshold
  predicted <- ifelse(data_myotis$confidence_index >= t, 1, 0)
  
  # Levels angleichen, damit confusionMatrix korrekt funktioniert
  levels_predicted <- levels(as.factor(data_myotis$success))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix berechnen
  cm <- confusionMatrix(predicted, as.factor(data_myotis$success), positive = "1")
  
  # Precision und Recall speichern
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

precision_recall_myotis <- data.frame(
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)
print(precision_recall_myotis)

# Gewicht definieren (z. B. 0.5 für gleichmäßige Gewichtung)
w <- 0.75

# Performance berechnen und auf 4 Kommastellen runden
precision_recall_myotis <- precision_recall_myotis %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

# Ergebnisse anzeigen
View(precision_recall_myotis)

# Besten Threshold basierend auf der höchsten Performance finden
best_threshold_row <- precision_recall_myotis %>%
  filter(Performance == max(Performance, na.rm = TRUE))

# Ergebnis anzeigen
view(best_threshold_row)

####Nyctaloid group

library(dplyr)
library(caret)

# Thresholds definieren
thresholds <- seq(0, 0.88, by = 0.01)

# Filter für Nyctaloid_group
data_nyctaloid <- data2 %>%
  filter(simp_species == "Nyctaloid_group")

# Leere Vektoren für Precision und Recall
precision_values <- c()
recall_values <- c()

# Berechnung für jeden Threshold
for (t in thresholds) {
  predicted <- ifelse(data_nyctaloid$confidence_index >= t, 1, 0)
  
  # Levels für confusionMatrix
  levels_predicted <- levels(as.factor(data_nyctaloid$success))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix
  cm <- confusionMatrix(predicted, as.factor(data_nyctaloid$success), positive = "1")
  
  # Precision & Recall speichern
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

# DataFrame mit den Ergebnissen
precision_recall_nyctaloid <- data.frame(
  Group = "Nyctaloid_group",
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)


print(precision_recall_nyctaloid)

# Gewicht definieren (z. B. 0.5 für gleichmäßige Gewichtung)
w <- 0.75

# Performance berechnen
precision_recall_nyctaloid <- precision_recall_nyctaloid %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

# Ergebnisse anzeigen
View(precision_recall_nyctaloid)

# Besten Threshold basierend auf der höchsten Performance finden
best_threshold_row <- precision_recall_nyctaloid %>%
  filter(Performance == max(Performance, na.rm = TRUE))

# Ergebnis anzeigen
print(best_threshold_row)


# Filter für Pipistrellus_species
# Thresholds definieren
thresholds <- seq(0, 0.91, by = 0.01)
data_pipistrellus <- data2 %>%
  filter(simp_species == "Pipistrellus_species")

# Leere Vektoren für Precision und Recall
precision_values <- c()
recall_values <- c()

# Berechnung für jeden Threshold
for (t in thresholds) {
  predicted <- ifelse(data_pipistrellus$confidence_index >= t, 1, 0)
  
  # Levels für confusionMatrix
  levels_predicted <- levels(as.factor(data_pipistrellus$success))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix
  cm <- confusionMatrix(predicted, as.factor(data_pipistrellus$success), positive = "1")
  
  # Precision & Recall speichern
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
}

# DataFrame mit den Ergebnissen
precision_recall_pipistrellus <- data.frame(
  Group = "Pipistrellus_species",
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4)
)

print(precision_recall_pipistrellus)




# Gewicht definieren (z. B. 0.5 für gleichmäßige Gewichtung)
w <- 0.75

# Performance berechnen
precision_recall_pipistrellus <- precision_recall_pipistrellus %>%
  mutate(Performance = round(Precision * w + Recall * (1 - w), 4))

# Ergebnisse anzeigen
View(precision_recall_pipistrellus)

# Besten Threshold basierend auf der höchsten Performance finden
best_threshold_row <- precision_recall_pipistrellus %>%
  filter(Performance == max(Performance, na.rm = TRUE))

# Ergebnis anzeigen
print(best_threshold_row)

