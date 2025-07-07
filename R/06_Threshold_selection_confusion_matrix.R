###########################################################################################
# This script evaluates detection performance of BatDetect2 based on the human validation process.
# It calculates precision, recall, accuracy for both full and species-filtered datasets. 
# An optimal threshold of 0.66 was determined.
#############################################################################################


# Load packages
library(caret)
library(ggplot2)
library(readxl)
library(dplyr) 

# "det_prob" (Batdetect2 score) and "Validation" (0 = no bat, 1 = bat)
setwd("~/Uni_Greifswald/Masterarbeit")
data2 <- read_excel("Meine_Analyse/Det_threshold_manual_insgesamt_3%_Daten.xlsx")

# "det_prob"  only two decimal places
data2$det_prob <- trunc(data2$det_prob * 100) / 100


# List of thresholds from 0-0.91 (to avoid NAs), in 0.01 steps
thresholds <- seq(0, 0.91, by = 0.01)

# Empty vectors for precision and recall
precision_values <- numeric()
recall_values <- numeric()
accuracy_values <- numeric()

# Calculation for each threshold
for (t in thresholds) {
  # Prediction based on each threshold
  predicted <- ifelse(data2$det_prob >= t, 1, 0)
  
  # Ensure that the levels of the factors match
  levels_predicted <- levels(as.factor(data2$Validation))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix for Precision and Recall
  cm <- confusionMatrix(predicted, as.factor(data2$Validation), positive = "1")
  
  # Save the Precision and Recall
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
  accuracy_values <- c(accuracy_values, cm$overall["Accuracy"])
}

precision_recall_df <- data.frame(
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4),
  Accuracy = round(accuracy_values, 4)
)
# By looking at the precision_recall_df the optimal threshold is at 0.66.
#####Species List
allowed_species <- c("Eptesicus serotinus", "Myotis bechsteinii", "Myotis brandtii", "Myotis dasycneme", "Myotis daubentonii","Myotis myotis","Myotis mystacinus", "Myotis nattereri", "Nyctalus leisleri", "Nyctalus noctula", "Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus", "Plecotus auritus", "Vespertilius murinus") 
data_filtered2 <- data2[data2$class %in% allowed_species, ]


data_filtered2$det_prob <- trunc(data_filtered2$det_prob * 100) / 100

# List of thresholds from 0-0.91 (to avoid NAs), in 0.01 steps
thresholds <- seq(0, 0.91, by = 0.01)

# Empty vectors for precision and recall
precision_values <- numeric()
recall_values <- numeric()
accuracy_values <- numeric()

# Calculation for each threshold
for (t in thresholds) {
  # Prediction based on each threshold
  predicted <- ifelse(data_filtered2$det_prob >= t, 1, 0)
  
  # Ensure that the levels of the factors match
  levels_predicted <- levels(as.factor(data_filtered2$Validation))
  predicted <- factor(predicted, levels = levels_predicted)
  
  # Confusion Matrix for Precision and Recall
  cm <- confusionMatrix(predicted, as.factor(data_filtered2$Validation), positive = "1")
  
  # Saving Precision and Recall
  precision_values <- c(precision_values, cm$byClass["Precision"])
  recall_values <- c(recall_values, cm$byClass["Recall"])
  accuracy_values <- c(accuracy_values, cm$overall["Accuracy"])
}

# DataFrame with the results
precision_recall_filtered <- data.frame(
  Threshold = thresholds,
  Precision = round(precision_values, 4),
  Recall = round(recall_values, 4),
  Accuracy = round(accuracy_values, 4)
)
# These results showed that the detection performance was also optimal at a threshold of 0.66.

### Statistics for chosen threshold (0.66)
# Function for calculating the metrics for a given data set and threshold value
evaluate_threshold <- function(data2, threshold, dataset_name) {
  # Predictions based on the threshold value
  predictions <- ifelse(data2$det_prob >= threshold, 1, 0)
  
  # Convert to factor with matching levels
  predictions <- factor(predictions, levels = levels(as.factor(data2$Validation)))
  
  #Calculation of the confusion matrix
  cm <- confusionMatrix(predictions, as.factor(data2$Validation), positive = "1")
  
  
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  accuracy <- cm$overall["Accuracy"]
  specificity <- cm$byClass["Specificity"]  # True Negative Rate
  fpr <- 1 - specificity  # False Positive Rate
  coverage <- mean(predictions == 1)  # Anteil der positiven Klassifizierungen
  
  # Results
  cat("\n--- Evaluation für", dataset_name, "---\n")
  cat("Threshold:", threshold, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1-Score:", f1_score, "\n")
  cat("Accuracy:", accuracy, "\n")
  cat("False Positive Rate:", fpr, "\n")
  cat("Coverage (positively classified instances):", coverage, "\n")
  
  # Data frame
  return(data.frame(Dataset = dataset_name, Threshold = threshold, 
                    Precision = precision, Recall = recall, F1_Score = f1_score, 
                    Accuracy = accuracy, FPR = fpr, Coverage = coverage))
}

# Threshold 0.66: not filtered data
results_data_066 <- evaluate_threshold(data2, 0.66, "Alle Daten")

# Threshold 0.66: filtered data
results_filtered_066 <- evaluate_threshold(data_filtered2, 0.66, "Gefilterte Daten")

# The results showed that the detection performance at the threshold of 0.66 remained unchanged after filtering.
# Therefore it is proceeded with the full dataset.

##### Graph Precision and Recall by threshold for both non-filtered and filtered data
allowed_species <- c("Eptesicus serotinus", "Myotis bechsteinii", "Myotis brandtii", "Myotis dasycneme", "Myotis daubentonii", "Myotis myotis", "Myotis mystacinus", "Myotis nattereri", "Nyctalus leisleri", "Nyctalus noctula", "Pipistrellus nathusii", "Pipistrellus pipistrellus", "Pipistrellus pygmaeus", "Plecotus auritus", "Vespertilius murinus")
data_filtered2 <- data2[data2$class %in% allowed_species, ]
data_filtered2$det_prob <- trunc(data_filtered2$det_prob * 100) / 100

# Defining all thresholds
thresholds <- seq(0, 0.89, by = 0.01)

# Function for calculating precision, recall and F1 score
calculate_metrics <- function(data2, thresholds) {
  precision_values <- numeric()
  recall_values <- numeric()
  
  for (t in thresholds) {
    predicted <- ifelse(data2$det_prob >= t, 1, 0)
    levels_predicted <- levels(as.factor(data2$Validation))
    predicted <- factor(predicted, levels = levels_predicted)
    cm <- confusionMatrix(predicted, as.factor(data2$Validation), positive = "1")
    precision_values <- c(precision_values, cm$byClass["Precision"])
    recall_values <- c(recall_values, cm$byClass["Recall"])
  }
  
  f1_scores <- 2 * (precision_values * recall_values) / (precision_values + recall_values)
  best_f1_threshold_filtered <- thresholds[which.max(f1_scores)]
  best_f1_score <- max(f1_scores, na.rm = TRUE)
  
  data.frame(Threshold = thresholds, Precision = precision_values, Recall = recall_values, BestF1Threshold = best_f1_threshold_filtered, BestF1Score = best_f1_score)
}

# Calculations
metrics_data <- calculate_metrics(data2, thresholds)
metrics_filtered <- calculate_metrics(data_filtered2, thresholds)

# Plotting
combined_plot <- ggplot() +
  geom_line(data = metrics_data, aes(x = Threshold, y = Precision, color = "Precision"), size = 1) +
  geom_line(data = metrics_data, aes(x = Threshold, y = Recall, color = "Recall"), size = 1) +
  geom_line(data = metrics_filtered, aes(x = Threshold, y = Precision, color = "Precision (Species list)"), size = 1, linetype = "dashed") +
  geom_line(data = metrics_filtered, aes(x = Threshold, y = Recall, color = "Recall (Species list)"), size = 1, linetype = "dashed") +
  # Vertical line at threshold 0.66
  geom_vline(xintercept = 0.66, linetype = "dotted", color = "black") +
  annotate("text", x = 0.66, y = 0.2, label = "Threshold: 0.66", color = "black", angle = 90, vjust = -0.5) +
  scale_color_manual(values = c("Precision" = "blue", "Recall" = "red", "Precision (Species list)" = "darkblue", "Recall (Species list)" = "darkred")) +
  scale_y_continuous(
    name = "Precision",
    sec.axis = sec_axis(~., name = "Recall")
  ) +
  guides(color = guide_legend(title = NULL)) +
  labs( x = "Confidence score") +
  theme_minimal() +
  theme(text = element_text(size = 12), axis.title.y.left = element_text(color = "blue"), axis.title.y.right = element_text(color = "red"))

print(combined_plot)
