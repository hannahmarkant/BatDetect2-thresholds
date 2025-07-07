# This script allows to model the error rate in bat calls automated identification made with 
# any software, by modelling the success probability of an identification as a function of the 
# confidence score provided by the software.Using the models, we can then determine the confidence
# score required to have the targeted False Positive Tolerance in the dataset (from 50% to 10%).
# This allows to make an objective data selection for datasets too large to be fully checked. 
# Then we checked if the automatic identification success and false negatives are biased by the 
# environnemental context of the recording stations.
#############################################################################################
# By Kévin Barré, Julie Pauwels and Yves Bas
#############################################################################################

rm(list=ls())

library(boot) # version 1.3-20
library(lme4) # version 1.1-18-1
library(data.table) # version 1.11.4
library(readxl)
library(dplyr)


setwd("~/Uni_Greifswald/Masterarbeit")  # Your work directory

# csv file containing all the bat passes manually checked. It must contain at least those columns:
# - the site ID ("ID")
# - the species identified by the software ("simp_species")
# - the confidence score of the identification ("confidence_index")
# - the success/failure of automatic identification (0=error and 1=success) ("success")
data1 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

mybats <- data1 %>%
  filter(success == 1) %>%
  select(simp_species) %>%
  unique()

mybats <- sort(c(mybats$simp_species))

#Filtering dataset for species
datacheck <- data1 %>%
  filter(simp_species %in% mybats)
# If no need to check for dependancy of errors to environment please disable the line 30
# csv file containing environmental variables to test the consistency of confidence scores in relation to
# ennvironmental conditions
###data2 <- fread("./environmental_variables.csv")

# If need to check for dependancy of errors to environment:
# Merging both files using the user ID column
###datacheck <- as.data.frame(merge(data1, data2, by.x ="ID", by.y="ID"))
# If no need to check for dependancy of errors to environment please activate the line 36 and disable the line 34:

# If no need to check for dependancy of errors to environment please disable the lines 62 to 71
# Vector creation to store the models intercept (Int) and Estimate, the confidence score thresholds for each 
# false positive tolerance tested (ER 50%, 40%, 30%, 20% and 10%),the false negative rates (FN) and
# false positive rates (FP),the estimate (EstimateVariable) and p-value (PVariable)for each environmental variable test, 
# the number of manual checks per species (Ncheck) and the proportion identification success per species (Success)
Int = vector() 
Estimate = vector() 
ER50 = vector() 
ER60 = vector() 
ER70 = vector()
ER80 = vector()
ER90 = vector() 
FNraw = vector()
FN50 = vector() 
FN60 = vector()
FN70 = vector() 
FN80 = vector() 
FN90 = vector() 
FPraw = vector()
FP50 = vector()
FP60 = vector() 
FP70 = vector() 
FP80 = vector() 
FP90 = vector() 
###Pforest = vector()
###Purban = vector()
###Pwetland = vector()
###Phedgerowlength = vector()
###Pedge = vector()
###EstimateForest = vector()
###EstimateUrban = vector()
###EstimateWetland = vector()
###EstimateHedgerowlength = vector()
###EstimateEdge = vector()
Ncheck = vector()
Success = vector()

# A logistic model is build for each species: success probability of automatic species identification as 
# a function of the confidence score provided by the software
for (i in unique(datacheck$simp_species))
{
  # Species selection
  data = datacheck[which(datacheck$simp_species == i),] 
  # Logistic modelling
  m <- glm(success ~ confidence_index, family=binomial(link = logit), data = data) 
  # Prediction scaling
  x <- seq(0, 1, 0.001) 
  # Logistic predictions
  y = inv.logit(m$coefficients[1] + m$coefficients[2]*x) 
  # Store of the intercept
  Int = c(Int, m$coefficients[1]) 
  # Store of the estimate
  Estimate = c(Estimate, m$coefficients[2])
  # Store of confidence scores threshold corresponding to 
  # each false positive tolerance (50%, 40%, 30%, 20% and 10%)
  ER50 = c(ER50, min(subset(x, y>0.5)))
  ER60 = c(ER60, min(subset(x, y>0.6)))
  ER70 = c(ER70, min(subset(x, y>0.7)))
  ER80 = c(ER80, min(subset(x, y>0.8)))
  ER90 = c(ER90, min(subset(x, y>0.9)))
  # Calculation of the false negative rate for each false positive tolerance
  FNraw = c(FNraw, sum(subset(data$success, data$confidence_index<min(subset(x, y>0))))/sum(data$success))
  FN50 = c(FN50, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.5))))/sum(data$success))
  FN60 = c(FN60, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.6))))/sum(data$success))
  FN70 = c(FN70, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.7))))/sum(data$success))
  FN80 = c(FN80, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.8))))/sum(data$success))
  FN90 = c(FN90, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.9))))/sum(data$success))
  # Calculation of the false positive rate for each false positive tolerance
  FPraw = c(FPraw, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0)))))
  FP50 = c(FP50, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.5))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.5)))))
  FP60 = c(FP60, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.6))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.6)))))
  FP70 = c(FP70, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.7))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.7)))))
  FP80 = c(FP80, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.8))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.8)))))
  FP90 = c(FP90, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.9))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.9)))))
  # Store the number of manual checks and identification success proportion
  Ncheck = c(Ncheck, length(data$success))
  Success = c(Success, sum(subset(data$success, data$success != 0))*100/length(data$success))
}




# If no species from other column (L119-155 and L200-219) was used please remove ", "Myonat"" in lines 224-232
# If dependancy of errors to environment was not tested please disable the lines 224 to 228
# Placing results in a dataframe
ErrorRiskThresholds = as.data.frame(cbind(Species = c(simp_species = unique(datacheck$simp_species)), Int, Estimate
                                          , ER50, ER60, ER70, ER80, ER90, FNraw,
                                          FN50, FN60, FN70, FN80, FN90, FPraw,FP50, FP60, FP70, FP80, FP90, Ncheck, Success))

# Saving the dataframe
write.csv(ErrorRiskThresholds, "./ErrorRiskThresholds.csv", row.names=F)


###bis hier der code von barree
library(dplyr)
library(tidyr)

# Umwandlung von ER-Werten in Long-Format
logistic_mods <- ErrorRiskThresholds %>% 
  pivot_longer(cols = -Species,      # Alles außer "Species" wird umgeformt
               names_to = "variable",
               values_to = "value") %>%
  mutate(vartype = substr(variable, 1, 2)) %>%
  filter(vartype == "ER" & value != Inf) %>%
  mutate(prec = as.numeric(substr(variable, 3, 4)))

# Bestes Threshold je Art auswählen
logistic_max <- logistic_mods %>% 
  group_by(Species) %>% 
  mutate(max_thres = max(value, na.rm=TRUE),
         max_prec = max(prec, na.rm=TRUE)) %>%
  filter(value == max_thres & prec == max_prec)

# Modellgüte bewerten oberhalb der Schwelle
log_mods <- datacheck %>% 
  select(Species = simp_species, confidence_index, success) %>%
  merge(logistic_max, by="Species", all.x=TRUE) %>%
  group_by(Species) %>%
  mutate(all_pos = sum(success)) %>%
  filter(confidence_index >= max_thres) %>%
  reframe(
    prec = mean(success),
    recall = sum(success)/all_pos,
    model_performance = prec*0.75 + recall*0.25
  ) %>% unique() %>%
  mutate(threshold = "logi")

# Ergebnis speichern
saveRDS(log_mods, "logistic_models.rds")

ErrorRiskThresholds %>% filter(Species == "Myotis_species") %>% select(ER50, ER60, ER70, ER80, ER90)
library(ggplot2)
ggplot(datacheck %>% filter(simp_species == "Myotis_species"),
       aes(x = confidence_index, y = success)) +
  geom_jitter(height = 0.05, width = 0.01) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
####ER50 = 0 heißt, das Modell sagt schon bei niedrigstem confidence score eine Erfolgswahrscheinlichkeit von > 50 % voraus — was ungewöhnlich sein kann, aber datenbedingt durchaus vorkommen kann.


##########

# This script allows to model the error rate in bat calls automated identification made with 
# any software, by modelling the success probability of an identification as a function of the 
# confidence score provided by the software.Using the models, we can then determine the confidence
# score required to have the targeted False Positive Tolerance in the dataset (from 50% to 10%).
# This allows to make an objective data selection for datasets too large to be fully checked. 
# Then we checked if the automatic identification success and false negatives are biased by the 
# environnemental context of the recording stations.
#############################################################################################
# By Kévin Barré, Julie Pauwels and Yves Bas
#############################################################################################

library(dplyr)
library(boot) # version 1.3-20
library(lme4) # version 1.1-18-1
library(data.table) # version 1.11.4
library(tidyr)
library(readxl)


# csv file containing all the bat passes manually checked. It must contain at least those columns:
# - the site ID ("ID")
# - the species identified by the software ("german")
# - the confidence score of the identification ("confidence_index")
# - the success/failure of automatic identification (0=error and 1=success) ("success")

setwd("~/Uni_Greifswald/Masterarbeit")  # Your work directory

# csv file containing all the bat passes manually checked. It must contain at least those columns:
# - the site ID ("ID")
# - the species identified by the software ("simp_species")
# - the confidence score of the identification ("confidence_index")
# - the success/failure of automatic identification (0=error and 1=success) ("success")
data1 <- read_excel("sortiert_Human_validation_speciesspecificthreshold.xlsx")

mybats <- data1 %>%
  filter(success == 1) %>%
  select(simp_species) %>%
  unique()

mybats <- sort(c(mybats$simp_species))

#Filtering dataset for species
datacheck <- data1 %>%
  filter(simp_species %in% mybats)



# If no need to check for dependancy of errors to environment please disable the lines 62 to 71
# Vector creation to store the models intercept (Int) and Estimate, the confidence score thresholds for each 
# false positive tolerance tested (ER 50%, 40%, 30%, 20% and 10%),the false negative rates (FN) and
# false positive rates (FP),the estimate (EstimateVariable) and p-value (PVariable)for each environmental variable test, 
# the number of manual checks per species (Ncheck) and the proportion identification success per species (Success)
Int = vector() 
Estimate = vector() 
ER10 = vector() 
ER20 = vector() 
ER30 = vector() 
ER40 = vector() 
ER50 = vector() 
ER60 = vector() 
ER70 = vector()
ER80 = vector()
ER90 = vector() 
FNraw = vector()
FN10 = vector() 
FN20 = vector() 
FN30 = vector() 
FN40 = vector() 
FN50 = vector() 
FN60 = vector()
FN70 = vector() 
FN80 = vector() 
FN90 = vector() 
FPraw = vector()
FP10 = vector()
FP20 = vector()
FP30 = vector()
FP40 = vector()
FP50 = vector()
FP60 = vector() 
FP70 = vector() 
FP80 = vector() 
FP90 = vector() 
Ncheck = vector()
prec = vector()
recall = vector()

# A logistic model is build for each species: success probability of automatic species identification as 
# a function of the confidence score provided by the software
for (i in unique(datacheck$simp_species))
{
  # Species selection
  data = datacheck[which(datacheck$simp_species == i),] 
  # Logistic modelling
  m <- glm(success ~ confidence_index, family=binomial(link = logit), data = data) 
  # Prediction scaling
  x <- seq(0, 1, 0.001) 
  # Logistic predictions
  y = inv.logit(m$coefficients[1] + m$coefficients[2]*x) 
  # Store of the intercept
  Int = c(Int, m$coefficients[1]) 
  # Store of the estimate
  Estimate = c(Estimate, m$coefficients[2])
  # Store of confidence scores threshold corresponding to 
  # each false positive tolerance (50%, 40%, 30%, 20% and 10%)
  ER10 = c(ER10, min(subset(x, y>0.1)))
  ER20 = c(ER20, min(subset(x, y>0.2)))
  ER30 = c(ER30, min(subset(x, y>0.3)))
  ER40 = c(ER40, min(subset(x, y>0.4)))
  ER50 = c(ER50, min(subset(x, y>0.5)))
  ER60 = c(ER60, min(subset(x, y>0.6)))
  ER70 = c(ER70, min(subset(x, y>0.7)))
  ER80 = c(ER80, min(subset(x, y>0.8)))
  ER90 = c(ER90, min(subset(x, y>0.9)))
  
  # Calculation of the false negative rate for each false positive tolerance
  FNraw = c(FNraw, sum(subset(data$success, data$confidence_index<min(subset(x, y>0))))/sum(data$success))
  FN10 = c(FN10, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.1))))/sum(data$success))
  FN20 = c(FN20, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.2))))/sum(data$success))
  FN30 = c(FN30, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.3))))/sum(data$success))
  FN40 = c(FN40, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.4))))/sum(data$success))
  FN50 = c(FN50, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.5))))/sum(data$success))
  FN60 = c(FN60, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.6))))/sum(data$success))
  FN70 = c(FN70, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.7))))/sum(data$success))
  FN80 = c(FN80, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.8))))/sum(data$success))
  FN90 = c(FN90, sum(subset(data$success, data$confidence_index<min(subset(x, y>0.9))))/sum(data$success))
  
  # Calculation of the false positive rate for each false positive tolerance
  FPraw = c(FPraw, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0)))))
  FP10 = c(FP10, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.1))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.1)))))
  FP20 = c(FP20, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.2))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.2)))))
  FP30 = c(FP30, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.3))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.3)))))
  FP40 = c(FP40, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.4))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.4)))))
  FP50 = c(FP50, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.5))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.5)))))
  FP60 = c(FP60, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.6))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.6)))))
  FP70 = c(FP70, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.7))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.7)))))
  FP80 = c(FP80, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.8))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.8)))))
  FP90 = c(FP90, sum(subset((1-data$success), data$confidence_index>=min(subset(x, y>0.9))))/length(subset(data$confidence_index, data$confidence_index>=min(subset(x, y>0.9)))))
  # Store the number of manual checks and identification success proportion
  Ncheck = c(Ncheck, length(data$success))
  prec = c(prec, sum(subset(data$success, data$success != 0)))
  recall = c(recall, sum(subset(data$success, data$success != 0)))/length(data$success)
}

# If dependancy of errors to environment was tested please activate the lines 229 to 231									   
ErrorRiskThresholds = as.data.frame(cbind(simp_species = unique(datacheck$simp_species), 
                                          ER10, ER20, ER30, ER40, ER50, ER60, ER70, ER80, ER90, Ncheck, prec, recall))

#FNraw,FN10, FN20, FN30, FN40, FN50, FN60, FN70, FN80, FN90, FPraw,
#FP10, FP20, FP30, FP40, FP50, FP60, FP70, FP80, FP90, Ncheck, Success))


logistic_mods <- ErrorRiskThresholds %>% 
  pivot_longer(cols = -simp_species,      # Columns to pivot (exclude ID)
               names_to = "variable", # Name for the new variable column
               values_to = "value" # Name for the new value column
  ) %>%
  mutate(vartype = substr(variable, start=1, stop=2)) %>%
  filter(vartype == "ER" & value != Inf) %>%
  mutate(prec = as.numeric(substr(variable, start=3, stop=4)))

logistic_max <- logistic_mods %>% group_by(simp_species) %>% 
  mutate(max_thres = max(value, na.rm=T),
         max_prec = max(prec, na.rm = T)) %>% filter(value == max_thres &
                                                       prec == max_prec)

log_mods <- datacheck %>% select(simp_species, confidence_index, success) %>% 
  merge(logistic_max, by="simp_species", all.x=T) %>%
  group_by(simp_species) %>%
  mutate(all_pos = sum(success)) %>%
  filter(confidence_index >= max_thres) %>% 
  reframe(prec = mean(success),
            recall = sum(success)/all_pos,
            model_performance = prec*0.75+recall*0.25) %>% unique() %>% mutate(threshold = "logi")

# Saving the dataframe
saveRDS(log_mods, "./data/logistic_models.rds", row.names=F)




