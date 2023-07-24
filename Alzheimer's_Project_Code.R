###############
# SETUP ----
###############

## Set working directory ----
getwd()

## Sophie
#setwd("/Users/sophielawrence/Documents/Data_Mining_Project")

## Ria
#setwd("/Users/ria/Downloads/Data_mining_project")

## Load libraries ----
library("tidyverse")
library("dplyr")

###############
# DATA CLEANING ----
###############

## Read data ----
df <- read_csv("Alzheimers_data.csv")
head(df)
str(df)

## Change class to factor ----
df$class <- factor(df$class)

## Mutating variables 
df <- df %>%
  mutate(hospital_death = as.factor(hospital_death)) %>%
  mutate(sepsis = as.factor(sepsis)) %>%
  mutate(cardiovascular_diagnosis = as.factor(cardiovascular_diagnosis)) %>%
  mutate(intubated_apache = as.factor(intubated_apache)) %>%
  mutate(gcs_eyes_apache = as.factor(gcs_eyes_apache)) %>%
  mutate(gcs_motor_apache = as.factor(gcs_motor_apache)) %>%
  mutate(temp_apache = as.numeric(temp_apache)) %>%
  mutate(map_apache = as.numeric(map_apache)) %>%
  mutate(h1_heartrate_max = as.numeric(h1_heartrate_max)) %>%
  mutate(d1_resprate_max = as.numeric(d1_resprate_max)) %>%
  mutate(d1_potassium_max = as.numeric(d1_potassium_max)) %>%
  mutate(d1_creatinine_max = as.numeric(d1_creatinine_max)) %>%
  mutate(d1_hematocrit_max = as.numeric(d1_hematocrit_max)) %>%
  mutate(sodium_apache = as.numeric(sodium_apache)) %>%
  mutate(wbc_apache = as.numeric(wbc_apache)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(pre_icu_los_days = as.numeric(pre_icu_los_days)) %>%
  mutate(bmi = as.numeric(bmi))

## Save cleaned dataset 





###############
# MODELING ----
###############

## Fit initial Random Forest model ----

## Tune Random Forest Model ----
### Set seed 
set.seed(123)

### Split data set into train and test 
train <- df %>% dplyr::sample_frac(0.80)
test <- dplyr::anti_join(df, train, by "ID")

## Drop ID column ----
df <- df[, -which(names(df) == "ID")]

### Separating X and Y 
train_x <- 
train_y <- 

## Fit final rf model ----

## Get accuracy ----

###############
# FIGURES ----

## Feature importance ----

## Boxplot for most important features vs. class (x-axis) ----

## Here, either plot the most important features directly (like mean_gmrt_24), 

## or calculate the mean across all trials for each metric, by group (class = patients/healthy)