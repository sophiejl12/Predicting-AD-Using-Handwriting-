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