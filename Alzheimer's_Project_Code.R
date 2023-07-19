###############
# SETUP ----
###############

## Set working directory ----
getwd()

## Sophie
#setwd("/Users/sophielawrence/Documents/Data_Mining_Project")

## Ria
setwd("/Users/ria/Downloads/Data_mining_project")

## Load libraries ----
library("tidyverse")


###############
# DATA CLEANING ----
###############

## Read data ----
df <- read_csv("Alzheimers_data.csv")

str(df)

## Change class to factor ----

## Drop ID column ----
df <- df[, -which(names(df) == "ID")]

###############
# MODELING ----
###############

## Fit initial Random Forest model ----

## Tune Random Forest Model ----

## Fit final rf model ----

## Get accuracy ----

###############
# FIGURES ----

## Feature importance ----

## Boxplot for most important features vs. class (x-axis) ----
## Here, either plot the most important features directly (like mean_gmrt_24), 
## or calculate the mean across all trials for each metric, by group (class = patients/healthy)