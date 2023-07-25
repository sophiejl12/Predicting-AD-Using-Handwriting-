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

## Save cleaned dataset 
write.csv(df, file = "Alzheimers_data_cleaned.csv", row.names = FALSE)


###############
# MODELING ----
###############

## Fit initial Random Forest model ----

## Tune Random Forest Model ----
### Randomly sort data 
library(dplyr)



### Set seed 
set.seed(123)

### Split data set into train and test 
train <- df %>% dplyr::sample_frac(0.80)
test <- dplyr::anti_join(df, train, by = "ID")

## Drop ID column ----
train <- train[, -which(names(train) == "ID")]
test <- test[, -which(names(test) == "ID")]


### Separating X and Y 
train_x <- test %>% select(-class)
test_x <- test %>% select(-class)
train_y <- test %>% select(class)
test_y <- test %>% select(class)
test_y <-unlist(test_y)
train_y <- unlist(train_y)

## Fitting rf model ----
# Loading random forest 
library(randomForest)
rf_model <- randomForest(x = train_x, y = train_y, xtest = test_x, ytest = test_y, importance = TRUE, ntree = 2000)

## Tuning model
mtry <- tuneRF(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  ntreeTry = 300,
  stepFactor =1.5,
  improve =0.01,
  trace =TRUE ,
  plot = TRUE
)

# Saving and printing best value tree
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

### Tuning the number of trees
rf_res_df <-
  data.frame(
    TRAINING_ERROR = rf_model$err.rate[,1],
    ITERATION = c(1:2000)
  ) %>%
  mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))

best_nrounds <- rf_res_df %>%
  filter(MIN) %>%
  pull(ITERATION)

print(best_nrounds)


### Fitting final model using best value 
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 14,
    importance = TRUE,
    ntree = 2000
  )
rf_final_model

### Feature importance



# Making features plottable


## convert rownames to column


## Selecting only relevant columns for mapping


# Plotting with ggplot 


# Improving plot 

```

## Get accuracy ----

###############
# FIGURES ----

## Feature importance ----

## Boxplot for most important features vs. class (x-axis) ----

## Here, either plot the most important features directly (like mean_gmrt_24), 

## or calculate the mean across all trials for each metric, by group (class = patients/healthy)