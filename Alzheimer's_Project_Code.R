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
library(randomForest)
library(caret)

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
### Set seed
set.seed(777)

### Split data set into train and test
train <- df %>% dplyr::sample_frac(0.75)
test <- dplyr::anti_join(df, train, by = "ID")

## Drop ID column ----
train <- train[,-which(names(train) == "ID")]
test <- test[,-which(names(test) == "ID")]

### Separating X and Y
train_x <- train %>% select(-class)
test_x <- test %>% select(-class)
train_y <- train %>% select(class)
test_y <- test %>% select(class)
test_y <- unlist(test_y)
train_y <- unlist(train_y)

# Fit random forest
rf_model <-
  randomForest(
    x = train_x,
    y = train_y,
    xtest = test_x,
    ytest = test_y,
    importance = TRUE,
    ntree = 2000
  )

## Tune Random Forest Model ----
mtry <- tuneRF(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  ntreeTry = 300,
  stepFactor = 1.5,
  improve = 0.001,
  trace = TRUE ,
  plot = TRUE
)

# Saving and printing best value tree
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

### Tuning the number of trees
rf_res_df <-
  data.frame(TRAINING_ERROR = rf_model$err.rate[, 1],
             ITERATION = c(1:2000)) %>%
  mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))

best_nrounds <- rf_res_df %>%
  filter(MIN) %>%
  pull(ITERATION)

print(best_nrounds)


### Fitting final model using best value
rf_model2 <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 31,
    importance = TRUE,
    ntree = 600
  )

rf_model2
### Error rate: 8.57%

### Feature importance

rf_features <- as.data.frame(varImp(rf_model2))
colnames(rf_features) <- "rf_imp"

## FIT FINAL MODEL ----
keep_features <- rf_features %>% select(rf_imp) %>% filter(rf_imp > 0)
imp_features <- rownames(keep_features)

train_x <- train_x %>% select(all_of(imp_features))
test_x <- test_x %>% select(all_of(imp_features))

## Tuning model
mtry <- tuneRF(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  ntreeTry = 5000,
  stepFactor = 1.5,
  improve = 0.01,
  trace = TRUE ,
  plot = TRUE
)

# Saving and printing best value tree
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# ### Tuning the number of trees
# rf_res_df <-
#   data.frame(TRAINING_ERROR = rf_model2$err.rate[, 1],
#              ITERATION = c(1:5000)) %>%
#   mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))
# 
# best_nrounds <- rf_res_df %>%
#   filter(MIN) %>%
#   pull(ITERATION)
# 
# print(best_nrounds)

### Fitting final model using best value
rf_final_model <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 19,
    importance = TRUE,
    ntree = 5000
  )
rf_final_model

## convert rownames to column
rf_features <- as.data.frame(varImp(rf_final_model))
rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp))



# Making features plottable
plot <- features %>%
  ggplot(aes(
    x = rf_imp,
    y = feature,
    color = "#2E86AB"
  )) +
  # Creates a point for the feature importance
  geom_point(position = position_dodge(0.5))

print(plot)
plot +
  # Connecting line between 0 and the feature
  geom_linerange(aes(xmin = 0, xmax = rf_imp),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  # Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "grey70") +
  # Adjust the scale if you need to based on your importance
  scale_x_continuous(limits = c(-1, 5)) +
  # Label the x and y axes
  labs(x = "Importance", y = "Feature") +
  # Make the theme pretty
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif")) +
  guides(color = guide_legend(title = NULL)) +
  # Plot them in order of importance
  scale_y_discrete(limits = features$feature[order(features$rf_imp, decreasing = FALSE)])


# Subset the 'features$feature' vector to only include the top 10 features

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