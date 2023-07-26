###############
# SETUP ----
###############

## Set working directory ----
getwd()

## Sophie
#setwd("/Users/sophielawrence/Documents/Data_Mining_Project")

##Ria
setwd("/Users/ria/Downloads/Data_mining_project")

## Loading libraries ----
library("tidyverse")
library("dplyr")
library(randomForest)
library(caret)
install.packages("dataMaid")
library(dataMaid)

###############
# DATA CLEANING ----
###############

## Reading data ----
df <- read_csv("Alzheimers_data.csv")
head(df)
str(df)

## Changing class to factor ----
df$class <- factor(df$class)

# get rid of the outliers
df <- df[!(row.names(df) %in% c("169"))]

dataMaid::identifyOutliers(df$total_time23, nMax = 1)

## Saving cleaned dataset
write.csv(df, file = "Alzheimers_data_cleaned.csv", row.names = FALSE)

###############
# MODELING ----
###############

## Fitting initial Random Forest model ----
### Setting seed
set.seed(125)

### Splitting data set into train and test
train <- df %>% dplyr::sample_frac(0.80)
test <- dplyr::anti_join(df, train, by = "ID")

### Dropping ID column ----
train <- train[,-which(names(train) == "ID")]
test <- test[,-which(names(test) == "ID")]

### Separating X and Y and train and test
train_x <- train %>% select(-class)
test_x <- test %>% select(-class)
train_y <- train %>% select(class)
test_y <- test %>% select(class)
test_y <- unlist(test_y)
train_y <- unlist(train_y)

# Fitting random forest 
rf_model <- randomForest(x = train_x, y = train_y, xtest = test_x, ytest = test_y, importance = TRUE, ntree = 4000)

rf_model <-
  randomForest(
    x = train_x,
    y = train_y,
    xtest = test_x,
    ytest = test_y,
    importance = TRUE,
    ntree = 4000
  )

## Tuning Random Forest Model ----
mtry <- tuneRF(
  x = train_x,
  y = train_y,
  xtest = test_x,
  ytest = test_y,
  ntreeTry = 4000,
  stepFactor = 1.5,
  improve = 0.001,
  trace = TRUE ,
  plot = TRUE
)

## Saving and printing best value tree
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

## Tuning the number of trees
rf_res_df <-
  data.frame(TRAINING_ERROR = rf_model$err.rate[, 1],
             ITERATION = c(1:4000)) %>%
  mutate(MIN = TRAINING_ERROR == min(TRAINING_ERROR))

best_nrounds <- rf_res_df %>%
  filter(MIN) %>%
  pull(ITERATION)

print(best_nrounds)


# Fitting final model ----
rf_model2 <-
  randomForest(
    x = train_x,
    y = train_y,
    mtry = 14,
    importance = TRUE,
    ntree = 4000
  )

rf_model2
### Error rate: 11.517%

### Feature importance ----

rf_features <- as.data.frame(varImp(rf_model2))
colnames(rf_features) <- "rf_imp"

## Converting rownames to column
rf_features$feature <- rownames(rf_features)
## Selecting only relevant columns for mapping
features <- rf_features %>% dplyr::select(c(feature, rf_imp)) 
features <- features %>% filter(feature != "ID")

## Keeping only rows with values over 0 
keep_features <- rf_features %>% select(rf_imp) %>% filter(rf_imp > 0)
imp_features <- rownames(keep_features)


## convert rownames to column
#rf_features <- as.data.frame(varImp(rf_final_model))
#rf_features$feature <- rownames(rf_features)

## Selecting only relevant columns for mapping
#features <- rf_features %>% dplyr::select(c(feature, rf_imp))
#train_x <- train_x %>% select(all_of(imp_features))
#test_x <- test_x %>% select(all_of(imp_features))


### Dataset with total importance

# Using stringr to separate the text in the features column, with text in one column "group" and number in another column "num"
features <- features %>% mutate(group = stringr::str_remove_all(feature, pattern = "\\d+"))
  
  
# Calculating the total of "rf_imp" by group and the mean of "rf_imp"
features_grouped <- features %>%
  group_by(group) %>%
  mutate(total_imp = sum(rf_imp),
         mean_imp = mean(rf_imp)) %>% select(c(3:5)) %>%
  distinct(group, .keep_all = TRUE) %>%
  arrange(desc(total_imp))

# Making features plottable
top_10_features <- head(features_grouped, 10)
top_10_features$group <- recode(top_10_features$group,
                                "paper_time"          = "Paper Time",
                                "air_time"            = "Air Time",
                                "disp_index"          = "Disp Index",
                                "total_time"          = "Total Time",
                                "mean_jerk_on_paper"  = "Mean Jerk on Paper",
                                "mean_speed_on_paper" = "Mean Speed on Paper",
                                "mean_gmrt"           = "Mean GMRT",
                                "gmrt_on_paper"       = "GMRT on Paper",
                                "mean_acc_in_air"     = "Mean Acceleration in Air",
                                "mean_jerk_in_air"    = "Mean Jerk in Air")

### Ploting the feature importance
plot <- top_10_features %>%
  ggplot(aes(x = total_imp, y = group, color = "#2E86AB")) +
  # Creates a point for the feature importance
  geom_point(position = position_dodge(0.5)) 

print(plot)

plot +
  # Connecting line between 0 and the feature
  geom_linerange(aes(xmin = 0, xmax = total_imp),
                 linetype = "solid",
                 position = position_dodge(.5)) +
  # Vertical line at 0
  geom_vline(xintercept = 0,
             linetype = "solid",
             color = "grey70") +
  # Adjusting scale
  scale_x_continuous(limits = c(-1, 175 )) +
  # Label the x and y axes
  labs(x = "Importance", y = "group") +
  # Setting theme
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "serif", face = "bold" )) +
  guides(color = guide_legend(title = NULL)) +

  # Plot them in order of importance
  scale_y_discrete(limits = top_10_features$group[order(top_10_features$total_imp, decreasing = FALSE)])



# Plotting with ggplot
library(ggplot2)
library("patchwork")
# Improving plot

## Get accuracy ----



###############
# FIGURES ----

## Feature importance ----

## Boxplot for most important features vs. class (x-axis) ----

p2 <- ggplot(data=df, aes(x = class, y = total_time23)) + geom_boxplot()

## Here, either plot the most important features directly (like mean_gmrt_24),

## or calculate the mean across all trials for each metric, by group (class = patients/healthy)