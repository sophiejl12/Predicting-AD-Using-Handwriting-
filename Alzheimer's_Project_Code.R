###############
# SETUP ----
###############

## Set working directory ----
getwd()

## Sophie
setwd("/Users/sophielawrence/Documents/Data_Mining_Project")

##Ria
#setwd("/Users/ria/Downloads/Data_mining_project")

## Loading libraries ----
library("tidyverse")
library("dplyr")
library(randomForest)
library(caret)

library(rstatix)

###############
# DATA CLEANING ----
###############

## Reading data ----
df <- read_csv("Alzheimers_data.csv")
head(df)
str(df)

## Changing class to factor ----
df$class <- factor(df$class)

## Replace outliers with mean values

df <- df %>%
  group_by(class) %>%
  mutate(across(where(is.numeric), ~ ifelse(is_extreme(.), mean(., na.rm = TRUE), .)))


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
    mtry = 31,
    importance = TRUE,
    ntree = 1500
  )

rf_model2

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

p1 <-plot +
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

# Adding overall mean pressure to original dataset
df_total3 <- df %>% mutate(pressure_mean_total = (pressure_mean1 + pressure_mean2 + pressure_mean3 + pressure_mean4 + pressure_mean5+pressure_mean6+pressure_mean7+pressure_mean8+pressure_mean9+pressure_mean10+pressure_mean11+pressure_mean12+pressure_mean13+pressure_mean14+pressure_mean15+pressure_mean16+pressure_mean17+pressure_mean18+pressure_mean19+pressure_mean20+pressure_mean21+pressure_mean22+pressure_mean23+pressure_mean24+pressure_mean25))
summary(df_total3)

# Adding total time to original dataset
df_total <- df %>% mutate(total_time = (total_time1 + total_time2 + total_time3 + total_time4 + total_time5+total_time6+total_time7+total_time8+total_time9+total_time10+total_time11+total_time12+total_time13+total_time14+total_time15+total_time16+total_time17+total_time18+total_time19+total_time20+total_time21+total_time22+total_time23+total_time24+total_time25))
summary(df_total)

# Excluding outliers 
df_total <- df_total %>%
  group_by(class) %>%
  mutate(across(where(is.numeric), ~ ifelse(is_extreme(.), mean(., na.rm = TRUE), .)))

# Isolating total time 
df_total2 <- df_total %>% select(c(ID, total_time))

###############
# FIGURES ----

# Boxplot for mean pressure vs. class ----
p2 <- ggplot(data = df_total3, aes(x = class, y = pressure_mean_total, fill = class)) + geom_boxplot() + theme_minimal() +theme(legend.position = "none",
                                                                                                                          text = element_text(family = "serif", face = "bold" )) +
  guides(color = guide_legend(title = NULL)) +
  ggtitle("Mean Pressure for Healthy Participants and Alzheimer's Patients") 
plot(p2)

# Boxplot for total time vs. class ----
p3 <-
  ggplot(data = df_total, aes(x = total_time, fill = class)) + geom_density(alpha = 0.5) + theme_minimal() + theme(legend.position = "none",
                                                                                                                   text = element_text(family = "serif", face = "bold" )) +
  guides(color = guide_legend(title = NULL)) +
  ggtitle("Total Writing Time of Healthy Participants Compared to Patients")
plot(p3)
