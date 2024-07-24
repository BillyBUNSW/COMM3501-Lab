# Load necessary libraries
required_packages <- c("readxl", "tidyverse", "rpart", "rpart.plot", "caret", 
                       "gains", "pROC", "adabag", "xgboost","randomForest")

# Function to check for missing packages and install them if necessary
check_and_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Check and install required packages
check_and_install_packages(required_packages)


# Q 9

Q9_data <- read_excel("jaggia_ba_2e_ch13_data.xlsx", sheet="Exercise_13.9_Data") 
Q9_score <- read_excel("jaggia_ba_2e_ch13_data.xlsx", sheet="Exercise_13.9_Score") 
Q9_data$y <- as.factor(Q9_data$y)

# a)
 
# Set seed for reproducibility
set.seed(1)

# Partition the data into training (70%) and validation (30%) sets
Q9_train_index <- createDataPartition(Q9_data$y, p = 0.7, list = FALSE)
Q9_train <- Q9_data[Q9_train_index, ]
Q9_valid <- Q9_data[-Q9_train_index, ]

# forming a default tree
default_tree <- rpart(y ~ ., data = Q9_train, method = "class")

summary(Q9_default_tree)
prp(default_tree, 
    type = 1, 
    extra = 1, 
    fallen.leaves = FALSE, 
    box.palette = c("red", "yellow", "green"), 
    varlen = 0, 
    faclen = 0)


# b)

# training a full tree
Q9_full_tree <- rpart(y ~ ., data = Q9_train, method = "class", 
                      cp = 0, 
                      minbucket = 1,
                      minsplit = 2)
prp(Q9_full_tree, 
    type = 1, 
    extra = 1, 
    fallen.leaves = FALSE, 
    box.palette = c("red", "yellow", "green"), 
    varlen = 0, 
    faclen = 0)
printcp(Q9_full_tree)

min_error_index <- which.min(Q9_full_tree$cptable[,"xerror"])
min_error_cp <- Q9_full_tree$cptable[min_error_index,"CP"]
Q9_full_tree$cptable[min_error_index,]
# minimum error tree has 2 splits 

# c)

min_error <- Q9_full_tree$cptable[min_error_index, "xerror"]
se <- Q9_full_tree$cptable[min_error_index, "xstd"]
best_pruned_index <- which(Q9_full_tree$cptable[,"xerror"] <= (min_error + se))[1]
best_pruned_cp <- Q9_full_tree$cptable[best_pruned_index, "CP"]
Q9_full_tree$cptable[best_pruned_index,]
# in this case, the best pruned tree has one splits


# d)
Q9_minimum_error_tree <- prune(Q9_full_tree, cp = min_error_cp)
prp(Q9_minimum_error_tree, 
    type = 1, 
    extra = 1, 
    fallen.leaves = FALSE, 
    box.palette = c("red", "yellow", "green"), 
    varlen = 0, 
    faclen = 0)

# e) 

predicted_Q9 <- predict(Q9_minimum_error_tree, Q9_valid, type = "class", positive = "1")
confusionMatrix(predicted_Q9, Q9_valid$y)


# f)

Q9_probs <- predict(Q9_minimum_error_tree, Q9_valid, type = "prob")


calculate_and_plot_gains <- function(model, validation_data) {
  # Infer the target column name from the model object
  target_column <- all.vars(model$call$formula)[1]
  
  # Predict the probability using the validation data
  predicted_prob <- predict(model, validation_data, type = "prob")[,2]
  
  # Calculate gains table with an appropriate number of groups
  distinct_predicted_values <- length(unique(predicted_prob))
  num_groups <- min(distinct_predicted_values, 10) # Adjusting to the smaller of distinct values or 10
  
  gains_result <- gains(as.numeric(validation_data[[target_column]]), predicted_prob, groups = num_groups)
  
  # Manually create a tibble from the gains_result components
  gains_table <- tibble(
    depth = gains_result$depth,
    obs = gains_result$obs,
    cume.obs = gains_result$cume.obs,
    mean.resp = gains_result$mean.resp,
    cume.mean.resp = gains_result$cume.mean.resp,
    cume.pct.of.total = gains_result$cume.pct.of.total,
    lift = gains_result$mean.resp / mean(as.numeric(validation_data[[target_column]])),
    cumulative_gain = gains_result$cume.pct.of.total * 100,
    cumulative_lift = gains_result$cume.mean.resp / mean(as.numeric(validation_data[[target_column]])),
    group = gains_result$depth
  ) %>%
    mutate(cumulative_cases_pct = cume.obs / max(cume.obs) * 100)
  
  # Add a dummy data point for (0, 0)
  dummy_point <- tibble(
    cume.obs = 0,
    cumulative_gain = 0,
    cumulative_cases_pct = 0
  )
  
  # Combine the dummy point with the gains_table
  plot_data <- bind_rows(dummy_point, gains_table)
  
  # Plot the gains chart
  gains_chart <- ggplot(plot_data, aes(x = cume.obs)) +
    geom_line(aes(y = cumulative_gain), color = "blue", size = 1) +
    geom_line(aes(y = cumulative_cases_pct), color = "red", size = 1, linetype = "dashed") +
    labs(
      title = "Gains Chart",
      x = "Number of Cases",
      y = "Cumulative"
    ) +
    theme_minimal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  print(gains_chart)
  
  ############# VERY IMPORTANT ###########################
  readline(prompt="Press [enter] to continue")
  ########################################################
  
  # Plot decile-wise lift chart using ggplot2
  lift_chart <- ggplot(gains_table, aes(x = factor(depth), y = lift)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = "Decile-Wise Lift Chart",
      x = "Percentile",
      y = "Lift"
    ) +
    theme_minimal()
  print(lift_chart)
  
  return(gains_table)
}

calculate_and_plot_gains(Q9_minimum_error_tree, Q9_valid)
#Yes, the entire lift curve lies above the baseline. The lift value of the leftmost bar is 1.11. This 
#implies that by selecting the top 47% of the validation cases with the highest predicted 
#probability of belonging to the target class, the model would identify 1.11 times as many target 
#class cases as if the cases are randomly selected.



# g)
roc_curve <- function(actuals, probs) {
roc_object <- roc(actuals, probs) 
plot.roc(roc_object) 
auc(roc_object) 
}
roc_curve(Q9_valid$y, Q9_probs[,2])

# h)

score_predictions <- predict(Q9_minimum_error_tree, Q9_score, type = "prob")





# Q 63

WFH_data <- read_excel("jaggia_ba_2e_ch13_data.xlsx", sheet="WorkFromHome") 
WFH_data$Performance <- as.factor(WFH_data$Performance) 
WFH_data$Management <- as.factor(WFH_data$Management) 

# a)
set.seed(1) 
WFH_Index <- createDataPartition(WFH_data$Performance, p=0.6, list=FALSE) 

WFH_train <- WFH_data[WFH_Index,] 
WFH_valid <- WFH_data[-WFH_Index,] 

set.seed(1) 
bagging_tree <- randomForest(Performance ~., data = WFH_train, ntree = 10000, mtry = 3, 
                             importance = TRUE) 

bagging_probs <- predict(bagging_tree, WFH_valid, type = "prob")
bagging_pred <- ifelse(bagging_probs[,2] >= 0.6, 1, 0) %>% as.factor

confusionMatrix(bagging_pred, WFH_valid$Performance)

# b)

set.seed(1) 
rf_tree <- randomForest(Performance ~., data = WFH_train, ntree = 10000, mtry = 2, 
                             importance = TRUE) 

rf_probs <- predict(rf_tree, WFH_valid, type = "prob")
rf_pred <- ifelse(rf_probs[,2] >= 0.6, 1, 0) %>% as.factor

confusionMatrix(rf_pred, WFH_valid$Performance)


# c)

calculate_and_plot_gains(rf_tree, WFH_valid)
calculate_and_plot_gains(bagging_tree, WFH_valid)
roc_curve(WFH_valid$Performance, bagging_probs[,2])
roc_curve(WFH_valid$Performance, rf_probs[,2])






# xgboost

WFH_numeric <- WFH_data %>%
  mutate(across(where(is.factor), as.numeric))

set.seed(1)
WFH_Index <- createDataPartition(WFH_numeric$Performance, p=0.6, list=FALSE) 

WFH_train_num <- WFH_numeric[WFH_Index,] 
WFH_valid_num <- WFH_numeric[-WFH_Index,] 

train_matrix <- xgb.DMatrix(data = as.matrix(WFH_train_num %>% select(-Performance)), label = WFH_train_num$Performance - 1)
valid_matrix <- xgb.DMatrix(data = as.matrix(WFH_valid_num %>% select(-Performance)), label = WFH_valid_num$Performance - 1)
# Set parameters for XGBoost
params <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  max_depth = 6,
  eta = 0.3,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 100,
  watchlist = list(train = train_matrix, eval = valid_matrix),
  verbose = 0
)


# tuning pArameter grid
param_grid <- expand.grid(
  nrounds = 100,
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = c(0.5, 0.75, 1),
  min_child_weight = c(1, 5, 10),
  subsample = c(0.5, 0.75, 1)
)

xgb_grid <- train(
  Performance ~ .,
  data = WFH_train,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid = param_grid
)

# Predict the class using the validation data
xgb_predictions <- predict(xgb_model, newdata = WFH_valid, type = "prob")
xgb_pred_class <- ifelse(xgb_predictions > 0.5, 1, 0)

xgb_predictions_tune <- predict(xgb_grid, newdata = WFH_valid, type = "prob")
xgb_pred_class_tune <- ifelse(xgb_predictions_tune[,2] > 0.5, 1, 0)

# Calculate and display a confusion matrix
conf_matrix <- confusionMatrix(as.factor(xgb_pred_class), as.factor(WFH_valid_num$Performance - 1))
confusionMatrix(as.factor(xgb_pred_class_tune), WFH_valid$Performance)

roc_curve(WFH_valid_num$Performance -1, xgb_predictions)
roc_curve(WFH_valid$Performance, xgb_predictions_tune[,2])
