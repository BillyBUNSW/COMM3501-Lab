library(readxl) 
library(tidyverse) 
library(class) 
library(caret)
library(gains)
library(pROC) 
library(ggplot2) 


#Q 9

CFA_Data <- read_excel("jaggia_ba_2e_ch12_data.xlsx", sheet="CFA_Data") 
CFA_Score <- read_excel("jaggia_ba_2e_ch12_data.xlsx", sheet="CFA_Score") 

ggplot(data=CFA_Data)+ 
  geom_text(aes(x=GPA, y=Experience-0.5, label=rownames(CFA_Data), color=Pass))+ 
  geom_point(mapping=aes(x=GPA, y=Experience, color=Pass))+ 
  theme(legend.position = "none")

# select is for subsetting columns
# slice is for subsetting rows
# scaling parameters
scaled_CFA <- CFA_Data %>% select(-Pass) %>% scale %>% 
  as_tibble %>% mutate(Pass = as.factor(CFA_Data$Pass))

set.seed(1)
# sample training indices
trainIndex_CFA <- createDataPartition(scaled_CFA$Pass, p = 0.6, list = FALSE)

# training/validation split
train_CFA <- scaled_CFA[trainIndex_CFA,]
valid_CFA <- scaled_CFA[-trainIndex_CFA,]

# specify cross validation amount
CFACtrl <- trainControl(method = "cv", number = 10)
# specify range of tuning parameters
CFAkGrid <- expand.grid(.k = 1:10)

CFA_KNN <- train(Pass ~ ., data = train_CFA, method = "knn", trControl = CFACtrl, 
                 tuneGrid = CFAkGrid)
print(CFA_KNN) 

# b

KNN_Probs <- predict(CFA_KNN, newdata = valid_CFA, k=8) 
confusionMatrix(KNN_Probs, valid_CFA$Pass, positive = '1')

# c

scaled_CFA_score <- CFA_Score %>% scale %>% as_tibble
applicant_KNN_predictions <- predict(CFA_KNN, newdata = scaled_CFA_score, k=8) 
applicant_predictions <- CFA_Score %>% mutate(Pass_predictions =applicant_KNN_predictions )





# Q 20
library(klaR) 

# read in character type data as factors
Q20Data <- read_excel("jaggia_ba_2e_ch12_data.xlsx", sheet="Exercise_12.20_Data") %>%
  mutate(across(where(is.character), as.factor))
Q20Score <- read_excel("jaggia_ba_2e_ch12_data.xlsx", sheet="Exercise_12.20_Score") 

Q20Data$y <- as.factor(ifelse(Q20Data$y == "Yes", 1, 0))
# a)

set.seed(1)

# sample training indices
trainIndex_Q20 <- createDataPartition(Q20Data$y, p = 0.6, list = FALSE)

# training/validation split
train_Q20 <- Q20Data[trainIndex_Q20,]
valid_Q20 <- Q20Data[-trainIndex_Q20,]

# set cross-validation parameters
Q20Ctrl <- trainControl(method = "cv", number = 10)
# train the naive bayes
Q20_NB <- train(y ~ ., data = train_Q20, method = "nb", trControl = Q20Ctrl)

# predict on the validation set
Q20_probs <- predict(Q20_NB, newdata = valid_Q20, type = "prob") 
Q20_Pred <- as.factor(ifelse(Q20_probs[,2] > 0.5, 1, 0))
confusionMatrix(Q20_Pred, valid_Q20$y, positive = '1')

# b

# gains table can only take in numeric arguments
valid_Q20$y <- as.integer(valid_Q20$y) - 1

# Calculate gains table with an appropriate number of groups
distinct_predicted_values <- length(unique(Q20_probs[, 2]))
gains_result <- gains(valid_Q20$y, Q20_probs[, 2], groups = distinct_predicted_values)

# convert into a table
gains_table <- tibble(
  depth = gains_result$depth,
  obs = gains_result$obs,
  cume.obs = gains_result$cume.obs,
  mean.resp = gains_result$mean.resp,
  cume.mean.resp = gains_result$cume.mean.resp,
  cume.pct.of.total = gains_result$cume.pct.of.total,
  lift = gains_result$mean.resp / mean(valid_Q20$y),
  cumulative_gain = gains_result$cume.pct.of.total * 100,
  cumulative_lift = gains_result$cume.mean.resp / mean(valid_Q20$y),
  group = 1:distinct_predicted_values
) %>%
  mutate(cumulative_cases_pct = cume.obs / max(cume.obs) * 100)

# Add a dummy data point for (0, 0)
dummy_point <- tibble(
  cume.obs = 0,
  cumulative_gain = 0,
  cumulative_cases_pct = 0
)

# Plot the gains table
ggplot(bind_rows(dummy_point, gains_table), aes(x = cume.obs)) +
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



# Lift chart

# Plot using ggplot2
ggplot(gains_table, aes(x = factor(depth), y = lift)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Decile-Wise Lift Chart",
    x = "Percentile",
    y = "Lift"
  ) +
  ylim(0, 3) +
  theme_minimal()

#c
roc_object <- roc(valid_Q20$y, Q20_probs[,2]) 
plot.roc(roc_object) 
auc(roc_object) 


# d
score_probs <- predict(Q20_NB, newdata = Q20Score, type = "prob") 
score_Pred <- as.factor(ifelse(score_probs[,2] > 0.5, "Yes", "No"))









# Q30
int_data <- read_excel("jaggia_ba_2e_ch12_data.xlsx", sheet="International") %>%
  mutate(across(where(is.character), as.factor))
int_data$Accept <- as.factor(int_data$Accept)

set.seed(1)

# a

# sample training indices
trainIndex_int <- createDataPartition(int_data$Accept, p = 0.6, list = FALSE)

# training/validation split
train_int <- int_data[trainIndex_int,]
valid_int <- int_data[-trainIndex_int,]

# set cross-validation parameters
intCtrl <- trainControl(method = "cv", number = 10)
# train the naive bayes
int_NB <- train(Accept ~ ., data = train_int, method = "nb", trControl = intCtrl)

# predict on the validation set
int_probs <- predict(int_NB, newdata = valid_int, type = "prob") 
int_Pred <- as.factor(ifelse(int_probs[,2] > 0.5, 1, 0))
confusionMatrix(int_Pred, valid_int$Accept, positive = '1')

# b
roc_object_int <- roc(valid_int$Accept, int_probs[,2]) 
plot.roc(roc_object_int) 
auc(roc_object_int) 


# c

# gains table can only take in numeric arguments
valid_int$Accept <- as.integer(valid_int$Accept) - 1

# Calculate gains table with an appropriate number of groups
distinct_predicted_values <- length(unique(int_probs[, 2]))
gains_result <- gains(valid_int$Accept, int_probs[, 2], groups = distinct_predicted_values)

# convert into a table
gains_table <- tibble(
  depth = gains_result$depth,
  obs = gains_result$obs,
  cume.obs = gains_result$cume.obs,
  mean.resp = gains_result$mean.resp,
  cume.mean.resp = gains_result$cume.mean.resp,
  cume.pct.of.total = gains_result$cume.pct.of.total,
  lift = gains_result$mean.resp / mean(valid_int$Accept),
  cumulative_gain = gains_result$cume.pct.of.total * 100,
  cumulative_lift = gains_result$cume.mean.resp / mean(valid_int$Accept),
  group = 1:distinct_predicted_values
) %>%
  mutate(cumulative_cases_pct = cume.obs / max(cume.obs) * 100)

# Add a dummy data point for (0, 0)
dummy_point <- tibble(
  cume.obs = 0,
  cumulative_gain = 0,
  cumulative_cases_pct = 0
)

# Plot the gains table
ggplot(bind_rows(dummy_point, gains_table), aes(x = cume.obs)) +
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



# Lift chart

# Plot using ggplot2
ggplot(gains_table, aes(x = factor(depth), y = lift)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Decile-Wise Lift Chart",
    x = "Percentile",
    y = "Lift"
  ) +
  ylim(0, 3) +
  theme_minimal()

