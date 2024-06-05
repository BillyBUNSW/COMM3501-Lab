# Question 25

library(readxl) 
TV_data <- read_excel("jaggia_ba_2e_ch08_data.xlsx", sheet="Television") 
TV_fit <- lm(GPA ~ Hours + I(Hours^2), data=TV_data) 
summary(TV_fit) 
plot(TV_data$Hours, TV_data$GPA)

#The quadratic term is justified since the quadratic variable (Hours)^2 is statistically 
#significant (p value = 0.00) at any reasonable significance level.  

#The maximum GPA is reached when the number of TV hours is -b1/2b0 =9.36 hr 
#per week. Therefore, this study suggests that 9.36 is the optimal number of hours of TV for 
#middle school students to maximize their GPA. 

# Question 41

# Hold-out method
IceCream <-  read_excel("jaggia_ba_2e_ch08_data.xlsx", sheet="IceCream") 
IceCream_Train <- IceCream[1:24,]
IceCream_Valid <- IceCream[25:35,] 

# MODEL 1

Model1 <- lm(Income ~ Hours + Hot + Holiday, data = IceCream_Train) 
summary(Model1) 
Pred1 <- predict(Model1, IceCream_Valid) 
RMSE1 = sqrt(mean((IceCream_Valid$Income-Pred1)^2)) 

# MODEL 2 - includes interaction term

Model2 <- lm(Income ~ Hours + Hot + Holiday + Hot*Holiday, data = IceCream_Train) 
summary(Model2) 
Pred2 <- predict(Model2, IceCream_Valid) 
RMSE2 = sqrt(mean((IceCream_Valid$Income-Pred2)^2)) 

# Compare RMSEs, model 2 has higher predictability
RMSE1; RMSE2

#Cross-validation
library(caret) 
train.control <- trainControl(method = "cv", number=5) 
model3 <- train(Income ~ Hours + Hot + Holiday,  
                data = TData, method = "lm",  trControl = train.control) 
summary(model3) 
print(model3) 
model4 <- train(Income ~ Hours + Hot + Holiday + Hot*Holiday, 
                data = TData, method = "lm", 
                trControl = train.control) 
summary(model4) 


# Question 43
library(readxl) 
library(caret) 
Default <- read_excel("jaggia_ba_2e_ch09_data.xlsx", sheet="Default") 
# Partition the data
Default_Train <- Default[1:300,]
Default_Valid <- Default[301:400,] 

logi_model <- glm(Default ~ LTV + FICO + Age, family = binomial, data = Default_Train) 
default_probs <- predict(logi_model, Default_Valid, type = "response")

# Creating predictions based on natural decision boundary 50%
default_pred1 <- ifelse(default_probs >= 0.5, 1,0) 

# Putting predictions and reference data from validation set in same data-frame
result <- data.frame(predicted=as.factor(default_pred1), default=as.factor(Default_Valid$Default)) 
confusionMatrix(result$predicted, result$default, positive="1") 

# Changing decision boundary from 50% to the proportion of positive cases
default_pred2 <- ifelse(default_probs >= mean(Default_Train$Default), 1,0) 
result2 <- data.frame(predicted=as.factor(default_pred2), default=as.factor(Default_Valid$Default)) 
confusionMatrix(result2$predicted,result2$default, positive="1")  

# It might be better to use the proportion as decision boundary because the sensitivity increases significantlu
