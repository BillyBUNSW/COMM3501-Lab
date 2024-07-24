# remember to set working directory

library(readxl)
library(forecast)
library(tidyverse)
library("zoo")

# Question 5
foodtruck_data <- read_excel("jaggia_ba_2e_ch10_data.xlsx", sheet="Foodtruck") 
View(foodtruck_data)

#MA3

# Calculate rolling average using tidyverse functions
foodtruck_data <- foodtruck_data %>%
  mutate(MA3 = rowMeans(cbind(lag(Students,1), lag(Students, 2), lag(Students, 3)), na.rm = FALSE)) %>%
  mutate(e3 = Students - MA3)
# OR
rollmean(foodtruck_data$Students, k = 3, align = "right", fill = NA)
# Prediction for day 41 is 154.33333

# Calculates all error metrics
accuracy(foodtruck_data$MA3, foodtruck_data$Students)

# manual calculation
mse <- mean(foodtruck_data$e3^2, na.rm = TRUE)
mad <- mean(abs(foodtruck_data$e3), na.rm = TRUE)
mape <- mean(abs(foodtruck_data$e3 / foodtruck_data$Students) * 100, na.rm = TRUE)


#MA5

foodtruck_data <- foodtruck_data %>%
  mutate(MA5 = rowMeans(cbind(lag(Students,1), lag(Students, 2), 
                              lag(Students, 3), lag(Students, 4), lag(Students, 5)), na.rm = FALSE)) %>%
  mutate(e5 = Students - MA5)
# OR
rollmean(foodtruck_data$Students, k = 5, align = "right", fill = NA)
# Prediction for day 41 is 135.2

accuracy(foodtruck_data$MA5, foodtruck_data$Students)



# Question 35
#a) 

tax_data <- read_excel("jaggia_ba_2e_ch10_data.xlsx", sheet="Tax_Revenue") 
View(tax_data)

#Turn into a time series object
tax_series <- ts(tax_data$Revenue, start = c(2014,2), end = c(2018,10), frequency=12) 

#Partition into training and validation sets
tax_train <- window(tax_series, end = c(2017,10))
tax_valid <- window(tax_series, start = c(2017,11))

# Fit the models
lin_model <- tslm(tax_train ~ trend); summary(lin_model)
quad_model <- tslm(tax_train ~ trend + I(trend^2)); summary(quad_model)
cubic_model <- tslm(tax_train ~ trend + I(trend^2) + I(trend^3)); summary(cubic_model)

# predict the validation set using the models
lin_forecast <- forecast(lin_model, h = length(tax_valid))
quad_forecast <- forecast(quad_model, h = length(tax_valid))
cubic_forecast <- forecast(cubic_model, h = length(tax_valid))

# commpute the performance measures
accuracy(lin_forecast, tax_valid)
accuracy(quad_forecast, tax_valid)
accuracy(cubic_forecast, tax_valid)

# We prefer the linear model since it has the lowest test RMSE, MAE and MAPE. 
# The cubic model has over fit since itperformed the best on the training but not the test

#retrain best model on entire dataset for prediction
lin_model <- tslm(tax_series ~ trend); summary(lin_model)
lin_forecast <- forecast(lin_model, h = 1)


# Using ARIMA
library(tseries) 
best_arima <- auto.arima(tax_train, seasonal=FALSE)
summary(best_arima) # compare RMSE, MAE, MPE 
fitted(best_arima)
checkresiduals(best_arima) 
predict(best_arima, h = length(tax_valid))



# Question 18
sentiment_data <- read_excel("jaggia_ba_2e_ch10_data.xlsx", sheet="Sentiment") 
View(sentiment_data)


#Turn into a time series object
sentiment_train <- ts(sentiment_data$Sentiment, end = c(2017,12), frequency=12) 
sentiment_test <- ts(sentiment_data$Sentiment, start = c(2018,1), frequency=12) 

# fit a seasonal and non seasonal model
season_model <- tslm(sentiment_train ~ trend + season); summary(season_model)
no_season_model <- tslm(sentiment_train ~ trend); summary(no_season_model)

# forecast on the validation set
season_forecast <- forecast(season_model, h = length(sentiment_test))
no_season_forecast <- forecast(no_season_model, h = length(sentiment_test))

accuracy(season_forecast, sentiment_test)
accuracy(no_season_forecast, sentiment_test)

# retrain the better model on the entire dataset
no_season_model <- tslm(sentiment_series ~ trend); summary(no_season_model)
forecast(no_season_model, h = 1)
