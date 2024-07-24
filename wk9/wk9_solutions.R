required_packages <- c("caret", "ANN2", "pROC", "tidyverse", "fastDummies", "neuralnet", "ANN2")

# Function to check for missing packages and install them if necessary
check_and_install_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}
check_and_install_packages(required_packages)

load("train_credit.Rdata")
load("test_credit.Rdata")

train_cre <- xtrain0
test_cre <- xtest0

summary(train_cre)


numeric_indices <- which(sapply(train_cre, is.numeric))

# max min scaling

for (i in numeric_indices) {
  test_cre[,i] <- (test_cre[,i] - min(train_cre[,i]))/(max(train_cre[,i]) - min(train_cre[,i]))
  train_cre[,i] <- (train_cre[,i] - min(train_cre[,i]))/(max(train_cre[,i]) - min(train_cre[,i]))
}


# dummy vars
factor_variables <- c("SEX", "EDUCATION", "MARRIAGE")
train_cre <- dummy_cols(train_cre, factor_variables, remove_first_dummy = TRUE)
test_cre <- dummy_cols(test_cre, factor_variables, remove_first_dummy = TRUE)

names(train_cre)
names(test_cre)
nrow(train_cre)
nrow(test_cre)
# NN
NN_cre <- neuralnetwork(X = train_cre[, c(1,5,12:23,25:30)],
                        y = train_cre[, 24],
                        hidden.layers = c(5,3),
                        optim.type = 'adam',
                        learn.rates = 0.005,
                        val.prop = 0,
                        regression = FALSE,
                        standardize = FALSE,
                        verbose = FALSE,
                        random.seed = 2024)

creglm <- glm(default ~ LIMIT_BAL+AGE+BILL_AMT1+BILL_AMT2+BILL_AMT3+
                BILL_AMT4+BILL_AMT5+BILL_AMT6
              +PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5
              +PAY_AMT6+SEX_Male+EDUCATION_2+EDUCATION_3+EDUCATION_4+
                MARRIAGE_2+MARRIAGE_3, data = train_cre, family = binomial)
summary(creglm)

# hyper parameter 
train_control <- trainControl(
  method = "cv",
  number = 3,
  search = "grid"
)

param_grid <- expand.grid(
  size  = c(5,10),
  decay = c(0,0.001,0.01)
)

detach("package:ANN2", unload = TRUE)
# Train the model
set.seed(123)
model <- train(
  default ~ LIMIT_BAL+AGE+BILL_AMT1+BILL_AMT2+BILL_AMT3 + BILL_AMT4+BILL_AMT5+BILL_AMT6
  +PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5
  +PAY_AMT6+SEX_Male+EDUCATION_2+EDUCATION_3+EDUCATION_4+
    MARRIAGE_2+MARRIAGE_3,            
  data = train_cre,          
  method = "nnet",            
  trControl = train_control,  
  tuneGrid = param_grid,
  linout = FALSE,            
  trace = FALSE            
)
library(ANN2)

print(model$bestTune)

# make predictions
nn_probs <- predict(NN_cre, newdata = test_cre[, c(1,5,12:23,25:30)])$probabilities[,2]
nn_pred <- as.factor(ifelse(nn_probs > 0.3, 1, 0))

tune_probs <- predict(model, newdata = test_cre[, c(1,5,12:23,25:30)], type = "prob")[,2]
tune_pred <- as.factor(ifelse(tune_probs > 0.3, 1, 0))

glm_probs <- predict(creglm, newdata = test_cre[, c(1,5,12:23,25:30)], type = "response")
glm_pred <- as.factor(ifelse(glm_probs > 0.3, 1, 0))

confusionMatrix(nn_pred, test_cre$default)
confusionMatrix(tune_pred, test_cre$default)
confusionMatrix(glm_pred, test_cre$default)

roc_object_nn <- roc(test_cre$default, nn_probs) 
plot.roc(roc_object_nn) 
auc(roc_object_nn) 

roc_object_tune <- roc(test_cre$default, tune_probs) 
plot.roc(roc_object_tune) 
auc(roc_object_tune) 


roc_object_glm <- roc(test_cre$default, glm_probs) 
plot.roc(roc_object_glm) 
auc(roc_object_glm) 



# 2
load("Train-set.RData")#newtrain
load("Test-set.RData")#newtest
train_claims<-newtrain
test_claims<-newtest
summary(train_claims)

#scale
numerical_indices <- c(3, 5, 6, 7)
for (i in numerical_indices) {
  test_claims[,i] <- (test_claims[,i] - min(train_claims[,i]))/(max(train_claims[,i]) - min(train_claims[,i]))
  train_claims[,i] <- (train_claims[,i] - min(train_claims[,i]))/(max(train_claims[,i]) - min(train_claims[,i]))
}

#dummy
factor_variables <- c("Area", "VehGas")
train_claims <- dummy_cols(train_claims, factor_variables, remove_first_dummy = TRUE)
test_claims <- dummy_cols(test_claims, factor_variables, remove_first_dummy = TRUE)

names(train_claims)

set.seed(123)
# NN
NN_claims <- neuralnetwork(X = train_claims[, c(3,7,12:17)],
                        y = train_claims[, 2],
                        hidden.layers = c(2,2),
                        optim.type = 'adam',
                        learn.rates = 0.005,
                        val.prop = 0,
                        regression = TRUE,
                        standardize = FALSE,
                        verbose = FALSE,
                        random.seed = 2024,
                        loss.type = "squared",
                        activ.functions = "relu")


# hyper parameter tuning
train_control <- trainControl(
  method = "cv",
  number = 3,
  search = "grid"
)

param_grid <- expand.grid(
  size  = c(5,10),
  decay = c(0,0.001,0.01)
)

detach("package:ANN2", unload = TRUE)
# Train the model
set.seed(123)  # For reproducibility
train_claims$ClaimNb <- as.numeric(train_claims$ClaimNb)
model <- train(
  ClaimNb~Exposure+DrivAge+Area_B+Area_C+Area_D+Area_E+Area_F+VehGas_Regular,             
  data = train_claims,         
  method = "nnet",           
  trControl = train_control,  
  tuneGrid = param_grid,     
  linout = FALSE,       
  trace = FALSE              
)
print(model$bestTune)
library(ANN2)

claimsglm<-glm(ClaimNb~Exposure+DrivAge+Area_B+Area_C+Area_D+Area_E+Area_F+VehGas_Regular,data=train_claims,
               family=poisson(link="log"))

test <- select(test_claims,c("Exposure","DrivAge","Area_B","Area_C","Area_D","Area_E","Area_F","VehGas_Regular"))

glm.results<-predict(claimsglm,newdata = test,type= "response")
tune.result <- predict(model, newdata = test)
nn.results<-predict(NN_claims,newdata = test,type= "response")

results<-data.frame(actual=test_claims$ClaimNb,
                    glm.prediction=glm.results,
                    nn.prediction = nn.results$predictions,
                    tune.prediction = tune.result)
mse<-data.frame(glm.mse=sum((results[,1]-results[,2])^2)/length(results[,2]),
                nn.mse = sum((results[,1]-results[,3])^2)/length(results[,3]),
                tune.mse = sum((results[,1]-results[,4])^2)/length(results[,4]))
