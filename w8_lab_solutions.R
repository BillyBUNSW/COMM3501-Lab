library(tidyverse)
library(caret)
library(randomForest)
library(ROSE) # for random over- and under-sampling
library(smotefamily) #for SMOTE oversampling

sba <- read_csv("sba_loans.csv")
sba <- data.frame(sba)#to transform the data into a data frame (will help with further steps)

# no need to encode any variables as numeric dummy variables. 

# encode the outcome/target variable as a factor variable
df$Default <- as.factor(df$Default)

# split into training (80%) and validation (20%) data 
set.seed(1) 
sba_index<- createDataPartition(df$Default, p=0.8, list=FALSE)
sba_train <- df[sba_index,]
sba_validation <- df[-sba_index,]

##!! Prefer scaling after splitting!!
# standardising variables: make sure to use the training data 
# means/SDs for scaling the validation set data. Can do this 'by hand' as 
# shown below, or using the preProcess() and predict() functions as shown in 
# tutorials
for(x in 1:(ncol(sba_train)-1)){
  sba_validation[,x] <- (sba_validation[,x] - mean(sba_train[,x]))/sd(sba_train[,x])
}
sba_train[,-ncol(sba_train)] <- scale(sba_train[,-ncol(sba_train)])

##!! Comment on scaling on dummy variables: for models like RF it does not make a change but scaling a dummy variable may not be appropriate for some models (i.e. losse of interpretability)

##RANFOM FOREST ON IMBALANCED DATA

# compute the square-root of the number of available predictors, to use as 
# input for randomForest() calls
sqrt.num.vars = floor(sqrt(ncol(df)-1))

# now fit randomForest on original (imbalanced) data
table(sba_train$Default) # observe imbalancedness 
table(sba_train$Default)/nrow(sba_train) # imbalancedness in proportional terms
minority_prop <- (table(sba_train$Default)/nrow(sba_train))[2]
set.seed(1) # for replicability and so that each randomForest call uses the same seed 
fit_RF_orig_data <- randomForest(Default~.,data=sba_train, ntree=100, mtry=sqrt.num.vars) 

# predict classes using cutoff = prevalence (proportion of positive class in training data)
pred_prob_RF_orig_data = predict(fit_RF_orig_data, newdata = sba_validation, type = 'prob')
pred_RF_orig_data_altCutoff = ifelse(pred_prob_RF_orig_data[ ,2] > minority_prop, 1, 0)

# predict classes using default cutoff of 0.5 using this fit
pred_RF_orig_data <-  predict(fit_RF_orig_data, newdata = sba_validation)

##RANFOM FOREST WITH RANDOM UNDERSAMPLING
# next, fit randomForest using the Random Undersampling technique

# Random undersampling
set.seed(1)
randomUndersamp_trainSet <- ovun.sample(Default~., data=sba_train, method="under", N = 2*sum(sba_train$Default==1)) 
# N is desired the size of the resulting dataset
# for undersampling we want each class to have the 
# number of observations of the minority class 
# (here y = 1 is the minority class)
table(randomUndersamp_trainSet$data$Default)

#Random forest
set.seed(1)
fit_RF_randomUndersamp <- randomForest(Default~.,data=randomUndersamp_trainSet$data, ntree=200, mtry=sqrt.num.vars) 
pred_RF_randomUndersamp <-  predict(fit_RF_randomUndersamp, newdata = sba_validation)

## RF WITH RANDOM OVERSAMPLING
# next, fit randomForest using Random Oversampling

# Random oversampling
set.seed(1)
randomOversamp_trainSet <- ovun.sample(Default~., data=sba_train, method="over", N = 2*sum(sba_train$Default==0))
# for oversampling we want each class to have the 
# number of observations of the majority class 
# (here y = 0 is the majority class)
table(randomOversamp_trainSet$data$Default)

# RF
set.seed(49012893)
fit_RF_randomOversamp <- randomForest(Default~.,data=randomOversamp_trainSet$data, ntree=100, mtry=sqrt.num.vars) 
pred_RF_randomOversamp <-  predict(fit_RF_randomOversamp, newdata = sba_validation)

# EXTRA - optional: try random combination sampling i.e. 
# peform both random oversample and random undersampling using the 
# ovun.sample() function with method = "both"
# e.g.: 
# randomCombsamp_trainSet = ovun.sample(yyes~., data=trainSet, method="both", p = 0.5)
# ...

## RF WITH SMOTE
# next, fit randomForest using SMOTE ("synthetic minority oversampling technique")

# SMOTE
smote_trainSet <- SMOTE(sba_train[-ncol(sba_train)], sba_train$Default)
table(smote_trainSet$data$class)

# RF
smote_trainSet$data$class <- as.factor(smote_trainSet$data$class)
set.seed(1)
fit_RF_SMOTE <- randomForest(class~.,data=smote_trainSet$data, ntree=100, mtry=sqrt.num.vars) 
pred_RF_SMOTE <-  predict(fit_RF_SMOTE, newdata = sba_validation)

# EXTRA - optional: try undersampling using a cluster-based method.
# You might try clustering the majority class data using the kmeans() 
# function, with the number of clusters equal to the minority class size.
# Alternatively, you might try another cluster-based method shown in the 
# slides, e.g. the pam() or agnes() functions, or you might look online for
# other clustering functions implemented in R or R packages.
# Below is an example of the cluster-centroid method outlined in the slides. 
# You might try adding it to the validation results matrix and the OOB plot 
# page, and/or comparing the results with another cluster-based method.
trainSet_majorityClassData = subset(trainSet, Default == 0)
trainSet_majorityClassData$Default = trainSet_majorityClassData$Default %>% as.character %>% as.numeric
set.seed(49012893)
clusteredMajorityClassData = kmeans(x = trainSet_majorityClassData, centers = sum(trainSet$Default == 1))
clustCentMajorityClassData = data.frame(clusteredMajorityClassData$centers)
# trainSet_minorityClassData = subset(trainSet, Default == 1)
# trainSet_minorityClassData$Default = trainSet_minorityClassData$Default %>% as.character %>% as.numeric
# clustCentUndersamp_trainSet = rbind(clustCentMajorityClassData, trainSet_minorityClassData)
clustCentUndersamp_trainSet$Default = as.factor(clustCentUndersamp_trainSet$Default)
# set.seed(49012893)
# fit_RF_clustCentUndersamp <- randomForest(Default~., data=clustCentUndersamp_trainSet, ntree=400, mtry=sqrt.num.vars) 
# pred_RF_clustCentUndersamp <-  predict(fit_RF_clustCentUndersamp, newdata = validationSet)

# EXTRA - optional: try a synthetic combination sampling technique e.g. using the SCUT() 
# function from the 'scutr' package, or look online for other implementations in R

## COMPARISON OF METHODS
# collate validationSet overall accuracy, sensitivity, specificity, PPV and NPV into a table

#create a matrix for the comparison #1 line per method
validationResultsMatrix = matrix(data = NA, nrow = 5, ncol = 6)
colnames(validationResultsMatrix) = c('Method', 'Accuracy', 'Sensitivity', 
                                      'Specificity', 'Recall', 'NPV') #PPV=P(y=1|y_hat=1) #NPV=P(y=0|y_hat=0)

#fill the matrix with results on accuracy, sensitivity, specificity, PPV and NPV #1 line per method
validationResultsMatrix[1, ] = c('Raw Imbalanced Data: Cutoff = 0.5',
                                 confusionMatrix(pred_RF_orig_data, sba_validation$Default, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_orig_data, sba_validation$Default, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))

validationResultsMatrix[2, ] = 
  c('Raw Imbalanced Data: Cutoff = Prevalence',
    confusionMatrix(as.factor(pred_RF_orig_data_altCutoff), sba_validation$Default, 
                    positive = "1")$overall[1] %>% round(., 3),
    confusionMatrix(as.factor(pred_RF_orig_data_altCutoff), sba_validation$Default, 
                    positive = "1")$byClass[1:4] %>% round(., 3))

validationResultsMatrix[3, ] = c('Random Undersampling Majority Class',
                                 confusionMatrix(pred_RF_randomUndersamp, sba_validation$Default, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_randomUndersamp, sba_validation$Default, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))

validationResultsMatrix[4, ] = c('Random Oversampling Minority Class',
                                 confusionMatrix(pred_RF_randomOversamp, sba_validation$Default, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_randomOversamp, sba_validation$Default, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))

validationResultsMatrix[5, ] = c('SMOTE Oversampling Minority Class',
                                 confusionMatrix(pred_RF_SMOTE, sba_validation$Default, 
                                                 positive = "1")$overall[1] %>% round(., 3),
                                 confusionMatrix(pred_RF_SMOTE, sba_validation$Default, 
                                                 positive = "1")$byClass[1:4] %>% round(., 3))

View(validationResultsMatrix)

# view OOB error rate plots for the randomForest() fits
par(mfrow = c(2,2))
plot(fit_RF_orig_data, ylim = c(0,1), main = 'Raw Imbalanced Data: Cutoff = 0.5')
plot(fit_RF_randomUndersamp, ylim = c(0,1), main = 'Random Undersampling Majority Class')
plot(fit_RF_randomOversamp, ylim = c(0,1), main = 'Random Oversampling Minority Class')
plot(fit_RF_SMOTE, ylim = c(0,1), main = 'SMOTE Oversampling Minority Class')
par(mfrow = c(1,1))


