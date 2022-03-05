library(tidyverse)
library(caret)
library(mlbench)
library(janitor)

#load data
data_or <- read_csv("orange_juice_withmissing.csv")

#check missing value
sum(is.na(data_or))

#delete missing value
data_or <- na.omit(data_or)

#clean column
data_or <- data_or %>%
  clean_names()

names(data_or)

glimpse(data_or)

#edit columns
data_or <- data_or %>%
  mutate_if(is.character,
            as.factor)

# change column purchase to factor 
data_or <- data_or %>%
  mutate(purchase = factor(purchase, 
                           levels = c("CH","MM"), 
                           labels = c(0, 1)))

data_or <- data_or %>%
  mutate(store7 = factor(store7, 
                         levels = c("No","Yes"), 
                         labels = c(0, 1)))

#split data
set.seed(42)
train_id<- createDataPartition(data_or$purchase, 
                               p = 0.8, list = FALSE)
train_data <- data_or[train_id, ]
test_data <- data_or[-train_id, ]

# train model
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

knn_model <- train(purchase ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Accuracy",
                   trControl = ctrl)

# score model predict
knn_pred <- predict(knn_model,
                    newdata = test_data)

# evaluate model
knn_accuracy <- mean(knn_pred == test_data$purchase)

#decision tree
tree_model <- train(purchase ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl)

#score predition
tree_pred <- predict(tree_model,
                     newdata = test_data)

# evaluate model
tree_accuracy <- mean(tree_pred == test_data$purchase)

#random forest
randomForest_model <- train(purchase ~ .,
                            data = train_data,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = ctrl)

# score model
rf_pred <- predict(randomForest_model,
                   newdata = test_data)

#evaluate model
rf_accuracy <- mean(rf_pred == test_data$purchase)

# model comparison
modellist <- list(
  knn = knn_model,
  dicisiontree = tree_model,
  randomforest = randomForest_model)

result <- resamples(modellist)
summary(result)

# confusion matrix 
cm <- table(tree_pred, 
            test_data$purchase,
            dnn = c("Prediction", "Actual"))

# Accuracy 
accuracy <- sum(diag(cm)) / sum(cm)

# Precision
precision <- cm[1, 1] / sum(cm[1, ])

# Recall
recall <- 102 / (102 + 25)

#F1 score
f1 <- 2 * precision * recall / (precision + recall)

#confusion matrix
c0 <- confusionMatrix(tree_pred,
                      test_data$purchase,
                      positive = "0")

c1 <- confusionMatrix(tree_pred,
                      test_data$purchase,
                      positive = "1")


#Repeated K-fold cross-validation
#knn
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

knn_model <- train(purchase ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Accuracy",
                   trControl = ctrl)

#tree
tree_model <- train(purchase ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl)

#randomForest
randomForest_model <- train(purchase ~ .,
                            data = train_data,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = ctrl)

# score model
knn_pred <- predict(knn_model,
                    newdata = test_data)

tree_pred <- predict(tree_model,
                     newdata = test_data)

rf_pred <- predict(randomForest_model,
                   newdata = test_data)

#evaluate model
knn_accuracy <- mean(knn_pred == test_data$purchase)
tree_accuracy <- mean(tree_pred == test_data$purchase)
rf_accuracy <- mean(rf_pred == test_data$purchase)

# model comparison
modellist <- list(
  knn = knn_model,
  dicisiontree = tree_model,
  randomforest = randomForest_model
)

result <- resamples(modellist)
summary(result)

#confusion matrix
c0 <- confusionMatrix(tree_pred,
                      test_data$purchase,
                      positive = "0")

c1 <- confusionMatrix(tree_pred,
                      test_data$purchase,
                      positive = "1")


#Classification-AUC Precision Recall F1
# split data
set.seed(42)
train_id <- createDataPartition(
  data_or$purchase , 
  p = 0.8,
  list = FALSE)

train_data <- data_or[train_id, ]
test_data <- data_or[-train_id, ]

# train model 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE,
  summaryFunction = prSummary,
  classProbs = TRUE) 

# AUC
knn_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "knn",
  metric = "AUC",
  trControl = ctrl )

# Precision
knn_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "knn",
  metric = "Precision",
  trControl = ctrl )

# Recall
knn_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "knn",
  metric = "Recall",
  trControl = ctrl )

# F1
knn_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "knn",
  metric = "F",
  trControl = ctrl )

# socre model
knn_pred <- predict(knn_model, newdata = test_data)

# evaluate model
mean(knn_pred == test_data$purchase)


# model comparison
# train model 
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE,
  summaryFunction = prSummary,
  classProbs = TRUE) 

#knn
knn_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "knn",
  metric = "AUC",
  trControl = ctrl )

#decision tree
rpart_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "rpart",
  metric = "AUC",
  trControl = ctrl )

#random forest 
rf_model <- train(
  purchase ~ ., 
  data = train_data,
  method = "rf",
  metric = "AUC",
  trControl = ctrl )

# model comparison
modellist <- list(
  knn = knn_model,
  dicisiontree = rpart_model,
  randomforest = rf_model)

result <- resamples(modellist)
summary(result)

# socre model
knn_pred <- predict(knn_model,
                    newdata = test_data)

rpart_pred <- predict(rpart_model,
                      newdata = test_data)

rf_pred <- predict(rf_model,
                   newdata = test_data)

# evaluate model
knn_test <- mean(knn_pred == test_data$purchase)
rpart_test <- mean(rpart_pred == test_data$purchase)
rf_test <- mean(rf_pred == test_data$purchase)

cat("knn: ", knn_test, 
    "\nrpart: ", rpart_test,
    "\nrf: ", rf_test)

# confusion matrix
confusionMatrix(rf_pred, 
                test_data$purchase, 
                positive = "CH",
                mode = "prec_recall")


# twoClassSummary
#Classification-ROC Sens Specs
#split data
set.seed(42)
train_id <- createDataPartition(data_or$purchase, 
                                p = 0.8, list = FALSE)
train_data <- data_or[train_id, ]
test_data <- data_or[-train_id, ]

# train model
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

# knn 
knn_model <- train(purchase ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrl)

# decision tree
rpart_model <- train(purchase ~ .,
                     data = train_data,
                     method = "rpart",
                     metric = "ROC",
                     trControl = ctrl)

# #random forest
rf_model <- train(purchase ~ .,
                  data = train_data,
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl)

# score model
knn_pred <- predict(knn_model,
                    newdata = test_data)

rpart_pred <- predict(rpart_model,
                      newdata = test_data)

rf_pred <- predict(rf_model,
                   newdata = test_data)

# evaluate model
knn_test <- mean(knn_pred == test_data$purchase)
rpart_test <- mean(rpart_pred == test_data$purchase)
rf_test <- mean(rf_pred == test_data$purchase)

cat("knn: ", knn_test, 
    "\nrpart: ", rpart_test,
    "\nrf: ", rf_test)

# model comparison
modellist <- list(
  knn = knn_model,
  dicisiontree = rpart_model,
  randomforest = rf_model
)

result <- resamples(modellist)
summary(result)

#confusion matrix
confusionMatrix(rf_pred,
                test_data$purchase,
                positive = "CH")


#variable importance
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

tree_model <- train(purchase ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl)

#score model
tree_pred <- predict(tree_model,
                     newdata = test_data)

# evaluate model
mean(tree_pred == test_data$purchase)

#variable importance
varImp(tree_model)


#finalmodel decision tree  
library(rpart.plot)
rpart.plot(tree_model$finalModel)


# random forest variable importance
randomforest_model <- train(purchase ~ .,
                            data = train_data,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = ctrl)

varImp(randomforest_model)

# score model
randomforest_pred <- predict(randomforest_model,
                             newdata = test_data)

# evaluate model
mean(randomforest_pred == test_data$purchase)


# ranger
library(ranger)

set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

ranger_model <- train(purchase ~ .,
                      data = train_data,
                      method = "ranger",
                      importance = "impurity",
                      metric = "Accuracy",
                      trControl = ctrl)

#score model
ranger_pred <- predict(ranger_model,
                       newdata = test_data)

# evaluate model
mean(ranger_pred == test_data$purchase)

varImp(ranger_model)

confusionMatrix(ranger_pred,
                test_data$purchase,
                positive = "CH",
                mode = "prec_recall")


###tuneLength = 5
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

ranger_model <- train(purchase ~ .,
                      data = train_data,
                      method = "ranger",
                      importance = "impurity",
                      metric = "Accuracy",
                      trControl = ctrl,
                      tuneLength = 10)

#score model
ranger_pred <- predict(ranger_model,
                       newdata = test_data)

# evaluate model
mean(ranger_pred == test_data$purchase)

confusionMatrix(ranger_pred,
                test_data$purchase,
                positive = "CH",
                mode = "prec_recall")


###tuneLength = 10
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

ranger_model <- train(purchase ~ .,
                      data = train_data,
                      method = "ranger",
                      importance = "impurity",
                      metric = "Accuracy",
                      trControl = ctrl,
                      tuneLength = 10)

#score model
ranger_pred <- predict(ranger_model,
                       newdata = test_data)

# evaluate model
mean(ranger_pred == test_data$purchase)

confusionMatrix(ranger_pred,
                test_data$purchase,
                positive = "CH",
                mode = "prec_recall")

#grid search
set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,
                     verboseIter = TRUE)

myGrid <- expand.grid(
  mtry = c(2, 5, 9, 11, 13, 17),
  splitrule = c("gini","extratrees"),
  min.node.size = 5)

ranger_model <- train(purchase ~ ., 
                      data = train_data, 
                      method = "ranger",
                      importance = "impurity", 
                      metric = "Accuracy", 
                      trControl = ctrl,
                      tuneGrid = myGrid)

#score model
ranger_pred <- predict(ranger_model,
                       newdata = test_data)

# evaluate model
mean(ranger_pred == test_data$purchase)

confusionMatrix(ranger_pred,
                test_data$purchase,
                positive = "CH",
                mode = "prec_recall")