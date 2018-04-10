library(mlbench)
library(tidyverse)
library(caret)
library(glmnet)
library(glmnetUtils)
data("Ionosphere")
Ionosphere <- as.tibble(Ionosphere)
head(Ionosphere)
set.seed(57676)
trainIndex <- createDataPartition(Ionosphere$Class, p = 0.8, list = FALSE, times = 1)
ionoTrain <- Ionosphere[trainIndex,]
ionoTest <- Ionosphere[-trainIndex,]
scaler <- preProcess(ionoTrain, method = c("center", "scale"))
ionoTrain <- predict(scaler, ionoTrain)
ionoTest <- predict(scaler, ionoTest)

LR <- glmnet(Class ~ . - V2, data = ionoTrain, family = "binomial", na.action = na.omit)
predictions <- predict(LR, ionoTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, ionoTest$Class)

LR <- glmnet(Class ~ . - V2, data = ionoTrain, family = "binomial", na.action = na.omit)
predictions <- predict(LR, ionoTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, ionoTest$Class)

