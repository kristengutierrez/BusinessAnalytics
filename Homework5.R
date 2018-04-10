#Homework 5
install.packages("glmnet")
install.packages("glmnetUtils")
library(tidyverse)
library(caret)
library(mlbench)
library(glmnet)
library(glmnetUtils)
data(scat)
scat <- as.tibble(scat)
head(scat)
scat <- select(scat, Species, Site, Age, Number, Diameter, TI, Mass, CN, segmented, d13C, d15N)
head(scat)
set.seed(57676)
trainIndex <- createDataPartition(scat$Species, p = 0.8, list = FALSE, times = 1)
scatTrain <- scat[trainIndex,]
scatTest <- scat[-trainIndex,]
scaler <- preProcess(scatTrain, method = c("center", "scale"))
scatTrain <- predict(scaler, scatTrain)
scatTest <- predict(scaler, scatTest)

#Segmented + Site
modelScat <- glmnet(Species ~ segmented + Site, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + Age
modelScat <- glmnet(Species ~ segmented + Age, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + Number
modelScat <- glmnet(Species ~ segmented + Number, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + Diameter
modelScat <- glmnet(Species ~ segmented + Diameter, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + TI
modelScat <- glmnet(Species ~ segmented + TI, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + Mass
modelScat <- glmnet(Species ~ segmented + Mass, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + CN
modelScat <- glmnet(Species ~ segmented + CN, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d13C
modelScat <- glmnet(Species ~ segmented + d13C, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N
modelScat <- glmnet(Species ~ segmented + d15N, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + Site + d15N
modelScat <- glmnet(Species ~ segmented + Site + d15N, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + Age
modelScat <- glmnet(Species ~ segmented + d15N + Age, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + Number
modelScat <- glmnet(Species ~ segmented + d15N + Number, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + Diameter
modelScat <- glmnet(Species ~ segmented + d15N + Diameter, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + TI
modelScat <- glmnet(Species ~ segmented + d15N + TI, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + Mass
modelScat <- glmnet(Species ~ segmented + d15N + Mass, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + CN
modelScat <- glmnet(Species ~ segmented + d15N + CN, data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + d13C
modelScat <- glmnet(Species ~ ., data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

#Segmented + d15N + d13C + Age
modelScat <- glmnet(Species ~ ., data = scatTrain, family = "multinomial", na.action = na.omit)
predictions <- predict(modelScat, scatTest, type = "class", na.action = na.pass, s=0.01)
head(predictions)
confusionMatrix(predictions, scatTest$Species)

