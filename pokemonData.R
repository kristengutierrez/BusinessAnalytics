#Pokemon Battles
library(tidyverse)
library(caret)
library(lubridate)
set.seed(6766)

#Data Gathering
pokemon <- read_csv("~/Desktop/pokemon.csv")
pokemon <- as.tibble(pokemon)
head(pokemon)
colnames(pokemon)[3] <- "type1"
colnames(pokemon)[4] <- "type2"
colnames(pokemon)[8] <- "spatk"
colnames(pokemon)[9] <- "spdef"
head(pokemon)

ggplot(data = pokemon, mapping = aes(x = type1, y = type2)) + geom_jitter()
ggplot(data = pokemon, mapping = aes(x = type1, y = HP)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = Attack)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = Defense)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = spatk)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = spdef)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = Speed)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = Generation)) + geom_boxplot()
ggplot(data = pokemon, mapping = aes(x = type1, y = Legendary)) + geom_jitter()
pokemon <- mutate(pokemon, type2 = as.factor(ifelse(is.na(type2), "none", type2)))

#Splitting data
trainSize = floor(0.8 * nrow(pokemon))
trainIndex = sample(seq_len(nrow(pokemon)), size = trainSize)
train <- pokemon[trainIndex, ]
test <- pokemon[-trainIndex, ]

scaler <- preProcess(train, method = c("center", "scale", "knnImpute"))
train <- predict(scaler, train)
test <- predict(scaler, test)

#Type 2
knnModel <- train(type1 ~ type2, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#HP
knnModel <- train(type1 ~ HP, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Attack
knnModel <- train(type1 ~ Attack, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Defense
knnModel <- train(type1 ~ Defense, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Special Attack
knnModel <- train(type1 ~ spatk, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Special Defense
knnModel <- train(type1 ~ spdef, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Speed
knnModel <- train(type1 ~ Speed, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Generation
knnModel <- train(type1 ~ Generation, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Legendary
knnModel <- train(type1 ~ Legendary, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#ROUND 2
#Type 2 + Defense
knnModel <- train(type1 ~ type2 + Defense, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Type 2 + Special Attack
knnModel <- train(type1 ~ type2 + spatk, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Type 2 + HP
knnModel <- train(type1 ~ type2 + HP, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Type 2 + Speed
knnModel <- train(type1 ~ type2 + Speed, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#ROUND 3
#Type 2 + Special Attack + Defense
knnModel <- train(type1 ~ type2 + spatk + Defense, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)

#Type 2 + Special Attack + HP
knnModel <- train(type1 ~ type2 + spatk + HP, data = train, method ="knn")
predictions <- predict(knnModel, test)
confusionMatrix(predictions, test$type1)
