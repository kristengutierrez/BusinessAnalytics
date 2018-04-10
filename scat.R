library(tidyverse)
library(caret)
data("scat")
scat <- as.tibble(scat)
head(scat)
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Month)) + geom_jitter()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Site)) + geom_jitter()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Length)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Age)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Number)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Location)) + geom_jitter()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Diameter)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = TI)) + geom_jitter()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = Mass)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = d13C)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = d15N)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = CN)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = ropey)) + geom_count()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = segmented)) + geom_count()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = flat)) + geom_boxplot()
ggplot(data = scat, mapping = aes(group = Species, x = Species, y = scrape)) + geom_count()

scat <- select(scat, Species, Site, Age, Number, Diameter, TI, Mass, CN, segmented, d13C, d15N)
head(scat)
set.seed(57676)
trainIndex <- createDataPartition(scat$Species, p = 0.8, list = FALSE, times = 1)
scatTrain <- scat[trainIndex,]
scatTest <- scat[-trainIndex,]
scaler <- preProcess(scatTrain, method = c("center", "scale"))
scatTrain <- predict(scaler, scatTrain)
install.packages("RANN")
scatTest <- predict(scaler, scatTest)


knnModel <- train(Species ~ Site, data = scatTrain, method ="knn")
Sitepredictions <- predict(knnModel, scatTest)
confusionMatrix(Sitepredictions, scatTest$Species)

knnModel <- train(Species ~ Age, data = scatTrain, method ="knn")
Sitepredictions <- predict(knnModel, scatTest)
confusionMatrix(Sitepredictions, scatTest$Species)

knnModel <- train(Species ~ Number, data = scatTrain, method ="knn")
Sitepredictions <- predict(knnModel, scatTest)
confusionMatrix(Sitepredictions, scatTest$Species)

knnModel <- train(Species ~ Diameter, data = scatTrain, method ="knn")
Sitepredictions <- predict(knnModel, scatTest)
confusionMatrix(Sitepredictions, scatTest$Species)


knnModel <- train(Species ~ d13C + segmented + CN + d15N, data = scatTrain, method ="knn")
Sitepredictions <- predict(knnModel, scatTest)
confusionMatrix(Sitepredictions, scatTest$Species)
