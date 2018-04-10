library(tidyverse)
library(caret)
library(lubridate)
set.seed(6766)

setwd("C:/Users/Me/Desktop/HW4")

ratings <- read_tsv("~/Desktop/user_ratedmovies.dat")
movies <- read_tsv("~/Desktop/movies.dat")

head(ratings)
head(movies)

#rename movieId columns to be the same
movies <- mutate(movies, movieID = movies$id)
movies <- select(movies, movieID, title, imdbID, year)

#join the tibbles
rated_films <- inner_join(movies, ratings, key="movieID")
head(rated_films)

#number of ratings and avg rating
avgRating <- group_by(rated_films, movieID, title) %>% summarize(count = n(), avg_rating = mean(rating))
head(avgRating)

#distributions of avg rating in histogram
ggplot(data = avgRating, mapping=aes(avg_rating)) + geom_histogram(binwidth=0.5)

# number of MovieLens ratings per movie (log scale) as densities
ggplot(data = avgRating, mapping=aes(log1p(count))) + geom_density()

#Number of Ratings (Log-Scale) per Movie vs Average Rating per Movie
ggplot(data = avgRating, mapping=aes(x = log1p(count), y = avg_rating)) + geom_point()

#Splitting data
trainSize = floor(0.8 * nrow(rated_films))
trainIndex = sample(seq_len(nrow(rated_films)), size = trainSize)
train <- rated_films[trainIndex, ]
test <- rated_films[-trainIndex, ]

#Baseline (by J-Hope)
overall_avg_rating = mean(train$rating)
test_predictions <- mutate(test, overall_avg_rating = overall_avg_rating)
postResample(test_predictions$rating, test_predictions$overall_avg_rating)

#Model 1
predicted_ratings <- group_by(train, movieID, title) %>% summarise(avgRating = mean(rating))
test_predictions <- inner_join(test, predicted_ratings, key = "movieID")
postResample(test_predictions$rating, test_predictions$avgRating)

#Model 2
predicted_ratings <- group_by(train, movieID, title) %>% summarise(avgRating = mean(rating), count = n())
predicted_ratings <- filter(predicted_ratings, count >= 10)
test_predictions <- inner_join(test, predicted_ratings, key = "movieID")
postResample(test_predictions$rating, test_predictions$avgRating)

#Model 3
predicted_ratings <- group_by(train, movieID, title) %>% summarise(avgRating = mean(rating), count = n())
predicted_ratings <- filter(predicted_ratings, count >= 20)
test_predictions <- inner_join(test, predicted_ratings, key = "movieID")
postResample(test_predictions$rating, test_predictions$avgRating)

#Model 4
predicted_ratings <- group_by(train, movieID, title) %>% summarise(avgRating = mean(rating), count = n())
predicted_ratings <- filter(predicted_ratings, count >= 30)
test_predictions <- inner_join(test, predicted_ratings, key = "movieID")
postResample(test_predictions$rating, test_predictions$avgRating)

#Baseline RMSE = 1.0008877
#Model 1 RMSE = 0.8821832
#Model 2 RMSE = 0.8775288
#Model 3 RMSE = 0.8749861
#Model 4 RMSE = 0.8737799
