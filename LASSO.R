songs_data = read.csv("19000-spotify-songs/cleaned_song_data.csv")
library(glmnet)
library(caret)
#split dataset
set.seed(15.071)

split <- createDataPartition(songs_data$avg_song_popularity, p = 0.7, list = FALSE)
data.train <- songs_data[split,]
data.test <- songs_data[-split,]


x.train = as.matrix(subset(data.train, select = -c(X, song_name, avg_song_popularity)))
y.train = as.matrix(subset(data.train, select = c(avg_song_popularity)))

cv.lasso <- cv.glmnet(x.train, y.train, alpha=1, standardize=TRUE)

lambda_min <- cv.lasso$lambda.min
lambda_best <- cv.lasso$lambda.1se

pred.train <- predict(cv.lasso, newx=x.train)

x.test = as.matrix(subset(data.test, select = -c(X, song_name, avg_song_popularity)))
y.text = as.matrix(subset(data.test, select = c(avg_song_popularity)))

y.pred <- predict(cv.lasso, newx=x.test)
coef(cv.lasso, s=lambda_best)
coef(cv.lasso, s=lambda_min)



mean_train <- mean(data.train$avg_song_popularity)
SSETest <- sum((y.pred - data.test$avg_song_popularity)^2)
SSTTest <- sum((data.test$avg_song_popularity- mean_train)^2)
OSR2 <- 1 - SSETest/SSTTest
OSR2
#
SSETrain <- sum((pred.train - data.train$avg_song_popularity)^2)
SSTTrain <- sum((data.train$avg_song_popularity- mean_train)^2)
R2 <- 1 - SSETrain/SSTTrain
R2
#
RMSE <- sqrt(mean((y.pred - data.test$avg_song_popularity)^2))
RMSE
