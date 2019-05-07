set.seed(150711)
library(caret)
setwd('C:\\Users\\shawn\\Desktop\\MIT college work\\15.0711\\Final Project')

spotify = read.csv("song_data.csv", stringsAsFactors = TRUE)
split = createDataPartition(spotify$song_name, p = 0.8, list = FALSE)

train = spotify[split,]
test = spotify[-split,]

mod1 = lm(song_popularity~. -song_name, data=train)
summary(mod1)

pred = predict(mod1, newdata = test)
SSE = sum((pred - test$song_popularity)^2)
train.mean = mean(train$song_popularity)
SST = sum((train.mean - test$song_popularity)^2)
1 - SSE/SST