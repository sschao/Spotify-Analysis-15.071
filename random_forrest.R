library(caret)
#use dataset calles data for training and testing models
#split dataset
set.seed(156)
split <- createDataPartition(data$avg_song_popularity, p = 0.8, list = FALSE)
data.train <- data[split,]
data.test <- data[-split,]
#fidn mean and sd of avg_song_popularity
#mu <- mean(data$avg_song_popularity)
#sd <- sd(data$avg_song_popularity)
#change avg song popularity to classes "low", "mid", "high"
#data$avg_song_popularity <- ifelse(data$avg_song_popularity > mu + sd, "high", 
             # ifelse(data$avg_song_popularity < mu - sd, "low", "mid"))

#str(data)
#data$avg_song_popularity <- as.factor(data$avg_song_popularity )
#data.train <- data[split,]
#data.test <- data[-split,]

#run rf model with cross validation
rf.cv <- train(y =  data.train$avg_song_popularity,
               x = subset(data.train, select=-c(avg_song_popularity, song_name, album_names, artist)),
               method="rf", ntree=100, nodesize = 20,
               trControl=trainControl(method="cv", number=5),
               tuneGrid=data.frame(mtry=seq(1,8,1)),
               importance = T)



library(rpart.plot)
#plot RMSE as a function of mtry
plot(rf.cv$results$mtry, rf.cv$results$RMSE, type = "l")
#assign best model
rf.cv.final <- rf.cv$finalModel
#make predictions
pred.train <- predict(rf.cv.final, newdata=data.train)
pred.test <- predict(rf.cv.final, newdata=data.test)
#pred.test <- predict(rf.mod.cv, newdata=data.test)
library(randomForest)
#Variable Importance
importance(rf.cv.final, scale = T)
rf.cv.final$importance

#make scaled barplot 
imp_inc_mse <- sort(importance(rf.cv.final, scale = T)[,1]/max(importance(rf.cv.final, scale = T)[,1])*100, decreasing =F)
imp_node_purity <- sort(rf.cv.final$importance[,2]*100/sum(rf.cv.final$importance[,2]), decreasing = F)
#barplot of incMSE
par(mai=c(1,2,1,1))
bar1<- barplot(imp_inc_mse, horiz = T, las = 1, col = "purple", main = "%IncMSE", xlim = c(0,110),
               border = F)
text(y=bar1, x=imp_inc_mse, pos=4,labels=round(imp_inc_mse, digits = 2))
#barplot of node purity
par(mai=c(1,2,1,1))
bar2 <- barplot(imp_node_purity, horiz = T, las = 1, col = "purple", main = "Inc Node Purity", xlim = c(0,80),
                border = F)
text(y=bar2, x=imp_node_purity, pos=4,labels=round(imp_node_purity, digits = 2))

# Calculate R2, OSR2, out of sample RMSE
mean_train <- mean(data.train$avg_song_popularity)
SSETest <- sum((pred.test - data.test$avg_song_popularity)^2)
SSTTest <- sum((data.test$avg_song_popularity- mean_train)^2)
OSR2 <- 1 - SSETest/SSTTest
OSR2
#
SSETrain <- sum((pred.train - data.train$avg_song_popularity)^2)
SSTTrain <- sum((data.train$avg_song_popularity- mean_train)^2)
R2 <- 1 - SSETrain/SSTTrain
R2
#
RMSE <- sqrt(mean((pred.test - data.test$avg_song_popularity)^2))
RMSE

#plot actual vs fitted values
plot(data.test$avg_song_popularity, col = "red", main = "Actual vs Fitted (RF)" )
lines(pred.test, type = "p", col = "blue")
legend(legend = c("Actual","Fitted"), fill = c("red","blue"), "topright", cex = 0.5)


