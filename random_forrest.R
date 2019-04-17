library(caret)
#split dataset
set.seed(121)
split <- createDataPartition(data$avg_song_popularity, p = 0.7, list = FALSE)
data.train <- data[split,]
data.test <- data[-split,]
#run rf model with cross validation
rf.cv <- train(y =  data$avg_song_popularity,
               x = subset(data, select=-c(avg_song_popularity, song_name)),
               method="rf", ntree=100, nodesize = 20,
               trControl=trainControl(method="cv", number=5),
               tuneGrid=data.frame(mtry=seq(1,6,1)),
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
imp_inc_mse <- importance(rf.cv.final, scale = T)[,1]
imp_node_purity <- rf.cv.final$importance[,2]*100/sum(rf.cv.final$importance[,2])
#barplot of incMSE
par(mai=c(1,2,1,1))
bar1<- barplot(imp_inc_mse, horiz = T, las = 1, col = "red", main = "%IncMSE", xlim = c(0,100))
text(y=bar1, x=imp_inc_mse, pos=4,labels=round(imp_inc_mse, digits = 2))
#barplot of node purity
par(mai=c(1,2,1,1))
bar2 <- barplot(imp_node_purity, horiz = T, las = 1, col = "red", main = "Inc Node Purity", xlim = c(0,100))
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


