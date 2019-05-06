#try neural nets
library(caret)
#use dataset called data for training and testing models
#split dataset
set.seed(156)
split <- createDataPartition(data$avg_song_popularity, p = 0.8, list = FALSE)
data.train <- data[split,]
data.test <- data[-split,]
#get packages
install.packages("neuralnet") 
install.packages("forecast")
library(forecast)
#normalize
norm.object <- preProcess(data.train, method="range")
data.train.norm <- predict(norm.object, data.train)
data.test.norm <- predict(norm.object, data.test)
#test different types od NN models
try_hidden <- list(c(3,3),c(2,2))

#creat function that runs NN for each choice of try_hidden
NN <- function(h){
  neuralnet(avg_song_popularity ~ avg_song_duration_ms+ 
              avg_acousticness+               
              avg_danceability+   
              avg_energy+
              avg_instrumentalness+     
              avg_key+ 
              avg_liveness+
              avg_loudness+ 
              avg_audiomode+
              avg_speechiness+ 
              avg_tempo+            
              avg_time_signature+ 
              avg_audio_valence, 
            data = data.train.norm, linear.output = F, 
            hidden = h)
}
# Now we can use lapply to run the neuralnet function over all the different
# values we want to try for "hidden"
NNmodels <- lapply(try_hidden,NN)
#plot models
lapply(NNmodels,plot)
# Now we'll write another custom function to help implement our prediction multiple times using different NNmodels
NNpredict <- function(myNNmodel){
  neuralnet::compute(myNNmodel, data.test.norm[, c("avg_song_duration_ms", 
                                          "avg_acousticness",               
                                          "avg_danceability",   
                                          "avg_energy",
                                          "avg_instrumentalness",     
                                          "avg_key",
                                          "avg_liveness",
                                          "avg_loudness", 
                                          "avg_audiomode",
                                          "avg_speechiness", 
                                          "avg_tempo",            
                                          "avg_time_signature", 
                                          "avg_audio_valence")])
} 

NNpredictions <- lapply(NNmodels,NNpredict)

NNpredictions_vector <- lapply(NNpredictions,function(p){return(as.vector(p$net.result))})

# check the out-of-sample accuracy
results <- lapply(NNpredictions_vector,accuracy,data.test.norm$avg_song_popularity)
results
# add names to the list of results to make it easier to follow what we did
model_names <- lapply(try_hidden,as.character)
names(results) <- model_names
results

#change results back to regular range for first model

NNpredictions_vector_original <- min(data.test$avg_song_popularity) + NNpredictions_vector[[1]]*max(data.test$avg_song_popularity) 

# Calculate R2, OSR2, out of sample RMSE
mean_train <- mean(data.train$avg_song_popularity)
SSETest <- sum((NNpredictions_vector_original - data.test$avg_song_popularity)^2)
SSTTest <- sum((data.test$avg_song_popularity- mean_train)^2)
OSR2 <- 1 - SSETest/SSTTest
OSR2

#
RMSE <- sqrt(mean((NNpredictions_vector_original - data.test$avg_song_popularity)^2))
RMSE


#plot variable importnace
olden(NNmodels[[2]])
