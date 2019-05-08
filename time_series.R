#this file runs regression model on all musical feature
#creates a function which given a vector of current featurs predicts features of next year
#most featurs cannot be predicted with high R2 using linear model



#drop song, artist, popularity, album columns
data_4 <- data3[!(colnames(data3) %in% c("song_name", "album_names", "artist", "avg_song_popularity"))]

#aggregate numerical data by year (data are sorted by year)
agg_data <- aggregate(data_4, by = list(date = data_4$date), mean)
agg_data$date <- NULL


#keep data from 1940 and later because they are consecutive
agg_data <- subset(agg_data, date >= 1947)
#plot time series for some features
dates <- agg_data$date
plot(agg_data$avg_song_duration_ms, xaxt = "n", type = "b")
axis(1, at = 1:length(dates), labels = dates, cex.axis = 0.7, las = 2)

#regression model 
# D[t+1] = a +bD[t]+epsilon

#create matrix with lagged values

agg_data$avg_song_duration_ms_lag <- c(NA, head(agg_data$avg_song_duration_ms, -1))
agg_data$avg_acousticness_lag <- c(NA, head(agg_data$avg_acousticness, -1))
agg_data$avg_danceability_lag<- c(NA, head(agg_data$avg_danceability, -1))
agg_data$avg_energy_lag <- c(NA, head(agg_data$avg_energy, -1))
agg_data$avg_instrumentalness_lag <- c(NA, head(agg_data$avg_instrumentalness, -1))
agg_data$avg_key_lag <- c(NA, head(agg_data$avg_key, -1))
agg_data$avg_liveness_lag <- c(NA, head(agg_data$avg_liveness, -1))
agg_data$avg_loudness_lag <- c(NA, head(agg_data$avg_loudness, -1))
agg_data$avg_audiomode_lag <- c(NA, head(agg_data$avg_audiomode, -1))
agg_data$avg_speechiness_lag <- c(NA, head(agg_data$avg_speechiness, -1))
agg_data$avg_tempo_lag<- c(NA, head(agg_data$avg_tempo, -1))
agg_data$avg_time_signature_lag <- c(NA, head(agg_data$avg_time_signature, -1))
agg_data$avg_audio_valence_lag <- c(NA, head(agg_data$avg_audio_valence, -1))
agg_data$mean_searches_lag<- c(NA, head(agg_data$mean_searches, -1))



names <- colnames(agg_data)

#run all regressions
model <- list()
formula <- vector()

for (i in seq(1, length(names)-15)){
  
  formula <- paste0("agg_data$", names[i], " ~ ", "agg_data$",names[i+15])
  
  model[[i]] <- lm(formula)
  print(summary(model[[i]])$r.squared)
  
}


#function to predict next years features given a vector of this years features
next_year <- vector()
predict <- function(previous_year){
  
  for (i in seq(1:length(previous_year))){
    
    next_year[i] <- model[[i]]$coefficients[1] + previous_year[i]*model[[i]]$coefficients[2] 
    
  }
  print(as.numeric(next_year))
}
#example
year_2019 <- agg_data[73, 1:14]
#prediction after a year 
predict(year_2019)
#predictions after 2 years
predict((predict(year_2019)))
