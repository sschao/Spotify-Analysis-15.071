#read data from desktop
data_dir <- "~/Github/Spotify-Analysis-15.071/19000-spotify-songs"

data_songs <- read.csv(paste(data_dir, "/song_data.csv", sep=""))
data_info <- read.csv(paste(data_dir, "/song_info.csv", sep=""))


#group by song in the data_songs dataset
#average the songs characteristics
#delete old columns
#replace by new columns (eg avg_song_popularity)

library(dplyr)
library(ggplot2)
data_songs_1 <- data_songs %>%
  group_by(song_name) %>%
  mutate(avg_song_popularity = mean(song_popularity),
          avg_song_duration_ms = mean(song_duration_ms),
          avg_acousticness = mean(acousticness),
          avg_danceability = mean(danceability),
          avg_energy = mean(energy),
          avg_instrumentalness = mean(instrumentalness), 
          avg_key = mean(key),
          avg_liveness = mean(liveness),
          avg_loudness = mean(loudness),
          avg_audiomode = mean(audio_mode),
          avg_speechiness = mean(speechiness),
          avg_tempo = mean(tempo),
          avg_time_signature = mean(time_signature),
          avg_audio_valence = mean(audio_valence),
         song_popularity = NULL,
         song_duration_ms = NULL,
         acousticness = NULL,
         danceability = NULL,
         energy = NULL,
         instrumentalness = NULL,
         key = NULL,
         liveness = NULL,
         loudness = NULL,
         audio_mode = NULL,
         speechiness = NULL,
         tempo = NULL,
         time_signature = NULL,
         audio_valence = NULL)

#keep unique songs
data <- unique(data_songs_1)
#the two datasets (song_data and song_info) have 13070 unique songs
length(unique(data_info$song_name))
length(unique(data_songs$song_name))
#the songs are listed in the same order in both datasets
unique(data_info$song_name == data_songs$song_name)


#test
try1 <- subset(data_songs, song_name == "Sex on Fire")
try2 <- subset(data_info, song_name == "Sex on Fire")
try3 <- subset(data, song_name == "Sex on Fire")

#visuals
hist(data$avg_song_popularity, breaks  = 100) 
ggplot(data = data) +
  geom_bar(mapping = aes(x = avg_song_popularity), stat = "count", width = 0.8, fill = "gray", color = "blue") + 
  labs(x = "Average Popularity", y = "# of Songs", title = " # of Songs by Average Popularity") +
  theme_light()


write.csv(data, file = paste(data_dir, "/cleaned_song_data.csv", sep=""))



