#read data from desktop
data_songs <- read.csv("~/Desktop/song_data.csv")
data_info <- read.csv("~/Desktop/song_info.csv")
data_searches <- read.csv("~/Desktop/number_of_searches.csv")



#data are in the same order 
unique(data_songs$song_name == data_info$song_name)

#attach album names and artist
data_songs$album_names <- data_info$album_names
data_songs$artist <- data_info$artist_name
#group by song and albumin the data_songs dataset
#average the songs characteristics
#delete old columns
#replace by new columns (eg avg_song_popularity)
#keep only the first song when songs have the same name (even form different albums)

library(dplyr)
data_songs_1 <- data_songs %>%
  group_by(song_name, artist) %>%
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
         audio_valence = NULL)%>%
         slice(1)

#keep unique songs
data1 <- unique(data_songs_1) 
#the two datasets (song_data and song_info) have 13070 unique songs
#length(unique(data_info$song_name))
#length(unique(data_songs$song_name))
#the songs are listed in the same order in both datasets
#unique(data_info$song_name == data_songs$song_name)


#test
try1 <- subset(data_songs, song_name == "Sex on Fire")
try2 <- subset(data_info, song_name == "Sex on Fire")
try3 <- subset(data1, song_name == "Sex on Fire")

#visuals
hist(data1$avg_song_popularity, breaks  = 100) 
library(ggplot2)
ggplot(data = data1) +
  geom_bar(mapping = aes(x = avg_song_popularity), stat = "count", width = 0.8, fill = "gray", color = "blue") + 
  labs(x = "Average Popularity", y = "# of Songs", title = " # of Songs by Average Popularity") +
  theme_light()

#attach searches from data_searches csv for each artist
#chnage names of columns
colnames(data_searches) <- c("artist", "num_of_searches")
#get rid of checks
data_searches <- subset(data_searches, num_of_searches != "check"  )
#make searches into numeric values form factors, make artists into characters
data_searches$num_of_searches <- as.numeric(levels(data_searches$num_of_searches ))[data_searches$num_of_searches]
data_searches$artist <- as.character(data_searches$artist)
#end up with average number of searches per artist 
data_searches1 <- data_searches %>%
  group_by(artist) %>%
  mutate(average_num_of_searches = mean(num_of_searches),
         num_of_searches = NULL)%>%
  slice(1)
#delete NA entries
data_searches1 <- na.omit(data_searches1)
#join two datasets 

data <- merge(data1, data_searches1, by = "artist" )
















write.csv(data, file = paste(data_dir, "/cleaned_song_data.csv", sep=""))



