#read data from desktop
data_songs <- read.csv("~/Desktop/song_data.csv")
data_info <- read.csv("~/Desktop/song_info.csv")
data_searches <- read.csv("~/Desktop/number_of_searches.csv")
data_date <- read.csv("~/Desktop/release_date_revised.csv")

#make searches into numeric values, make artists into characters
data_searches$Number.of.Searches <- as.numeric(levels(data_searches$Number.of.Searches ))[data_searches$Number.of.Searches]
data_searches$Artist <- as.character(data_searches$Artist)

#data are in the same order 
unique(data_songs$song_name == data_info$song_name)

#attach album names, artist, date, searches
data_songs$album_names <- data_info$album_names
data_songs$artist <- data_info$artist_name
data_songs$searches <- data_searches$Number.of.Searches
data_songs$year <- data_date$Release.Year

####


#create dataset for modeling purposes

#group songs by song and artist
#average the songs characteristics
#delete old columns
#replace by new columns (eg avg_song_popularity)
#average the number of searches
#keep earliest year 
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
         audio_valence = NULL,
         mean_searches = mean(searches, na.rm = T),
         date = min(year),
         searches = NULL, 
         year =  NULL)%>%
         slice(1)



data <- data_songs_1

#test
try1 <- subset(data_songs, song_name == "Sex on Fire")
try2 <- subset(data_info, song_name == "Sex on Fire")
try3 <- subset(data, song_name == "Sex on Fire")

#visuals
hist(data$avg_song_popularity, breaks  = 100) 
library(ggplot2)
ggplot(data = data) +
  geom_bar(mapping = aes(x = avg_song_popularity), stat = "count", width = 0.8, fill = "gray", color = "blue") + 
  labs(x = "Average Popularity", y = "# of Songs", title = " # of Songs by Average Popularity") +
  theme_light()


#dataset for original model (contains no date or searches)
data1 <- data[ , !(names(data) %in% c("mean_searches", "date"))]

#dataset for augmented model (contains searches)
data2 <- data[ , !(names(data) %in% c("date"))]
#delete rows with NaN date
data2 <- na.omit(data2)

#dataset for augmented model (contains searches and year)
data3 <- na.omit(data)

#save 3 cleaned files
write.csv(data1, file = paste(data_dir, "/cleaned_song_data1.csv", sep=""))
write.csv(data2, file = paste(data_dir, "/cleaned_song_data2.csv", sep=""))
write.csv(data3, file = paste(data_dir, "/cleaned_song_data3.csv", sep=""))




