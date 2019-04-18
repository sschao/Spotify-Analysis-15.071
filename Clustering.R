songs_data = read.csv("19000-spotify-songs/cleaned_song_data.csv")

to_clust = subset(songs_data, select = -c(X, song_name, avg_song_popularity))

d <- dist(to_clust)
hclust.mod <- hclust(d, method="ward.D2")
plot(hclust.mod, labels=F, ylab="Dissimilarity")
hc.dissim <- data.frame(k = seq_along(hclust.mod$height),
                        dissimilarity = rev(hclust.mod$height))
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l")
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l", xlim=c(0,25))

h.clusters <- cutree(hclust.mod, 11)
table(songs_data$avg_song_popularity, h.clusters)
table(h.clusters)

set.seed(15071)
km <- kmeans(to_clust, iter.max=100, 13)
kmc <- km$cluster
table(songs_data$avg_song_popularity, kmc)
table(kmc)
table(h.clusters, kmc)
