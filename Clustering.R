songs_data = read.csv("19000-spotify-songs/cleaned_song_data.csv")

to_clust = subset(songs_data, select = -c(X, song_name, avg_song_popularity))

d <- dist(to_clust)
hclust.mod <- hclust(d, method="ward.D2")
plot(hclust.mod, labels=F, ylab="Dissimilarity")


hc.dissim <- data.frame(k = seq_along(hclust.mod$height),
                        dissimilarity = rev(hclust.mod$height))

plot(hc.dissim$k, hc.dissim$dissimilarity, type="l")
plot(hc.dissim$k, hc.dissim$dissimilarity, type="l", xlim=c(0,25))

h.clusters <- cutree(hclust.mod, 5)

rect.hclust(hclust.mod , k = 5, border = 2:6)
abline(h = 5, col = 'red')


table(cut(songs_data$avg_song_popularity, 10), h.clusters)
table(h.clusters)

set.seed(15071)
km <- kmeans(to_clust, iter.max=100, 5)
kmc <- km$cluster
table(cut(songs_data$avg_song_popularity, 10), kmc)
table(kmc)
table(h.clusters, kmc)

aggregate(subset(songs_data, select = -c(X, song_name)), by = list(h.clusters), mean)

aggregate(subset(songs_data, select = -c(X, song_name)), by = list(kmc), mean)
