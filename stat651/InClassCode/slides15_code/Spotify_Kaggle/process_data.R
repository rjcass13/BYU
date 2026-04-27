library("tidyverse")

dat0 <- read.csv("Spotify_Kaggle.csv", head = TRUE, )
head(dat0)

table(dat0$track_genre)
dat <- dat0 %>% filter(track_genre %in% c("alternative", "country", "dance", "death-metal", "emo", "hard-rock",
                                          "hip-hop", "indie-pop", "pop", "punk_rock", "r-n-b", "rock", "show_tunes",
                                          "soul", "techno"))

Xcat <- dat[,c("popularity", "time_signature", "track_genre", "explicit")]
Xcont <- dat[,c("duration_ms", "danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")]
Xcont
any(is.na(Xcont))
any(is.na(Xcat))
any(is.na(dat$popularity))

str(Xcat)
table(Xcat$time_signature)
Xcat$time_signature <- factor(Xcat$time_signature, levels = c(4,3,5,1))
Xcatmat <- model.matrix(popularity ~ time_signature + explicit, data = Xcat)
head(Xcatmat)
colnames(Xcatmat)[1] <- "intercept"
colSums(Xcatmat)

dat_out <- as.data.frame(cbind(popularity = dat$popularity, genre = dat$track_genre, Xcatmat, Xcont))
str(dat_out)
write.csv(dat_out, file = "Spotify_Kaggle_subset.csv", row.names = FALSE)
