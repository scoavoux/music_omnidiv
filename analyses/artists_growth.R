## author: Samuel Coavoux
## date: 09/06/2017

library(tidyverse)
library(parallel)
load("data/streams.RData")
load("data/french_users.RData")

## On cherche à mesurer le nombre d'artistes différents écoutés en une semaine.

###### Bootstrap ######
bootstrap_artists <- function(x, st){
  filter(st, user_id %in% sample(unique(user_id), x)) %>% 
    distinct(art_id) %>% 
    nrow()
}




cl <- makeCluster(10)
clusterEvalQ(cl, library(tidyverse))
clusterExport(cl, c("bootstrap_artists", "st"))
x <- data.frame(users = rep(c(seq(10, 40, 10), seq(50, 400, 50), seq(500, 2000, 100), seq(2200, 2700, 200)), each = 10))
weeks <- unique(st$week)
x$artists <- parSapply(cl, X = x$users, st = filter(st, week == sample(weeks, 1)), FUN = bootstrap_artists)
stopCluster(cl)
save(x, file = "data/artists_growth.RData")

###### Analysis ######

load("data/artists_growth.RData")

y <- group_by(x, users) %>% 
  summarise(artists = mean(artists)) %>% 
  rbind(data.frame(users = nrow(us), artists = length(unique(st$art_id))))

ggplot(y, aes(users, artists)) +
  geom_point()

On cherche à modéliser artists par users.
