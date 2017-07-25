## author: Samuel Coavoux
## date: 09/06/2017

library(tidyverse)
library(parallel)
load("data/streams.RData")
load("data/french_users.RData")

###### Bootstrap ######
bootstrap_artists <- function(x){
  filter(st, user_id %in% sample(unique(user_id), x)) %>% 
    distinct(art_id) %>% 
    nrow()
}


# cl <- makeCluster(detectCores() - 2)
# clusterEvalQ(cl, library(tidyverse))
# clusterExport(cl, c("bootstrap_artists", "st"))
# x <- data.frame(users = rep(c(seq(10, 40, 10), seq(50, 400, 50), seq(500, 2000, 100), seq(2200, 3800, 200)), each = 10))
# x$artists <- parSapply(cl, x$users, bootstrap_artists)
# stopCluster(cl)
# save(x, file = "data/artists_growth.RData")
###### Analysis ######

load("data/artists_growth.RData")

y <- group_by(x, users) %>% 
  summarise(artists = mean(artists)) %>% 
  rbind(data.frame(users = nrow(us), artists = length(unique(st$art_id))))

ggplot(y, aes(users, artists)) +
  geom_point()

ggplot(y, aes(users^0.465, artists)) +
  geom_point() +
  geom_smooth(method = "lm")

# Power law where alpha = 0.465
reg <- lm(artists ~ I(users^0.465), data = y)

# On atteint les 3M d'artistes du catalogues avec 4M d'usagers.
predict(reg, newdata = data.frame(users= seq(3e06, 6e06, 1e06))
        )
## Mais peu convaincant parce que ne suppose pas de plafond...

# Quelle stratégie pour modéliser? C'est compliqué. Il s'agit d'une logistic curve
# dont on souhaite estimer l'asymptote. Il semble que ça se fasse dans les modèles
# écologiques (pour mesurer la croissance des populations) ainsi que dans les modèles
# en géologie pour estimer les ressources naturelles (petrole). mais complexe à mettre
# en oeuvre. j'ai essayé de jouer avec la fonction nls et avec SSlogis. Le problème est 
# celui des paramètres et de la caractérisation de la fonction, mais c'est un problème
# mathématique autant que statistique. Pour le moment, rien de satisfaisant. 24 juillet 2017
# 
# Une piste:
# https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/


# Après plus de recherche, conclusion: les données dont on dispose ressemblent à une power law ; 
# on ne peut pas aller plus loin sans données supplémentaires.
# On abandonne donc l'idée de savoir jusqu'ou ça va.