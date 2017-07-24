load("data/streams.RData")
load("data/songs_artists.RData")
load("data/genres.RData")

library(tidyverse)
library(knitr)
library(FactoMineR)

###### Classifier les genres ######

# Etudier l'association entre les genres à partir des co-écoutes
# genres sont liés si mêmes personnes écoutent les deux
# NMAX <- Inf
source("scripts/import.R")

df <- count(st, user_id, genre) %>% 
  filter(!is.na(genre)) %>% 
  group_by(user_id) %>% 
  mutate(n = n/sum(n)) %>% 
  spread(genre, n, fill = 0)
  
gpca <- PCA(df[, -1], graph = FALSE)
plot(gpca, choix = "var")
ghc <- HCPC(gpca, graph = FALSE)

plot(ghc, choice = "tree")

###### Classifier dans un genre ######

## Approche par les kmeans
sol <- so[so$alb_id %in% filter(genres, name == "Alternative")$alb_id, ]

stl <- filter(st, sng_id %in% sol$sng_id)

# peut changer distinct par count
df <- distinct(stl, user_id, art_id) %>% 
  mutate(x = 1) %>% 
  spread(art_id, x, fill = 0) %>% 
  select(-user_id)

df <- df[, colSums(df) > 20]
artid <- colnames(df)
df <- as.matrix(df) %>% t()
row.names(df) <- artid

## clara fonctionne mal: produit un seul gros cluster
## => on préfère kmeans
classif <- kmeans(df, 10)
d <- data_frame(art_id = as.integer(artid), cluster = classif$cluster) %>% 
  left_join(ar)

freq(d$cluster)

# à adapter; filter les plus fort cluster
filter(d, cluster != 10) %>% arrange(cluster) %>% kable()


install.packages("randomForest")
library(randomForest)
randomForest()