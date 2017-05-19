load("data/streams.RData")
load("data/songs_artists.RData")
load("data/genres.RData")

library(tidyverse)

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
library(knitr)
# à adapter; filter les plus fort cluster
filter(d, cluster != 10) %>% arrange(cluster) %>% kable()


install.packages("randomForest")
library(randomForest)
randomForest()