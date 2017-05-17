load("data/streams.RData")
load("data/songs_artists.RData")
load("data/genres.RData")

library(tidyverse)


so <- so[so$alb_id %in% filter(genres, name == "Rap/Hip Hop")$alb_id, ]

st <- filter(st, sng_id %in% so$sng_id)

# peut changer distinct par count
df <- distinct(st, user_id, art_id) %>% 
  mutate(x = 1) %>% 
  spread(art_id, x, fill = 0) %>% 
  select(-user_id)

df <- df[, colSums(df) > 5]
artid <- colnames(df)
df <- as.matrix(df) %>% t()
row.names(df) <- artid

## clara fonctionne mal: produit un seul gros cluster
## => on préfère kmeans
classif <- kmeans(df, 10)
d <- data_frame(art_id = as.integer(artid), cluster = classif$cluster) %>% 
  left_join(ar)

library(knitr)
filter(d, cluster != 5) %>% arrange(cluster) %>% kable()

freq(d$cluster)
