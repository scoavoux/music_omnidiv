library(tidyverse)
library(devtools)
#devtools::install("~/Projets/triversity")
library(triversity)
load("data/streams.RData")

## take a random subset of users
st <- filter(st, !is.na(genre))

#st <- slice(st, sample(seq_len(nrow(st)), 1e5))

st <- select(st, user_id, art_id, genre) 

get_tripartite_from_df <- function(df, lev1, lev2, lev3){
  data.frame(V1 = c(rep(1, nrow(df)),
                    rep(2, nrow(df))),
             V2 = c(df[[lev1]],
                    df[[lev2]]),
             V3 = c(rep(2, nrow(df)),
                    rep(3, nrow(df))),
             V4 = c(df[[lev2]],
                    df[[lev3]]))# %>% 
  #  get_multipartite(data = .)
}

get_tripartite_from_df_w <- function(df, lev1, lev2, lev3){
  df <- aggregate()
  data.frame(V1 = c(rep(1, nrow(df)),
                    rep(2, nrow(df))),
             V2 = c(df[[lev1]],
                    df[[lev2]]),
             V3 = c(rep(2, nrow(df)),
                    rep(3, nrow(df))),
             V4 = c(df[[lev2]],
                    df[[lev3]]))#%>% 
  #  get_multipartite(data = .)
}

rd <- count(st, user_id, art_id)
dic <- distinct(st, art_id, genre)
df <- data.frame(part_from = c(rep(1, nrow(rd)),
                               rep(2, nrow(dic))),
                 node_from = c(rd$user_id,
                               dic$art_id),
                 part_to = c(rep(2, nrow(rd)),
                             rep(3, nrow(dic))),
                 node_to = c(rd$art_id, 
                             dic$genre),
                 weight = c(rd$n,
                            rep(1, nrow(dic))))

#df <- get_tripartite_from_df(st, "user_id", "art_id", "genre")
ex <- get_multipartite(data = df)


user_type_div <- get_diversity_from_path(graph = ex, path = c(1, 2, 3), measure = "entropy")
user_art_div <- get_diversity_from_path(graph = ex, path = c(1, 2), measure = "entropy")

overall_div <- list(type = get_diversity_from_path(graph = ex, path = c(1, 2, 3), measure = "entropy", type = "collective"),
                    artists = get_diversity_from_path(graph = ex, path = c(1, 2), measure = "entropy", type = "collective"))

load("data/french_users.RData")


df <- data.frame(user_type_div) %>% rownames_to_column() %>% rename(type_div = entropy) %>% 
  full_join(data.frame(user_art_div) %>% rownames_to_column() %>% rename(art_div = entropy))

us <- left_join(us, df, by = c(user_id = "rowname"))

ggplot(us, aes(div_artists, 2^art_div)) +
  geom_point() +
  coord_fixed()
cor(us$div_artists, 2^us$art_div, use = "complete")
plot(us$div_genre, us$type_div)

data("tripartite_example")
ex <- get_multipartite(data = tripartite_example)
x <- get_diversity_from_path(ex, c(1, 2), measure = "entropy")
data.frame(x) %>% rownames_to_column() %>% rename(id = rowname) %>% str()


system.time(
count(st, user_id, art_id) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_artists = prod(f^f)^-1))
