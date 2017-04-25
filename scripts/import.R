###### Packages ######
library(tidyverse)
library(lubridate)

###### Streams ######
st <- read_tsv("data/orig/stream.tsv", 
               col_names = c("user_id", "sng_id", "type_stream", "country", "length", "context_name", "context_id", "app_id", "app_type", "offer_id", "timestamp_off", "timestamp_sync", "pause", "seek", "timestamp"))

# remove rows that are badly imported (around 300 => we don't care)
n <- unique(problems(st)$row)
st <- st[-n, ]
rm(n)

## Enlever les usagers dont l'usage semble abberrant

## Calculer nombre de tracks et temps cumulé d'écoute 
u <- group_by(st, user_id) %>% 
  summarize(n = n(),
            t = sum(length)/(60*60*24))

# ggplot(u, aes(x = n)) + geom_histogram() + scale_x_log10()
# ggplot(u, aes(x = t)) + geom_histogram() + scale_x_log10()

## Supprimer sur la base ci-dessus (plus de 40000 tracks ou plus de 100h d'écoutes cumulées)
st <- anti_join(st, filter(u, n > 4e04 | t > 1e02), by = "user_id")
rm(u)

## Enlever les écoutes avant le 1er avril 2014
st <- filter(st, timestamp >= 1396310400 | is.na(timestamp))

# Mettre les données au bon format
st <- mutate(st, 
             type_stream = factor(type_stream, 
                                  levels = 0:2, 
                                  labels = c("MOD", "smartradio", "radio")),
             timestamp = parse_date_time(timestamp, tz="Europe/Paris"),
             # Déjà recodé, on a pas les données brutes annoncées dans le dictionnaire des variables
             app_type = factor(app_type,
                               levels = levels = c("desktop", "mobile", "tablet", "web")),
             offer_id = ifelse(offer_id > 2, 3, offer_id) %>% 
               factor(levels = 0:3,
                      labels = c("Free", "Premium", "Premium+", "Partenaire")))

save(st, file = "data/streams.RData")

###### Users ######

us <- read_tsv("data/orig/orange_user_detail.tsv", 
               col_names = c("user_id", "date_birth", "gender", "date_registered", "city"), 
               col_types = "ccccc")
# TODO: enlever les valeurs aberrantes (grands nombre de streams; cf. code Sisley)

## Mettre les dates dans le bon format ; calculer l'âge
us <- mutate_at(us, .cols = vars(starts_with("date_")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd())) %>% 
  ## supprimer les plus jeunes (ages aberrants)
  mutate(age = 2013 - year(date_birth),
         age = ifelse(age < 10, NA, age)) %>% 
  ## Corriger les types de vecteur
  mutate(gender = factor(gender))

# save
save(us, file = "data/french_users.RData")

###### Songs ######

so <- read_tsv("data/orig/song_catalog.tsv", 
               col_names = c("sng_id", "art_id", "alb_id", "label_id", 
                             "digital_release", "physical_release", "sng_title", "duration"))

###### Artists ######

ar <- read_tsv("data/orig/artist_catalog.tsv", 
               col_names = c("art_id", "art_name", "rank_artist", "artist_fans"))

save(so, ar, file = "data/songs_artists.RData")

###### Fav songs ######

fs <- read_tsv("data/orig/fav_songs.tsv", 
               col_names = c("user_id", "X2", "sng_id", "timestamp"))

###### Fav albums ######

fal <- read_tsv("data/orig/fav_albums.tsv",
                col_names = c("user_id", "alb_id", "add", "timestamp"))

###### Fav artists ######
far <- read_tsv("data/orig/fav_artists.tsv",
                col_names = c("user_id", "art_id", "add", "timestamp"))

###### Fav albums ######

alf <- read_tsv("data/orig/album_fav.tsv", 
                col_names = c("alb_id", "art_id", "label_id", "digital_release",
                              "physical_release", "alb_title", "rank", "nb_fans"))

save(fs, fal, far, alf, file = "data/favorites.RData")
