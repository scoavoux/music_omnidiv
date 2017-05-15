###### Packages ######
library(tidyverse)
library(lubridate)

###### Songs ######

so <- read_tsv("data/orig/song_catalog.tsv", 
               col_names = c("sng_id", "art_id", "alb_id", "label_id", 
                             "digital_release", "physical_release", "sng_title", "duration"))


## Corriger les chansons dont la durée est abérrante (> 2h)
so <- mutate(so, duration = ifelse(duration > 3600*2, NA, duration)) %>% 
## Corriger les dates
  mutate_at(.cols = vars(ends_with("_release")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd())) %>% 
## Nouveauté
  mutate(nouveaute = factor(year(physical_release) >= 2014 | year(digital_release) >= 2014, levels = c(TRUE, FALSE), labels = c("Nouveauté", "Pas une nouveauté")))

###### Artists ######

ar <- read_tsv("data/orig/artist_catalog.tsv", 
               col_names = c("art_id", "art_name", "rank_artist", "artist_fans"))


###### Streams ######
if(!exists("NMAX")) NMAX <- Inf

st <- read_tsv("data/orig/stream.tsv", 
               col_names = c("user_id", "sng_id", "type_stream", "country", "length", "context_name", "context_id", 
                             "app_id", "app_type", "offer_id", "timestamp_off", "timestamp_sync", "pause", "seek", "timestamp")
               # Prévoir la possibilité de réduire la taille du fichier pour l'exploration interactive en local
               , n_max = NMAX
               )


## Note: Code de Sisley ne documente pas comment elle a fait.
## Simplement, elle supprime certaines personnes par leur index
## cf. Database_create.do, l. 26-53
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
             ## Reliquat: pas utilisé; cf. bout de code ci-dessous
             # type_stream = factor(type_stream, 
             #                      levels = 0:2, 
             #                      labels = c("MOD", "smartradio", "radio")),
             timestamp = as_datetime(as.numeric(timestamp), tz="Europe/Paris"), 
             # Déjà recodé, on a pas les données brutes annoncées dans le dictionnaire des variables
             app_type = factor(app_type,
                               levels = c("desktop", "mobile", "tablet", "web")),
             offer_id = ifelse(offer_id > 2, 3, offer_id) %>% 
               factor(levels = 0:3,
                      labels = c("Free", "Premium", "Premium+", "Partenaire")))


## Reliquat du code de Sisley; pas utilisé.
# st <- st %>%  mutate(activite = case_when(.$type_stream != "MOD" ~ "Passive",
#                                           .$type_stream == "MOD" & .$context_name %in%
#                                             c("radio_page", "feed_user_radio" , "collection_radio" ,
#                                               "profile_radios", "artist_smartradio", "smartradio_page",
#                                               "feed_smartradio", "playlist_radio", "notification_genreradio") ~ "Passive",
#                                           .$context_name != "unknown" & !is.na(.$context_name) ~ "Active",
#                                           TRUE ~ NA_character_) %>% factor())


## Temps d'écoute

## Vérifier que longueur moindre que longueur de la piste
## + ajouter année de diffusion
st <- select(so, sng_id, art_id, duration) %>% 
  right_join(st, by = "sng_id") %>% 
  mutate(length = ifelse(length > duration, duration, length))

# 5% de durée d'écoute négative... NA?
# sum(st$length < 0) / nrow(st)
st$length[st$length < 0] <- NA

## Définition des "stars"
### Après discussion avec JS le 11 mai 2017
### On définit les stars par le nb de personnes uniques différentes 
### qui écoutent (plutôt que nombre d'écoutes) +
### On prend les seuils suivants (quantiles)
### + 0    ; .001 : star
### + .001 ; .01  : higher midtail
### + .01  ; .05  : lower midtail
### + .05  ;  1   : long tail

## Popularité des titres et artistes
### Note: contrairement à ce que dit la doc de dplyr
### length(unique(x)) est BCP plus rapide que n_distinct()
sng_pop <- group_by(st, sng_id) %>% 
  summarize(nb_aud_sng = length(unique(user_id))) %>% 
  mutate(sng_pop = cut(nb_aud_sng, 
                       breaks = rev(quantile(nb_aud_sng, probs = 1 - c(0, 0.001, 0.01, 0.05, 1))),  
                       labels = c("Long tail", "Lower mid-tail", "Higher mid-tail", "Star"), 
                       include.lowest = TRUE)) %>% 
  select(-nb_aud_sng)

art_pop <- group_by(st, art_id) %>% 
  summarize(nb_aud_art = length(unique(user_id))) %>% 
  mutate(art_pop = cut(nb_aud_art, 
                       breaks = rev(quantile(nb_aud_art, probs = 1 - c(0, 0.001, 0.01, 0.05, 1))),  
                       labels = c("Long tail", "Lower mid-tail", "Higher mid-tail", "Star"), 
                       include.lowest = TRUE)) %>% 
  select(-nb_aud_art)

st <- left_join(st, sng_pop) %>% left_join(art_pop)
  
rm(sng_pop, art_pop)
gc()

## Nouveautés

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



###### Fav songs ######

fs <- read_tsv("data/orig/fav_songs.tsv", 
               col_names = c("user_id", "X2", "sng_id", "timestamp_favsong"))
## Supprimer X2/variable au sens inconnu
fs <- select(fs, -X2)

## Supprimer les favoris après la fin de la période d'observation
fs <- filter(fs, timestamp_favsong < 1409529600)

## Ajouter variable nombre de chansons favorites
us <- count(fs, user_id) %>% 
  rename(nb_favorites = n) %>% 
  right_join(us, by = "user_id")

## Ajouter titre favori à streams
## trois niveaux: 
## + pas favori, 
## + pas encore favori (mais le deviendra dans la période)
## + favori

st <- left_join(st, fs, by = c("user_id", "sng_id")) %>% 
  mutate(fav_song = factor(case_when(is.na(.$timestamp_favsong) ~ "Not favorite",
                                .$timestamp_favsong <= .$timestamp ~ "Favorite",
                                .$timestamp_favsong > .$timestamp ~ "Not yet favorite"),
                      levels = c("Not favorite", "Not yet favorite", "Favorite")))


###### Fav artists ######
far <- read_tsv("data/orig/fav_artists.tsv",
                col_names = c("user_id", "art_id", "add", "timestamp_favartist"))

## Idem pour artistes
us <- filter(far, add == 1) %>% 
  count(user_id) %>% 
  rename(nb_fav_artists = n) %>% 
  right_join(us, by = "user_id")

st <- select(so, sng_id, nouveaute) %>% 
  right_join(st, by = "sng_id") %>% 
  left_join(filter(far, add == 1) %>% select(-add)) %>% 
  mutate(fav_artist = factor(case_when(is.na(.$timestamp_favartist) ~ "Not favorite",
                                     .$timestamp_favartist <= .$timestamp ~ "Favorite",
                                     .$timestamp_favartist > .$timestamp ~ "Not yet favorite"),
                           levels = c("Not favorite", "Not yet favorite", "Favorite")))

## Idem pour aimer une autre chanson de l'artiste
## 
## 
# st <- select(so, sng_id, art_id) %>% 
#   right_join(st, by = "sng_id") %>% 
#   group_by(user_id, art_id) %>% 
#   mutate(fav_artist_other_song = factor(case_when(any(.$fav == "Favorite") ~ "Favorite",
#                                                   any(.$fav == "Not yet favorite") ~ "Not yet favorite",
#                                                   any(.$fav == "Not favorite") ~ "Not favorite"), 
#                                         levels = c("Not favorite", "Not yet favorite", "Favorite")))


context_cat_dic <- c( "tops_album" = "top",
                      "tops_playlist" = "top",
                      "tops_track" = "top",
                      "ticker_album"  = "social",
                      "ticker_playlist"  = "social",
                      "ticker_track"  = "social",
                      "recommendations_friend_share_album"  = "social",
                      "facebook_track" = "social",
                      "radio_page" = "radio_editoriale",
                      "artist_smartradio"  = "smartradio",
                      "smartradio_page" = "smartradio",
                      "profile_user_radio" = "radio_flow",
                      "playlist_radio" = "radio_flow",
                      "feed_user_radio" = "feed_radio",
                      "feed_smartradio" = "feed_smartradio",
                      "feed_album" = "feed_album",
                      "feed_playlist" = "feed_playlist",
                      "feed_track"  = "feed_track",
                      "suggest_track"  = "feed_track",
                      "notification_track" = "feed_track",
                      "artist_discography" = "artist_disco",
                      "artist_top" = "artist_top",
                      "explore_releases_album" = "explore_release",
                      "folder_page" = "stock",
                      "favorite" = "stock",
                      "history_page" = "stock",
                      "inapp_page" = "stock",
                      "loved_page" = "stock",
                      "collection_album" = "stock",
                      "collection_radio" = "stock",
                      "collection_playlist" = "stock",
                      "profile_albums" = "stock",
                      "profile_history" = "stock",
                      "profile_top" = "stock",
                      "profile_top_albums" = "stock",
                      "profile_top_tracks" = "stock",
                      "profile_playlists" = "stock",
                      "personnalsong_page" = "stock",
                      "profile_radios" = "stock",
                      "search_page" = "search",
                      "album_page" = "ND",
                      "track_page" = "ND",
                      "playlist_page" = "ND",
                      "unknown" = "unknown",
                      "player_default_playlist" = "player_defaut",
                      "selection_album" = "experts_editor",
                      "explore_picks_album" = "experts_editor",
                      "explore_region_album" = "experts_editor")

st <- st %>% mutate(context_name = ifelse(fav_artist == "Favorite" | fav_song == "Favorite",
                                          "favorite",
                                          context_name))
st$context_cat <- factor(context_cat_dic[st$context_name])

# context_cat= ifelse(context_cat   %in% c("feed_smartradio", "feed_radio", "feed_album", "feed_playlist", "feed_track"), "perso", context_cat)
# context_cat= ifelse(context_cat   %in% c("radio_editoriale", "smartradio", "radio_flow"), "radios", context_cat)
# context_cat= ifelse(context_cat   %in% c("artist_disco", "artist_top"), "contextuel", context_cat)

## Temporalité de l'écoute
st <- mutate(st, 
             week = week(timestamp),
             wday = lubridate::wday(timestamp, label = TRUE),
             yday = yday(timestamp),
             hour = hour(timestamp))

###### Fav albums ######
fal <- read_tsv("data/orig/fav_albums.tsv",
                col_names = c("user_id", "alb_id", "add", "timestamp_favalbum"))

###### Fav albums ######

alf <- read_tsv("data/orig/album_fav.tsv", 
                col_names = c("alb_id", "art_id", "label_id", "digital_release",
                              "physical_release", "alb_title", "rank", "nb_fans"))


###### Clean-up  ######
## Remove some unused variable so that knitting becomes possible
st <- select(st, -duration, -type_stream, -country, -context_id, -app_id, -timestamp_off, -timestamp_sync, -pause, -seek)
gc()

save(fs, fal, far, alf, file = "data/favorites.RData")
save(us, file = "data/french_users.RData")
save(so, ar, file = "data/songs_artists.RData")
save(st, file = "data/streams.RData")
