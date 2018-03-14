# author: Samuel Coavoux
# date: avril 2017

## Produire une base de données (un ensemble de bases)
## propres à partir des données brutes Deezer

###### Packages ######
library(tidyverse)
library(lubridate)
library(stringr)

###### Genres ######
## Produire le fichier des genres:
# source("scripts/genres_codage.R")
load("data/genres.RData")

###### Songs ######

so <- read_tsv("data/orig/song_catalog.tsv", 
               col_names = c("sng_id", "art_id", "alb_id", "label_id", 
                             "digital_release", "physical_release", "sng_title", "duration"))


## Corriger les chansons dont la durée est abérrante (> 2h)
so <- mutate(so, duration = ifelse(duration > 3600*2, NA, duration)) %>% 
## Corriger les dates
  mutate_at(.vars = vars(ends_with("_release")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd()))

## On récupère les valeurs manquantes (en tous cas la plupart)
## à partir d'un nouveau scraping de Deezer
## à refaire un jour en refaisant le scrapping genres...
## pour avoir vraiment toutes les dates de la même source
load("data/dates_raw.RData")
dates <- bind_rows(l) %>% 
  mutate(release_date = ifelse(release_date == "0000-00-00", NA, release_date))

so <- left_join(so, dates, by = "alb_id") %>% mutate(release_date = ymd(release_date))

so <-mutate(so, 
           release_date = 
             ifelse(!is.na(release_date), 
                    release_date, 
                    ifelse(physical_release < digital_release, 
                           physical_release,
                           digital_release)) %>% 
             as.Date(origin = "1970-01-01") %>% 
             ifelse(. > ymd("2015-01-01"), NA, .) %>% 
             as.Date(origin = "1970-01-01"),
           release_year = year(release_date),
           nouveaute = factor(release_year >= 2013, levels = c(TRUE, FALSE), labels = c("Nouveauté", "Pas une nouveauté")))

rm(dates, l, i)

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

## Supprimer un certain nombre de variables inutilisées
st <- mutate(st, online = factor(is.na(timestamp_off), levels = c(TRUE, FALSE), labels = c("En ligne", "Hors-ligne"))) %>% 
  select(-type_stream, -country, -context_id, -app_id, -timestamp_off, -timestamp_sync, -pause, -seek)

## Vérifier que longueur moindre que longueur de la piste
## + ajouter année de diffusion
st <- select(so, sng_id, art_id, alb_id, duration, release_year, nouveaute) %>% 
  right_join(st, by = "sng_id") %>% 
  mutate(length = ifelse(length > duration, duration, length))

# 5% de durée d'écoute négative... NA?
# sum(st$length < 0) / nrow(st)
st$length[st$length < 0] <- NA

## Enlever les écoutes avant le 1er avril 2014
st <- filter(st, (timestamp >= 1396310400 & timestamp < 1420113600) | is.na(timestamp))

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
             offer_id2 = ifelse(offer_id > 1, 1, offer_id) %>% 
               factor(levels = 0:1,
                      labels = c("Free", "Premium")),
             offer_id = ifelse(offer_id > 2, 3, offer_id) %>% 
               factor(levels = 0:3,
                      labels = c("Free", "Premium", "Premium+", "Partenaire")))

## First listen of artist
st <- arrange(st, user_id, art_id, timestamp) %>% 
  group_by(user_id, art_id) %>% 
  mutate(n_reecoute_art = n(),
         first_listen_art = factor(row_number() == 1, levels = c(TRUE, FALSE), labels = c("First artist listen", "Not first artist listen"))) %>% 
    ungroup() %>% 
  arrange(timestamp)


## Reliquat du code de Sisley; pas utilisé.
# st <- st %>%  mutate(activite = case_when(.$type_stream != "MOD" ~ "Passive",
#                                           .$type_stream == "MOD" & .$context_name %in%
#                                             c("radio_page", "feed_user_radio" , "collection_radio" ,
#                                               "profile_radios", "artist_smartradio", "smartradio_page",
#                                               "feed_smartradio", "playlist_radio", "notification_genreradio") ~ "Passive",
#                                           .$context_name != "unknown" & !is.na(.$context_name) ~ "Active",
#                                           TRUE ~ NA_character_) %>% factor())




## Définition des "stars"
### Après discussion avec JS le 11 mai 2017
### On définit les stars par le nb de personnes uniques différentes 
### qui écoutent (plutôt que nombre d'écoutes) +
### On prend les seuils suivants (quantiles)
### + 0    ; .001 : star
### + .001 ; .01  : higher midtail
### + .01  ; .1   : lower midtail
### + .1  ;  1    : long tail

## Popularité des titres et artistes
### Note: contrairement à ce que dit la doc de dplyr
### length(unique(x)) est BCP plus rapide que n_distinct()
sng_pop <- group_by(st, sng_id) %>% 
  summarize(nb_aud_sng = length(unique(user_id))) %>% 
  mutate(sng_pop = cut(nb_aud_sng, 
                       breaks = rev(quantile(nb_aud_sng, probs = 1 - c(0, 0.001, 0.01, 0.1, 1))),  
                       labels = c("Long tail", "Lower mid-tail", "Higher mid-tail", "Star"), 
                       include.lowest = TRUE)) %>% 
  select(-nb_aud_sng)

art_pop <- group_by(st, art_id) %>% 
  summarize(nb_aud_art = length(unique(user_id))) %>% 
  mutate(art_pop = cut(nb_aud_art, 
                       breaks = rev(quantile(nb_aud_art, probs = 1 - c(0, 0.001, 0.01, 0.1, 1))),  
                       labels = c("Long tail", "Lower mid-tail", "Higher mid-tail", "Star"), 
                       include.lowest = TRUE)) %>% 
  select(-nb_aud_art)

st <- left_join(st, sng_pop) %>% left_join(art_pop)
  
rm(sng_pop, art_pop)
gc()

## Nouveautés

###### Users ######

# source("scripts/localisation.R")
load("data/cities.RData")

us <- read_tsv("data/orig/orange_user_detail.tsv", 
               col_names = c("user_id", "date_birth", "gender", "date_registered", "city"), 
               col_types = "ccccc")

## Mettre les dates dans le bon format ; calculer l'âge
us <- mutate_at(us, .vars = vars(starts_with("date_")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd())) %>% 
  ## supprimer les plus jeunes (ages aberrants)
  mutate(age = 2013 - year(date_birth),
         age = ifelse(age < 10, NA, age)) %>% 
  ## Corriger les types de vecteur
  mutate(gender = factor(gender))

us <- mutate(us, city = tolower(city) %>% chartr("éèêëàâäîïùûüôöç ", 
                                                 "eeeeaaaiiuuuooc-", 
                                                 .) %>% str_trim()) %>% 
  left_join(cities, by = c("city" = "origname"))

## Ancienneté sur Deezer
us <- mutate(us, 
             anciennete_days = as.numeric(ymd("20140407") - date_registered),
             anciennete_cat = cut(anciennete_days, 
                                  breaks = c(-1, 7, 365, 365*2, 365*5, Inf),
                                  labels = c("Nouveaux utilisateurs", "Moins d'un an", "Un à deux ans", "Deux à cinq ans", "Plus de cinq ans")))


## Type d'offre
us <- count(st, user_id, offer_id) %>% 
  group_by(user_id) %>% 
  filter(n == max(n)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(user_id, offer_id) %>% 
  right_join(us, by = "user_id")

us <- count(st, user_id, offer_id2) %>% 
  group_by(user_id) %>% 
  filter(n == max(n)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(user_id, offer_id2) %>% 
  right_join(us, by = "user_id")

###### Fav songs ######

## Une manière d'accélerer ce code serait de fusionner d'abord les différents fichiers de favori, 
## puis de les merger avec St/us en une seule fois, plutôt que les multiples passages que l'on fait ici

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
  mutate(fav_song = factor(ifelse(timestamp_favsong <= timestamp & !is.na(timestamp_favsong), "Favorite", "Not favorite")))

###### Fav artists ######
far <- read_tsv("data/orig/fav_artists.tsv",
                col_names = c("user_id", "art_id", "add", "timestamp_favartist"))

## Idem pour artistes
us <- filter(far, add == 1) %>% 
  count(user_id) %>% 
  rename(nb_fav_artists = n) %>% 
  right_join(us, by = "user_id")

st <- left_join(st, filter(far, add == 1) %>% 
                  select(-add) %>% 
                  group_by(user_id, art_id) %>% 
                  mutate(timestamp_favartist = min(timestamp_favartist)) %>% 
                  ungroup() %>% 
                  distinct()
) %>% 
  mutate(fav_artist = factor(ifelse(timestamp_favartist <= timestamp & !is.na(timestamp_favartist), "Favorite", "Not favorite"))) %>% 
  group_by(user_id, art_id) %>% 
  mutate(fav_artist_other_song = factor(ifelse(any(fav_artist == "Favorite"), min(timestamp), 9999999999) <= timestamp, 
                                        levels = c(TRUE, FALSE), 
                                        labels = c("Favorite", "Not favorite"))) %>% 
  ungroup()

###### Fav albums ######
fal <- read_tsv("data/orig/fav_albums.tsv",
                col_names = c("user_id", "alb_id", "add", "timestamp_favalbum"))
# sts <- st
st <- left_join(st, 
                filter(fal, add == 1) %>% 
                  select(-add) %>% 
                  group_by(user_id, alb_id) %>% 
                  mutate(timestamp_favalbum = min(timestamp_favalbum)) %>% 
                  ungroup() %>% 
                  distinct()
) %>% 
  mutate(fav_album = factor(ifelse(timestamp_favalbum <= timestamp & !is.na(timestamp_favalbum), "Favorite", "Not favorite"))) %>% 
  group_by(user_id, alb_id) %>% 
  mutate(fav_album_other_song = factor(ifelse(any(fav_album == "Favorite"), min(timestamp), 9999999999) <= timestamp, 
                                       levels = c(TRUE, FALSE), 
                                       labels = c("Favorite", "Not favorite"))) %>% 
  ungroup()

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

st <- st %>% mutate(context_name = ifelse((fav_song == "Favorite" | fav_artist == "Favorite" | fav_artist_other_song == "Favorite" | fav_album == "Favorite" | fav_album_other_song  == "Favorite") & 
                                            context_name %in% c("search_page", "album_page", "track_page", "playlist_page", "unknown"),
                                          "favorite",
                                          context_name))
st <- select(st, -starts_with("timestamp_fav"), -starts_with("fav"))
gc()
st$context_cat <- factor(context_cat_dic[st$context_name])

## Temporalité de l'écoute
st <- mutate(st,
             week = week(timestamp),
             wday = lubridate::wday(timestamp, label = TRUE),
             yday = yday(timestamp),
             hour = hour(timestamp))

## Genres
st <- left_join(st, select(genres, genre = name, alb_id), by = "alb_id")
so <- left_join(so, select(genres, genre = name, alb_id), by = "alb_id")

###### Appareiller streams et users######
st <- mutate(st, guid = ifelse(context_cat %in% c("ND", "unknown"),
                               NA,
                               ifelse(context_cat %in% c("stock", "search", "artist_disco", "artist_top", "ND", "unknown") | is.na(context_cat), # que faire de artist_top
                                      "Non guidée",
                                      "Guidée")) %>% 
               factor(),
             type_guid = ifelse(is.na(guid),
                                NA,
                                ifelse(guid == "Non guidée", 
                                       "Non guidée",
                                       ifelse(context_cat %in% c("experts_editor", "explore_release", "social", "player_defaut", "top", "feed_playlist", "feed_track", "feed_album"), 
                                              "Guidage", 
                                              "Flux"))) %>% 
               factor()) 

us <- group_by(st, user_id) %>% 
  summarise(nb_ecoutes = n(),
            nb_guid = sum(guid == "Guidée", na.rm=TRUE),
            nb_edit = sum(type_guid == "Guidage", na.rm=TRUE),
            nb_flux = sum(type_guid == "Flux", na.rm=TRUE),
            fq = nb_guid / sum(!is.na(guid)),
            fa = nb_flux / sum(!is.na(type_guid)),
            fe = nb_edit / sum(!is.na(type_guid)),
            fg = sum(type_guid == "Guidage", na.rm=TRUE) / sum(type_guid != "Non guidée" & !is.na(type_guid)),
            ## Passivité: ajouter une modalité quand aucune écoute passive
            passifs = cut(fq, breaks = c(-.1, .00001, 0.8, 1.1), labels = c("Usagers exclusivement actifs", "Usagers mixtes", "Usagers principalement passifs")),
            nb_tracks = n_distinct(sng_id),
            nb_artists = n_distinct(art_id), 
            volume_ecoute = sum(length, na.rm = TRUE), 
            freq_mobile = sum(app_type == "mobile", na.rm = TRUE)/n(),
            freq_desktop = sum(app_type == "desktop", na.rm = TRUE)/n(),
            freq_radio = sum(context_cat %in% c("feed_radio", "radio_editoriale", "radio_flow", "smart_radio"), na.rm = TRUE) / n(),
            nb_dispositifs = n_distinct(context_cat), 
            freq_star_sng = sum(sng_pop == "Star", na.rm = TRUE) / n(),
            freq_star_art = sum(art_pop == "Star", na.rm = TRUE) / n(),
            freq_longtail_art = sum(art_pop == "Long tail", na.rm = TRUE) / n(),
            freq_nouveaute = sum(nouveaute == "Nouveauté", na.rm = TRUE) / sum(!is.na(nouveaute))) %>% 
  right_join(us, by = "user_id")
us$passifs[us$nb_ecoutes < 100] <- NA


## Diversité des genres écoutés

# On utilise les métriques de la présentation de Robin Lamarche-Perrin et al
# Par conséquent:
#   + richness = richness
#   + shannon = exponentielle de entropie de shannon
#   + herfindahl = inverse de index de Herfindahl
#   + BergerParker = inverse de index de Berger-Parker
# On ne conserve pour le moment que l'entropie de Shannon
# Propriétés: maximum = nombre de genres disponibles
# 
us <- count(st, user_id, genre) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(#div_richness = length(unique(genre)),
            div_genre = prod(f^f)^-1
            # div_herfindahl = sum(f^2)^-1,
            # div_bergerparker = max(f)^-1
            ) %>% 
  right_join(us)

## Diversité des artistes écoutés
us <- count(st, user_id, art_id) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_artists = prod(f^f)^-1) %>% 
  right_join(us)

## Diversité des chansons écoutés
us <- count(st, user_id, sng_id) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_sng = prod(f^f)^-1) %>% 
  right_join(us)

## Diversité des dispositifs employés
us <- count(st, user_id, context_cat) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_disp = prod(f^f)^-1) %>% 
  right_join(us)

## favoris
us <- mutate(us, 
             nb_fav_artists = ifelse(is.na(nb_fav_artists), 0, nb_fav_artists),
             nb_favorites = ifelse(is.na(nb_favorites), 0, nb_favorites))
###### Fav albums ######

# alf <- read_tsv("data/orig/album_fav.tsv", 
#                 col_names = c("alb_id", "art_id", "label_id", "digital_release",
#                               "physical_release", "alb_title", "rank", "nb_fans"))


###### Selection  ######
source("scripts/filter_users.R")

## Songs and artists
ar <- group_by(st, art_id) %>% 
  summarise(n_listen = n(),
            n_users = n_distinct(user_id),
            freq_guid = sum(guid == "Guidée", na.rm=TRUE) / sum(!is.na(guid))) %>% 
  right_join(ar, by = "art_id")


save(fs, fal, far, file = "data/favorites.RData")
save(us, file = "data/french_users.RData")
save(so, ar, file = "data/songs_artists.RData")
save(st, file = "data/streams.RData")
