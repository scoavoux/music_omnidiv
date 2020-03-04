# author: Samuel Coavoux
# date: avril 2017-avril 2018
# Retapé en décembre 2019 pour les favoris

## Produire une base de données (un ensemble de bases)
## propres à partir des données brutes Deezer

###### Packages ######
library(tidyverse)
library(lubridate)
library(stringr)
library(here)

###### Genres ######
## Produire le fichier des genres:
# source("scripts/genres_codage.R")
load(here("data", "genres.RData"))

###### Songs ######

so <- read_tsv("data/orig/song_catalog.tsv", 
               col_names = c("sng_id", "art_id", "alb_id", "label_id", 
                             "digital_release", "physical_release", "sng_title", "duration"))


## Corriger les chansons dont la durée est abérrante (> 2h)
so <- mutate(so, duration = ifelse(duration > 3600*2, NA, duration)) %>% 
## Corriger les dates
  mutate_at(.vars = vars(ends_with("_release")), .funs = function(x){ ifelse(x == "null", NA, x) %>% ymd()})

## On récupère les valeurs manquantes (en tous cas la plupart)
## à partir d'un nouveau scraping de Deezer
## à refaire un jour en refaisant le scrapping genres...
## pour avoir vraiment toutes les dates de la même source
load(here("data", "dates_raw.RData"))

dates <- bind_rows(l) %>% 
  mutate(release_date = ifelse(release_date == "0000-00-00", NA, release_date))

so <- left_join(so, dates, by = "alb_id") %>% 
  mutate(release_date = ymd(release_date))

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
rm(NMAX)

## Supprimer un certain nombre de variables inutilisées
st <- mutate(st, online = factor(is.na(timestamp_off), levels = c(TRUE, FALSE), labels = c("En ligne", "Hors-ligne"))) %>% 
  select(-type_stream, -country, -app_id, -timestamp_off, -timestamp_sync, -pause, -seek)

## Vérifier que longueur moindre que longueur de la piste
## + ajouter année de diffusion
st <- select(so, sng_id, art_id, alb_id, duration, release_year, nouveaute) %>% 
  right_join(st, by = "sng_id") %>% 
  mutate(length = ifelse(length > duration, duration, length))

# 5% de durée d'écoute négative... NA?
# sum(st$length < 0) / nrow(st)
st <- mutate(st, length = ifelse(length < 0, NA, length))

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

## Enlever les écoutes avant le 1er avril 2014
st <- filter(st, year(timestamp) == 2014 & month(timestamp) %in% 4L:8L)

## First listen of artist
# st <- arrange(st, user_id, art_id, timestamp) %>% 
#   group_by(user_id, art_id) %>% 
#   mutate(n_reecoute_art = n(),
#          first_listen_art = factor(row_number() == 1, levels = c(TRUE, FALSE), labels = c("First artist listen", "Not first artist listen"))) %>% 
#     ungroup() %>% 
#   arrange(timestamp)


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
  summarize(nb_aud_sng = n_distinct(user_id)) %>% 
  mutate(sng_pop = cut(nb_aud_sng, 
                       breaks = rev(quantile(nb_aud_sng, probs = 1 - c(0, 0.001, 0.01, 0.1, 1))),  
                       labels = c("Long tail", "Lower mid-tail", "Higher mid-tail", "Star"), 
                       include.lowest = TRUE)) %>% 
  select(-nb_aud_sng)

art_pop <- group_by(st, art_id) %>% 
  summarize(nb_aud_art = n_distinct(user_id)) %>% 
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
load(here("data", "cities.RData"))

us <- read_tsv("data/orig/orange_user_detail.tsv", 
               col_names = c("user_id", "date_birth", "gender", "date_registered", "city"), 
               col_types = "ccccc")

## Mettre les dates dans le bon format ; calculer l'âge
us <- mutate_at(us, .vars = vars(starts_with("date_")), .funs = function(x){(ifelse(x == "null", NA, x) %>% ymd())}) %>% 
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
  mutate(fav_song = factor(ifelse(is.na(timestamp_favsong), 
                                  "Not favorite", 
                                  ifelse(timestamp_favsong <= timestamp, 
                                         "Already favorite",
                                         "To become favorite"))))

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
) 
st <- mutate(st, fav_artist = factor(ifelse(is.na(timestamp_favartist), 
                                    "Not favorite", 
                                    ifelse(timestamp_favartist <= timestamp, 
                                           "Already favorite",
                                           "To become favorite")))) #%>% 
# On supprimer le fav_artist_other song parce que ca n'a pas de sens
    # group_by(user_id, art_id) %>% 
  # mutate(fav_artist_other_song = factor(ifelse(any(fav_artist == "Already favorite"), min(timestamp), 9999999999) <= timestamp, 
  #                                       levels = c(TRUE, FALSE), 
  #                                       labels = c("Favorite", "Not favorite"))) %>% 
  # ungroup()

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
  mutate(fav_album = factor(ifelse(is.na(timestamp_favalbum), 
                                   "Not favorite", 
                                   ifelse(timestamp_favalbum <= timestamp, 
                                          "Already favorite",
                                          "To become favorite")))) #%>% 
  # group_by(user_id, alb_id) %>% 
  # mutate(fav_album_other_song = factor(ifelse(any(fav_album == "Already favorite"), min(timestamp), 9999999999) <= timestamp, 
  #                                      levels = c(TRUE, FALSE), 
  #                                      labels = c("Favorite", "Not favorite"))) %>% 
  # ungroup()

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
                      "playlist_page_guidage_editorial" = "playlist_page_guidage_editorial",
                      "playlist_page_personal_playlist" = "playlist_page_personal_playlist",
                      "unknown" = "unknown",
                      "player_default_playlist" = "player_defaut",
                      "selection_album" = "experts_editor",
                      "explore_picks_album" = "experts_editor",
                      "explore_region_album" = "experts_editor")

### Changement important par rapport à la première version (Réseaux)
### = on ne considère plus que favori + playlist = Stock
### + on conserve playlist_page dans context_cat
# st <- st %>% mutate(context_name = ifelse((fav_song == "Already favorite" | fav_artist == "Already favorite" | fav_artist_other_song == "Already favorite" | fav_album == "Already favorite" | fav_album_other_song  == "Already favorite") & 
#                                             context_name %in% c("search_page", "album_page", "track_page", "playlist_page", "unknown"),
#                                           "favorite",
#                                           context_name))

st <- st %>% mutate(context_name = ifelse((fav_song == "Already favorite" | fav_artist == "Already favorite" | fav_album == "Already favorite") &
                                            context_name %in% c("search_page", "album_page", "track_page", "unknown"),
                                          "favorite",
                                          context_name))

st <- select(st, -starts_with("timestamp_fav"), -starts_with("fav"))
gc()

# on ajoute un export intermédiaire pour pouvoir retraiter les données
# pour le diagnostic des playlists
# save(st, file = here("data", "streams_for_playlist.RData"))

## Traitement des playlists
## discuté dans document 00_Playlists.Rmd

load(here::here("data", "playlists.RData"))
pl <- filter(st, context_name == "playlist_page") %>% 
  mutate(id = as.numeric(context_id)) %>% 
  group_by(id) %>% 
  summarize(n_listen = n(),
            n_users = n_distinct(user_id)) %>% 
  right_join(pl, by = c("id"))

pl <- mutate(pl, 
       recl = case_when(
         fans >= 30 ~ "playlist_page_guidage_editorial",
         fans == 0 & n_users == 1 ~ "playlist_page_personal_playlist",
         TRUE ~ "unknown"),
       id = as.character(id)) %>% 
  select(id, recl)

st <- left_join(st, pl, by = c(context_id = "id"))
st <- mutate(st, context_name = ifelse(context_name == "playlist_page" & !is.na(recl), recl, context_name))

st <- mutate(st, context_cat = factor(context_cat_dic[context_name]))
st <- select(st, -recl)

rm(pl)

## Temporalité de l'écoute
st <- mutate(st,
             week = week(timestamp),
             wday = lubridate::wday(timestamp, label = TRUE),
             yday = yday(timestamp),
             hour = hour(timestamp))

## Genres
st <- left_join(st, select(genres, genre = name, alb_id), by = "alb_id")
so <- left_join(so, select(genres, genre = name, alb_id), by = "alb_id")

rm(genres)

### Genre classification by legitimacy level
omni_dic <- c(
  "Classical" = "Highbrow",
  "Jazz" = "Highbrow",
  
  "Rock" = "Middlebrow",
  "French songs" = "Middlebrow",
  "World music" = "Middlebrow",
  "Alternative" = "Middlebrow", 
  
  "Hip-hop" = "Lowbrow",
  "Metal" = "Lowbrow",
  "Dance" = "Lowbrow",
  "Pop" = "Lowbrow",
  "R&B" = "Lowbrow",
  "Soul" = "Lowbrow",
  "Electronic" = "Lowbrow",
  
  "Kids' music" = "Unclassified",
  "Musicals" = "Unclassified",
  "Blues" = "Unclassified",
  "Country & Folk" = "Unclassified",
  "Reggae" = "Unclassified", 
  "Movies/games" = "Unclassified")

st <- mutate(st, legit = omni_dic[genre] %>% factor(levels = c("Lowbrow", "Middlebrow", "Highbrow"))) #Unclassified

### Il reste un gros problème à résoudre par du scraping.
### Pour le moment, les playlists dominent, mais on ne sait pas
### si ce sont des playlists personnelles ou du guidage
### => il faut les scrapper ID par ID.
### Puis revoir le code ci-dessous
### Playlist personnelle => stock
### Playlist autres/institutionnelles => guidage
st <- mutate(st, guid = ifelse(context_cat %in% c("ND", "unknown"),
                               NA,
                               ifelse(context_cat %in% c("stock", "search", "artist_disco", "artist_top", "playlist_page_personal_playlist") | is.na(context_cat), # que faire de artist_top
                                      "Non guidée",
                                      "Guidée")) %>% 
               factor(),
             type_guid = ifelse(is.na(guid),
                                NA,
                                ifelse(guid == "Non guidée", 
                                       "Non guidée",
                                       ifelse(context_cat %in% c("experts_editor", "explore_release", "social", "player_defaut", "top", "feed_playlist", "feed_track", "feed_album", "playlist_page_guidage_editorial"), 
                                              "Guidage", 
                                              "Flux"))) %>% 
               factor()) 

###### Appareiller streams et users######

us <- group_by(st, user_id) %>% 
  summarise(nb_ecoutes = n(),
            nb_guid = sum(guid == "Guidée", na.rm=TRUE),
            nb_stock = sum(guid == "Non guidée", na.rm=TRUE),
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
#           volume_ecoute = sum(length, na.rm = TRUE), 
            freq_mobile = sum(app_type == "mobile", na.rm = TRUE) / nb_ecoutes,
            freq_desktop = sum(app_type == "desktop", na.rm = TRUE) / nb_ecoutes,
            freq_radio = sum(context_cat %in% c("feed_radio", "radio_editoriale", "radio_flow", "smart_radio"), na.rm = TRUE) / nb_ecoutes,
            nb_dispositifs = n_distinct(context_cat), 
            freq_star_sng = sum(sng_pop == "Star", na.rm = TRUE) / nb_ecoutes,
            freq_star_art = sum(art_pop == "Star", na.rm = TRUE) / nb_ecoutes,
            freq_longtail_art = sum(art_pop == "Long tail", na.rm = TRUE) / nb_ecoutes,
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
  filter(!is.na(genre)) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_genre = prod(f^f)^-1) %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

## Diversité des artistes écoutés
us <- count(st, user_id, art_id) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_artists = prod(f^f)^-1) %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

## Diversité des niveaux de légitimité
us <- select(st, legit, user_id) %>% 
  filter(legit != "Unclassified", !is.na(legit)) %>% 
  count(user_id, legit) %>% 
  group_by(user_id) %>% 
  mutate(f = n/sum(n)) %>% 
  select(-n) %>% 
  spread(legit, f, fill=0) %>% 
  mutate(div_omni = ((Highbrow^Highbrow)*(Middlebrow^Middlebrow)*(Lowbrow^Lowbrow))^-1,
         div_omni_rich = ceiling(Lowbrow) + ceiling(Middlebrow) + ceiling(Highbrow)) %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

## Diversités stock vs. edit
### Artists
us <- filter(st, !is.na(guid)) %>% 
  count(user_id, guid, art_id) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_art = prod(f^f)^-1) %>% 
  spread(guid, div_art) %>% 
  rename(div_artists_stock = "Non guidée",
         div_artists_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

us <- filter(st, !is.na(type_guid), type_guid != "Non guidée") %>% 
  count(user_id, type_guid, art_id) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_art = prod(f^f)^-1) %>% 
  spread(type_guid, div_art) %>% 
  rename(div_artists_rec_edit = "Guidage",
         div_artists_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

us <- filter(st, !is.na(guid), !is.na(genre)) %>% 
  count(user_id, guid, genre) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_genre = prod(f^f)^-1) %>% 
  spread(guid, div_genre) %>% 
  rename(div_genre_stock = "Non guidée",
         div_genre_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

us <- filter(st, !is.na(type_guid), type_guid != "Non guidée", !is.na(genre)) %>% 
  count(user_id, type_guid, genre) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_genre = prod(f^f)^-1) %>% 
  spread(type_guid, div_genre) %>% 
  rename(div_genre_rec_edit = "Guidage",
         div_genre_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")


us <- filter(st, !is.na(guid), !is.na(legit)) %>% 
  count(user_id, guid, legit) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = prod(f^f)^-1) %>% 
  spread(guid, div_omni) %>% 
  rename(div_omni_stock = "Non guidée",
         div_omni_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")

us <- filter(st, !is.na(type_guid), type_guid != "Non guidée", !is.na(legit)) %>% 
  count(user_id, type_guid, legit) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = prod(f^f)^-1) %>% 
  spread(type_guid, div_omni) %>% 
  rename(div_omni_rec_edit = "Guidage",
         div_omni_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(us, by = "user_id")


## Specific diversity not defined where stock/rec too low:
us <- mutate_at(us, vars(ends_with("_stock")), funs(ifelse(nb_stock < 100, NA, .))) %>% 
  mutate_at(vars(ends_with("_rev")), funs(ifelse(nb_guid < 100, NA, .)))
  
# ## Diversité des chansons écoutés
# us <- count(st, user_id, sng_id) %>% 
#   group_by(user_id) %>% 
#   mutate(f = n / sum(n)) %>% 
#   summarize(div_sng = prod(f^f)^-1) %>% 
#   right_join(us)
# 
# ## Diversité des dispositifs employés
# us <- count(st, user_id, context_cat) %>% 
#   group_by(user_id) %>% 
#   mutate(f = n / sum(n)) %>% 
#   summarize(div_disp = prod(f^f)^-1) %>% 
#   right_join(us)

## favoris
us <- mutate(us, 
             nb_fav_artists = ifelse(is.na(nb_fav_artists), 0, nb_fav_artists),
             nb_favorites = ifelse(is.na(nb_favorites), 0, nb_favorites))
###### Fav albums ######

# alf <- read_tsv("data/orig/album_fav.tsv", 
#                 col_names = c("alb_id", "art_id", "label_id", "digital_release",
#                               "physical_release", "alb_title", "rank", "nb_fans"))


###### Selection  ######
source(here("import", "filter_users.R"))

## Songs and artists
# ar <- group_by(st, art_id) %>% 
#   summarise(n_listen = n(),
#             n_users = n_distinct(user_id),
#             freq_guid = sum(guid == "Guidée", na.rm=TRUE) / sum(!is.na(guid))) %>% 
#   right_join(ar, by = "art_id")
# 
# al <- group_by(st, alb_id) %>% 
#   summarise(n_listen = n(),
#             n_users = n_distinct(user_id),
#             freq_guid = sum(guid == "Guidée", na.rm=TRUE) / sum(!is.na(guid)))


save(fs, fal, far, file = "data/favorites.RData")
save(us, file = "data/french_users.RData")
save(so, 
     #ar, al, 
     file = "data/songs_artists.RData")
save(st, file = "data/streams.RData")
