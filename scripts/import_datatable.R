###### Packages ######
library(data.table)

###### Songs ######

so <- fread(input = "data/orig/song_catalog.tsv", 
            sep = "\t", 
            col.names = c("sng_id", "art_id", "alb_id", "label_id", 
                             "digital_release", "physical_release", "sng_title", "duration"))


## Corriger les chansons dont la durée est abérrante (> 2h)
so[, duration := ifelse(duration > 3600*2, NA, duration)]

## Corriger les dates de sortie
so[digital_release == "null", digital_release := NA]
so[physical_release == "null", physical_release := NA]
so[, physical_release := as.IDate(physical_release)]
so[, digital_release := as.IDate(digital_release)]
so[, nouveaute := year(digital_release) == 2014]


###### Artists ######

ar <- fread("data/orig/artist_catalog.tsv",
            sep = "\t",
            col.names = c("art_id", "art_name", "rank_artist", "artist_fans"))


###### Streams ######
st <- fread("data/orig/stream.tsv",
            sep = "\t",
            col.names = c("user_id", "sng_id", "type_stream", "country", "length", "context_name", "context_id", 
                          "app_id", "app_type", "offer_id", "timestamp_off", "timestamp_sync", "pause", "seek", "timestamp"))


## Note: Code de Sisley ne documente pas comment elle a fait.
## Simplement, elle supprime certaines personnes par leur index
## cf. Database_create.do, l. 26-53
## Calculer nombre de tracks et temps cumulé d'écoute 
u <- st[, .(n = .N, t = sum(length) / (60*60*24)), user_id]

# ggplot(u, aes(x = n)) + geom_histogram() + scale_x_log10()
# ggplot(u, aes(x = t)) + geom_histogram() + scale_x_log10()

## Supprimer sur la base ci-dessus (plus de 40000 tracks ou plus de 100h d'écoutes cumulées)
st <- anti_join(st, filter(u, n > 4e04 | t > 1e02), by = "user_id")
rm(u)

## Enlever les écoutes avant le 1er avril 2014
st <- filter(st, timestamp >= 1396310400 | is.na(timestamp))

# Mettre les données au bon format
st <- mutate(st, type_stream = factor(type_stream, 
                                      levels = 0:2, 
                                      labels = c("MOD", "smartradio", "radio")),
             timestamp = as_datetime(as.numeric(timestamp), tz="Europe/Paris"), 
             # Déjà recodé, on a pas les données brutes annoncées dans le dictionnaire des variables
             app_type = factor(app_type,
                               levels = c("desktop", "mobile", "tablet", "web")),
             offer_id = ifelse(offer_id > 2, 3, offer_id) %>% 
               factor(levels = 0:3,
                      labels = c("Free", "Premium", "Premium+", "Partenaire")))

## Temporalité de l'écoute
st <- mutate(st, 
             week = week(timestamp),
             wday = lubridate::wday(timestamp, label = TRUE),
             yday = yday(timestamp),
             hour = hour(timestamp))

st <- st %>%  mutate(activite = case_when(.$type_stream != "MOD" ~ "Passive",
                                          .$type_stream == "MOD" & .$context_name %in%
                                            c("radio_page", "feed_user_radio" , "collection_radio" , 
                                              "profile_radios", "artist_smartradio", "smartradio_page", 
                                              "feed_smartradio", "playlist_radio", "notification_genreradio") ~ "Passive",
                                          .$context_name != "unknown" & !is.na(.$context_name) ~ "Active",
                                          TRUE ~ NA_character_) %>% factor())


## Temps d'écoute

## Vérifier que longueur moindre que longueur de la piste
## + ajouter année de diffusion
st <- select(so, sng_id, art_id, duration) %>% 
  right_join(st, by = "sng_id") %>% 
  mutate(length = ifelse(length > duration, duration, length))

# 5% de durée d'écoute négative... NA?
# sum(st$length < 0) / nrow(st)
st$length[st$length < 0] <- NA

## Popularité des titres et artistes
st <- group_by(st, sng_id) %>% 
  mutate(nb_ecoutes_sng = n()) %>% 
  group_by(art_id) %>% 
  mutate(nb_ecoutes_art = n()) %>% 
  ungroup() %>% 
  mutate(sng_popularite = cut(nb_ecoutes_sng, breaks = c(0, 2, 60, 1e20),  labels = c("Tail", "Mid-tail", "Star")),
         art_popularite = cut(nb_ecoutes_art, breaks = c(0, 2, 227, 1e20), labels = c("Tail", "Mid-tail", "Star")))

## Nouveauté

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



st <- st %>% mutate(context_name2 = ifelse(fav_artist == "Favorite" | fav_song == "Favorite",
                                           "favorite",
                                           context_name),
                    context_cat = ifelse(context_name2 %in% c("tops_album", "tops_playlist", "tops_track"), "top", NA),
                    context_cat = ifelse(context_name2 %in% c("ticker_album" , "ticker_playlist" , "ticker_track" , "recommendations_friend_share_album" , "facebook_track"), "social", context_cat),
                    context_cat = ifelse(context_name2 %in% c("radio_page"), "radio_editoriale", context_cat),
                    context_cat = ifelse(context_name2 %in% c("artist_smartradio" , "smartradio_page"), "smartradio", context_cat),
                    context_cat = ifelse(context_name2 %in% c("profile_user_radio", "playlist_radio"), "radio_flow", context_cat),
                    context_cat = ifelse(context_name2 %in% c("feed_user_radio"), "feed_radio", context_cat),
                    context_cat = ifelse(context_name2 %in% c("feed_smartradio"), "feed_smartradio", context_cat),
                    context_cat = ifelse(context_name2 %in% c("feed_album"), "feed_album", context_cat),
                    context_cat = ifelse(context_name2 %in% c("feed_playlist"), "feed_playlist", context_cat),
                    context_cat = ifelse(context_name2 %in% c("feed_track" , "suggest_track" , "notification_track"), "feed_track", context_cat),
                    context_cat = ifelse(context_name2 %in% c("artist_discography"), "artist_disco", context_cat),
                    context_cat = ifelse(context_name2 %in% c("artist_top"), "artist_top", context_cat),
                    context_cat = ifelse(context_name2 %in% c("explore_releases_album"), "explore_release", context_cat),
                    context_cat = ifelse(context_name2 %in% c("folder_page", "favorite", "history_page", "inapp_page", "loved_page", "collection_album", "collection_radio", "collection_playlist", "profile_albums", "profile_history", "profile_top", "profile_top_albums", "profile_top_tracks", "profile_playlists", "personnalsong_page", "profile_radios"), "stock", context_cat) ,
                    context_cat = ifelse(context_name2 %in% c("search_page"), "search", context_cat),
                    context_cat = ifelse(context_name2 %in% c("album_page", "track_page", "playlist_page"), "ND", context_cat),
                    context_cat = ifelse(context_name2 %in% c("unknown"), "unknown", context_cat),
                    context_cat = ifelse(context_name2 %in% c("player_default_playlist"), "player_defaut", context_cat),
                    context_cat = ifelse(context_name2 %in% c("selection_album", "explore_picks_album", "explore_region_album"), "experts_editor", context_cat))

# context_cat= ifelse(context_cat   %in% c("feed_smartradio", "feed_radio", "feed_album", "feed_playlist", "feed_track"), "perso", context_cat)
# context_cat= ifelse(context_cat   %in% c("radio_editoriale", "smartradio", "radio_flow"), "radios", context_cat)
# context_cat= ifelse(context_cat   %in% c("artist_disco", "artist_top"), "contextuel", context_cat)

###### Fav albums ######
fal <- read_tsv("data/orig/fav_albums.tsv",
                col_names = c("user_id", "alb_id", "add", "timestamp_favalbum"))

###### Fav albums ######

alf <- read_tsv("data/orig/album_fav.tsv", 
                col_names = c("alb_id", "art_id", "label_id", "digital_release",
                              "physical_release", "alb_title", "rank", "nb_fans"))


###### Clean-up  ######
## Remove some unused variable so that knitting becomes possible
st <- select(st, -duration, -type_stream, -country, -context_name, -context_name2, -context_id, -app_id, -timestamp_off, -timestamp_sync, -pause, -seek)
gc()

save(fs, fal, far, alf, file = "data/favorites.RData")
save(us, file = "data/french_users.RData")
save(so, ar, file = "data/songs_artists.RData")
save(st, file = "data/streams.RData")
