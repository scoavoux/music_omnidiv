# Ce script part des données scrappés le 6 décembre 2019
# pour produire une base propre des playlists
# et une des tracklists de ces playlists

library(tidyverse)
library(purrr)
library(lubridate)
library(here)

load("data/playlists_raw.RData")

## On commence par fabriquer un tibble des playlists que l'on a tenté de scraper
## en précisant si erreur ou réussite.
pl <- data_frame(id   = as.integer(urls$id),
                 extract = factor(sapply(l, length), levels = c(1, 22), labels = c("Error", "Playlist")),
                 error_code = sapply(l, function(x) x$error$code) %>% as.character()) %>% 
  mutate(row = row_number())

pl <- mutate(pl, error_code = factor(error_code, 
                                     levels = c("200", "800"),
                                     labels = c("200 -- authentification",
                                                "800 -- missing data")))

## On extrait les données pertinentes par playlist
extract_data <- function(x){
  if(is.null(x$error)){
    r <-   data_frame(id = x$id,
                      title = x$title,
                      description = x$description,
                      duration = x$duration,
                      public = x$public,
                      is_loved_track = x$is_loved_track,
                      collaborative = x$collaborative,
                      nb_tracks = x$nb_tracks,
                      fans = x$fans,
                      link = x$link,
                      checksum = x$checksum,
                      creation_date = x$creation_date,
                      creator_id = x$creator$id,
                      creator_name = x$creator$name,
                      creator_type = x$creator$type,
                      creator_tracklist = x$creator$tracklist,
                      type = x$type)
    return(r) 
  }
}

## Fabriquer pl

pl <- map(l, extract_data) %>% 
  bind_rows(x) %>% 
  right_join(pl, "id") %>% 
  distinct(id, .keep_all = TRUE)

## Extraire les tracklists

extract_tracklist <- function(x){
  if(is.null(x$error) & length(x$tracks$data) > 0){
    tracks  <- as_tibble(x$tracks$data) %>% select(-artist, -album)
    albums  <- as_tibble(x$tracks$data[["album"]]) %>% 
      rename_all(~paste0("album_", .))
    artists <- as_tibble(x$tracks$data[["artist"]]) %>% 
      rename_all(~paste0("artist_", .))
    r <- bind_cols(tracks, artists, albums)
    return(r) 
  }
}

pl_tr <- map(l, extract_tracklist)
pl_tr <- bind_rows(pl_tr)
pl_tr <- select(pl_tr, -link, -starts_with("explicit"), -artist_link, -artist_tracklist,
                -starts_with("album_cover"), -album_tracklist)

save(pl, file = here("data", "playlists.RData"))
save(pl_tr, file = here("data", "playlists_tracklist.RData"))
