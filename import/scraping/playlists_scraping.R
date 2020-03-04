# 04 Décembre 2019
# Récupérer infos sur les playlists écoutées
# executé le 6 décembre 2019

library(jsonlite)
library(tidyverse)
library(lubridate)

st <- read_tsv("data/orig/stream.tsv", 
               col_names = c("user_id", "sng_id", "type_stream", "country", "length", "context_name", "context_id", 
                             "app_id", "app_type", "offer_id", "timestamp_off", "timestamp_sync", "pause", "seek", "timestamp"))

st <- mutate(st, timestamp = as_datetime(as.numeric(timestamp), tz="Europe/Paris")) %>% 
  ## Enlever les écoutes avant le 1er avril 2014
  filter(year(timestamp) == 2014 & month(timestamp) %in% 4L:8L)

st <- filter(st, context_name == "playlist_page")


urls <- data_frame(id = unique(st$context_id),
                   url = paste0("http://api.deezer.com/playlist/", id))

## At start, create l ; then, load it from data/genres.RData
# l <- vector("list", length = nrow(urls))
# load("data/playlists_raw.RData")

for(i in i:nrow(urls)){
  l[[i]] <- fromJSON(urls$url[i])
  ## API request limit: 50 requests every 5 seconds
  Sys.sleep(.11)
  ## Save intermediary results in case the computer fails
  ## during scraping (every 1000 operations)
  if(i %% 1000 == 0){
    save(l, i, file = "data/playlists_raw.RData")
    print(paste(i, "saved"))
  } else {
    print(i)
  }
}

save(l, i, urls, file = "data/playlists_raw.RData")
