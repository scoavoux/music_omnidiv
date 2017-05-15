# Get album genres from Deezer API
# Starting from unique albums as defined in the song dataset

library(jsonlite)
library(tidyverse)

so <- read_tsv("data/orig/song_catalog.tsv",
               col_names = c("sng_id", "art_id", "alb_id", "label_id",
                             "digital_release", "physical_release", "sng_title", "duration"))

urls <- data_frame(id = unique(so$alb_id),
                   url = paste0("http://api.deezer.com/album/", id))

## At start, create l ; then, load it from data/genres.RData
# l <- vector("list", length = nrow(urls))
# load("data/genres_raw.RData")

for(i in 1:nrow(urls)){
  df <- fromJSON(urls$url[i])
  if(is.null(df$error)){
    if(df$genre_id != -1){
      genre <- fromJSON(urls$url[i])$genres$data
    } else {
      genre <- data_frame(id = NA, name = NA, picture = NA, type = NA)
    }
    alb_id = urls$id[i]
    fans = df$fans
    alb_type = as.character(df$type)
    if(!is_empty(df$tracks$data)) 
      nb_tracks = nrow(df$tracks$data)
    else
      nb_tracks = NA
    
    l[[i]] <- mutate(genre, alb_id = alb_id,
                     nb_fans = fans,
                     alb_type = alb_type,
                     nb_tracks = nb_tracks                     )
    
  }
  ## API request limit: 50 requests every 5 seconds
  Sys.sleep(.12)
  ## Save intermediary results in case the computer fails
  ## during scraping (every 1000 operations)
  if(i %% 1000 == 0){
    save(l, i, file = "data/genres_raw.RData")
    print(paste(i, "saved"))
  } else {
    print(i)
  }

}

save(l, i, file = "data/genres_raw.RData")
