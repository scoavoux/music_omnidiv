# Get album genres from Deezer API
# Starting from unique albums as defined in the song dataset

library(jsonlite)
library(tidyverse)
library(here)


load(here("data", "metacritic.RData"))
unique(paste0(mc_alb$album, mc_alb$artist))
urls <- data.frame(
  url = paste0('https://api.deezer.com/search?q=artist:%22',
       mc_alb$artist,
       '%22 album:%22',
       mc_alb$album,
       '%22'), 
  stringsAsFactors = FALSE)

urls$url <- gsub("\\s", "%20", urls$url)

i <- 1245
library(rvest)

read_html(urls$url[i])

for(i in 1:nrow(urls)){
  df <- fromJSON(urls$url[i])
  if(is.null(df$error)){
    alb_id <- urls$id[i]
    release_date <- df$release_date

    l[[i]] <- data_frame(alb_id = alb_id,
                         release_date = release_date)

  }
  ## API request limit: 50 requests every 5 seconds
  Sys.sleep(.1)
  ## Save intermediary results in case the computer fails
  ## during scraping (every 1000 operations)
  if(i %% 1000 == 0){
    save(l, i, file = "data/dates_raw.RData")
    print(paste(i, "saved"))
  } else {
    print(i)
  }

}

save(l, i, file = "data/dates_raw.RData")
