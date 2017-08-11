# Get album genres from Deezer API
# Starting from unique albums as defined in the song dataset

library(jsonlite)
library(tidyverse)

so <- read_tsv("data/orig/song_catalog.tsv",
               col_names = c("sng_id", "art_id", "alb_id", "label_id",
                             "digital_release", "physical_release", "sng_title", "duration"))

sos <- filter(so, digital_release == "null")

urls <- data_frame(id = unique(sos$alb_id),
                   url = paste0("http://api.deezer.com/album/", id))

## At start, create l ; then, load it from data/genres.RData
# l <- vector("list", length = nrow(urls))
# load("data/dates_raw.RData")

# for(i in 1:nrow(urls)){
#   df <- fromJSON(urls$url[i])
#   if(is.null(df$error)){
#     alb_id <- urls$id[i]
#     release_date <- df$release_date
# 
#     l[[i]] <- data_frame(alb_id = alb_id,
#                          release_date = release_date)
#     
#   }
#   ## API request limit: 50 requests every 5 seconds
#   Sys.sleep(.1)
#   ## Save intermediary results in case the computer fails
#   ## during scraping (every 1000 operations)
#   if(i %% 1000 == 0){
#     save(l, i, file = "data/dates_raw.RData")
#     print(paste(i, "saved"))
#   } else {
#     print(i)
#   }
#   
# }
# 
# save(l, i, file = "data/dates_raw.RData")


