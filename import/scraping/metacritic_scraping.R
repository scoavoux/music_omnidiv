library(tidyverse)
library(rvest)

# base_urls <- data.frame(base_url = c("http://www.metacritic.com/browse/albums/artist?view=condensed",
#                                      paste0("http://www.metacritic.com/browse/albums/artist/", letters, "?view=condensed")),
#                         stringsAsFactors = FALSE)
# 
# ## Generate full urls based on number of pages for each letter
# 
# get_page_number <- function(url){
#   p <- read_html(url) %>% 
#     html_node(css = "li.last_page a.page_num") %>% 
#     html_text()
#   p
# }
# 
# base_urls$page_number <- sapply(base_urls$base_url, get_page_number)
# base_urls$page_number <- as.numeric(base_urls$page_number)
# 
# save(base_urls, file = "data/metacritic.RData")

# albums_urls <- c(base_urls$base_url,
#                  ## Ouh que c'est moche
#                  sapply(1:nrow(base_urls), function(i){
#                    if(!is.na(base_urls$page_number[i])){
#                      paste0(base_urls$base_url[i], "&page=", 1:(base_urls$page_number[i]-1))
#                    }
#                  }) %>% unlist()
#                  )
# 
# get_albums_urls <- function(page){
#   read_html(page) %>% 
#     html_nodes(css="li.product.release_product div.product_wrap div.basic_stat.product_title a") %>% 
#     html_attr("href")
#     
# }
# 
# l <- vector("list", length(albums_urls))
# for(i in 1:length(l)){
#   message(paste("Scraping page", i))
#   l[[i]] <- get_albums_urls(albums_urls[i])
#   Sys.sleep(2)
# }
# 
# mc <- data.frame(url = unlist(l), 
#                  stringsAsFactors =FALSE)
# 
# save(base_urls, mc, i, l, file = "data/metacritic_raw.RData")

# mc$url <- paste0("http://www.metacritic.com", mc$url, "/critic-reviews")
load("data/metacritic_raw.RData")
get_album_infos <- function(url){
  page <- read_html(url)
  album <- html_node(page, css = "h1.product_title span") %>% html_text()
  artist <- html_node(page, css = "div.product_artist a span.band_name") %>% html_text()
  average_note <- html_node(page, css = "div.metascore_w.large span") %>% html_text()
  review_count <- html_node(page, css = "div.summary p span.count") %>% 
    html_text()
  if(!is.na(average_note)){
    source <- html_nodes(page, css = "div.review_critic div.source") %>% html_text()
    score <- html_nodes(page, css = "div.review_grade div.metascore_w:not(.user)") %>% html_text()
  } else {
    source <- score <- NA
  }
  
  r <- data.frame(album = album, 
                  artist = artist, 
                  average_note = average_note, 
                  review_count = review_count, 
                  source = source,
                  score = score,
                  stringsAsFactors = FALSE)
  return(r)
}

# l <- vector("list", length = nrow(mc))
# for(i in 14502:nrow(mc)){
#   message(paste0("Scraping album #", i))
#   l[[i]] <- tryCatch(get_album_infos(mc$url[i]),
#                      error = function(e) e)
#   if(!i %% 50) message(paste0(i, "/", nrow(mc), " ", round(i/nrow(mc)*100, 2), "% complete"))
# }
# 
# save(base_urls, mc, l, i, file = "data/metacritic.RData")

l[(sapply(l, class) %>% sapply(length) == 3)] <- NULL
library(stringr)
mc_rev <- bind_rows(l) %>% 
  filter(!is.na(average_note)) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate(review_count = sub(".*?(\\d+) Critics?$", "\\1", review_count) %>% as.numeric(),
         score = as.numeric(score),
         average_note = as.numeric(average_note)) %>% 
  distinct() %>% 
  group_by(album, artist) %>% 
  mutate(review_count_scrapped = n(),
         average_note_scrapped = mean(score)) %>% 
  ungroup()

# En comparant review_count_scrapped et review_count, 
# on constate que quelques albums (65) ont un écart 
# entre les deux, qui correspond soit à une erreur, 
# soit à ce que certaines critiques n'ont pas été récupérée

# On note également un écart considérable entre la note
# moyenne calculée par metacritic et la note moyenne que l'on
# retrouve en la calculant nous même. la note de metacritic
# est plus élevée de 2.3 points en moyenne

# summary(mc_rev$average_note_scrapped - mc_rev$average_note)

mc_alb <- distinct(mc_rev, artist, album, average_note, review_count)
save(mc_rev, mc_alb, file = "data/metacritic.RData")
