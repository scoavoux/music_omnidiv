###### Packages ######
library(tidyverse)
library(lubridate)

###### Streams ######
st <- read_tsv("data/orig/stream.tsv", col_names = FALSE)

# remove rows that are badly imported (around 300 => we don't care)
n <- unique(problems(st)$row)
st <- st[-n, ]
rm(n)

st <- mutate(st, support = factor(X9, levels = c("desktop", "mobile", "tablet", "web")))

save(st, file = "data/streams.RData")

###### Users ######

us <- read_tsv("data/orig/orange_user_detail.tsv", col_names = c("user_hash", "date_birth", "gender", "date_registered", "city"), 
              col_types = "ccccc")
# TODO: enlever les valeurs aberrantes (grands nombre de streams; cf. code Sisley)

## Mettre les dates dans le bon format ; calculer l'âge
us <- mutate_at(us, .cols = vars(starts_with("date_")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd())) %>% 
  ## supprimer les plus jeunes (ages aberrants)
  mutate(age = 2013 - year(date_birth),
         age = ifelse(age < 10, NA, age)) %>% 
  ## Corriger les types de vecteur
  mutate(gender = factor(gender))

# save
save(us, file = "data/french_users.RData")

###### Songs ######

so <- read_tsv("data/orig/song_catalog.tsv", col_names = FALSE)
print(so, n = 50)

###### Artists ######

ar <- read_tsv("data/orig/artist_catalog.tsv", col_names = FALSE)
