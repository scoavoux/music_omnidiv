###### Packages ######
library(tidyverse)
library(lubridate)

###### Data ######
s <- read_tsv("data/orig/stream.tsv", col_names = FALSE)

# remove rows that are badly imported (around 300 => we don't care)
n <- unique(problems(s)$row)
s <- s[-n, ]
rm(n)

str(s)
head(s)

s <- mutate(s, support = factor(X9, levels = c("desktop", "mobile", "tablet", "web")))

save(s, file = "data/streams.RData")


u <- read_tsv("data/orig/orange_user_detail.tsv", col_names = c("user_hash", "date_birth", "gender", "date_registered", "city"), 
              col_types = "ccccc")
# TODO: enlever les valeurs aberrantes (grands nombre de streams; cf. code Sisley)

## Mettre les dates dans le bon format ; calculer l'âge
u <- mutate_at(u, .cols = vars(starts_with("date_")), .funs = funs(ifelse(. == "null", NA, .) %>% ymd())) %>% 
  ## supprimer les plus jeunes (ages aberrants)
  mutate(age = 2013 - year(date_birth),
         age = ifelse(age < 10, NA, age)) %>% 
  ## Corriger les types de vecteur
  mutate(gender = factor(gender))

u

# save
save(u, file = "data/french_users.RData")


