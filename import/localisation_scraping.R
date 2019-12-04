# author: Samuel Coavoux
# date: mai 2017
# Première étape de la géoloc:
#   récupérer le code commune et un ensemble
# de variables sur les villes à partir
# de geoapi
# En entrée, liste de villes; 
# en sortie, cities_coordinates

library("tidyverse")
library("stringr")
library("rgeoapi")

load("data/french_users.RData")
## On repart des usagers. Clean-up nom de ville
## TODO: améliorer pour augmenter le nombre de match
us <- mutate(us, city = tolower(city) %>% chartr("éèêëàâäîïùûüôöç ", 
                                               "eeeeaaaiiuuuooc-", 
                                               .) %>% str_trim())

p <- data_frame(cities = unique(us$city[!is.na(us$city)]))

## Interroger geo api pour le code INSEE des communes
l <- vector(mode = "list", length = nrow(p))

for(i in seq_along(p$cities)){
  l[[i]] <- tryCatch(ComByName(p$cities[i])[1, ], error = function(e){})
  if(length(l[[i]]) > 0) l[[i]]$origname <- p$cities[i]
}

cities <- bind_rows(l)
rm(l)

save(cities, file = "data/cities_coordinates.RData")
