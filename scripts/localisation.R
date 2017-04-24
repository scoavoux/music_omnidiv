## Geoloc;
# objectif: 
# + saisir zone urbaine + revenu médian de la zone/commune
# pour chaque personne
library("tidyverse")
library("stringr")
library("rgeoapi")

## On repart des usagers. Clean-up nom de ville
## TODO: améliorer pour augmenter le nombre de match
u <- mutate(u, city = tolower(city) %>% chartr("éèêëàâäîïùûüôöç ", 
                                               "eeeeaaaiiuuuooc-", 
                                               .) %>% str_trim())

p <- data_frame(cities = unique(u$city[!is.na(u$city)]))

## Interroger geo api pour le code INSEE des communes
l <- vector(mode = "list", length = nrow(p))

for(i in seq_along(p$cities)){
  l[[i]] <- tryCatch(ComByName(p$cities[i])[1, ], error = function(e){})
  if(length(l[[i]]) > 0) l[[i]]$origname <- p$cities[i]
}

a <- bind_rows(l)
save(a, file = "data/cities.RData")

## Jeu de données socio-demo sur les communes
### Pour le revenu:
### http://www.insee.fr/fr/statistiques/2388413



