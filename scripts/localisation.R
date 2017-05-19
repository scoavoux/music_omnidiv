## Geoloc;
# objectif: 
# + saisir zone urbaine + revenu médian de la zone/commune
# pour chaque personne
library("tidyverse")
library("stringr")
library("readxl")

# source("scripts/localisation_scraping.R")
load("data/cities_coordinates.RData")

## Jeu de données socio-demo sur les communes
### Pour le revenu:
### http://www.insee.fr/fr/statistiques/2388413
### Téléchargé dans data/INSEE_revenu_communes

###### Prepare INSEE data ######
## Récupérer les codes des variables
dico <- lapply(list.files("data/INSEE_revenu_communes", full.names = TRUE)[1:2],
            function(f){
              read_xls(f, sheet = 2, skip = 4, n_max = 2, col_names = FALSE) %>% 
                t() %>% 
                as_data_frame() %>% 
                mutate(file = sub("data/INSEE_revenu_communes/(.*)", "\\1", f))
            })
dico <- bind_rows(dico) %>% rename(label = V1, code = V2)

## récupérer les données
r <- lapply(list.files("data/INSEE_revenu_communes", full.names = TRUE)[1:2],
            function(f){
              read_xls(f, sheet = 2, skip = 5)
            })

r <- full_join(r[[1]], r[[2]], by = c("CODGEO", "LIBGEO"))

cities <- left_join(cities, r, by = c("codeInsee" = "CODGEO")) %>% 
  rename(revenu_median = Q213)

save(cities, file = "data/cities.RData")
rm(dico, r)
