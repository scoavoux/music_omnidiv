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

## Jeu de données Aires urbaines/taille ville
### https://www.insee.fr/fr/information/2115011

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
r <- select(r, CODGEO, revenu_median = Q213)

## Importer les données AUR
s <- read_xls("data/INSEE_AUR/AU2010 au 01-01-2017.xls", sheet = 2, skip = 5)
s_dic <- c(`111` = "Commune appartenant à un grand pôle (10 000 emplois ou plus)",
           `112` = "Commune appartenant à la couronne d'un grand pôle",
           `120` = "Commune multipolarisée des grandes aires urbaines",
           `211` = "Commune appartenant à un moyen pôle (5 000 à moins de 10 000 emplois)",
           `212` = "Commune appartenant à la couronne d'un moyen pôle",
           `221` = "Commune appartenant à un petit pôle (de 1 500 à moins de 5 000 emplois)",
           `222` = "Commune appartenant à la couronne d'un petit pôle",
           `300` = "Autre commune multipolarisée",
           `400` = "Commune isolée hors influence des pôles")
s <- select(s, CODGEO, AUR = CATAEU2010) %>% 
  mutate(AUR = s_dic[AUR] %>% factor(levels = s_dic))

cities <- left_join(cities, r, by = c("codeInsee" = "CODGEO")) %>%
  left_join(s, by = c("codeInsee" = "CODGEO"))

save(cities, file = "data/cities.RData")
rm(dico, r, s)
