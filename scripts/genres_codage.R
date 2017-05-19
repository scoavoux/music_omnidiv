# author: Samuel Coavoux
# date: mai 2017
# Ce script prend en entrée les genres au format brut, 
# tels que produit par le script genre_scraping.R, 
# et ressort une base donnant un genre unique par album
# (avec 8% de NA environ).
# 
# Contient l'ensemble de la démarche suivie pour le codage
# des genres. On est parti de 91 genres différents pour arriver
# à 20 genres unique, en éliminant les plus petits ou les moins
# musicaux, en agrégeant jusqu'à n'avoir que des genres faisant
# sens, puis en fixant une liste de priorité pour que le genre
# unique soit le plus raffiné possible.

library(tidyverse)
load("data/genres_raw.RData")

genres <- bind_rows(l) %>% tbl_df()
rm(i, l)
genres <- select(genres, -picture, -type, -id) %>% 
  group_by(alb_id) %>% 
  mutate(ngnr = n()) %>% 
  ungroup()

###### ÉLIMINATION DE GENRES ######

## On supprime tous les genres pour lesquels
## l'intégralité des albums est aussi présent
## dans le même autre genre plus gros (=genre sous-ensemble)
## Par exemple, tous les albums "West Coast" sont dans
## "Rap/Hip-Hop", donc on supprime West Coast
genres <- filter(genres, !(name %in% c("Enfants & famille", "Pop Culture, TV & films", "West Coast", "Delta blues",
                                       "Histoires", "Country blues", "Dancefloor", "Renaissance", "Old School",
                                       "Musiques de jeux vidéo", "Blues acoustique", "Période classique",
                                       "Médieval", "Romantique", "Tropical", "Electro Hip-Hop", "Moderne",
                                       "Comptines/Chansons", "Electro Pop/Electro Rock", "Jazz vocal", "Blues électrique", 
                                       "Chicago blues", "Baroque", "Dirty South", "Ska", "Bollywood", "Dubstep",
                                       "East Coast", "Opéra", "Bandes originales", "Dub", "Trance", "Chill Out/Trip-Hop/Lounge", 
                                       "Hard Rock", "Pop Indé", "Techno/House")))

## Idem mais plus contestable
genres <- filter(genres, !(name %in% c("Jazz Hip Hop",
                                       ## Jazz instrumental: tous sauf un associé à jazz ; 
                                       ## le dernier associé à RnB => On peut supprimer la catégorie
                                       "Jazz instrumental",
                                       ## R&B contemporain: vient habituellement qualifier un autre genre 
                                       ## (le plus souvent R&B, parfois soul, rapi, hip hop...)
                                       ## Jamais présent tout seul => supprimer
                                       "R&B contemporain",
                                       ## Survit la plupart du temps ; catégorie très hétérogène
                                       "Singer & Songwriter",
                                       ## Catégorie très hétérogène
                                       "Spiritualité & religion",
                                       ## Idem
                                       "Rock & Roll/Rockabilly",
                                       ## Codés ailleurs, très petite catégorie
                                       "Pop française",
                                       ## codé Rap/Rock par ailleurs
                                       "Rock français",
                                       "Rap français")))

## TODO: récupérer le "français" de Rap français / rock français


## Supprimer les genres HS
genres$name[genres$name == "Informations & politique"] <- NA


genres <- mutate(genres
                 ## Agréger les différents sous-genres de musiques de films
                 , name = ifelse(name %in% c("BO TV", "Musiques de films", "Comédie"), 
                                       "Films/Jeux vidéo",
                                       name)
                 ## Agréger Raggae dans reggae
                 , name = ifelse(name %in% c("Dancehall/Ragga"), 
                                 "Reggae",
                                 name)
                 , name = ifelse(name %in% c("Disco"), 
                                 "Dance",
                                 name)
                 ## Agréger trois genres de musique mexicaine + 
                 ## 1 genre de musique cubaine dans latino
                 , name = ifelse(name %in% c("Bolero", "Corridos", "Norteño",  "Ranchera"), 
                                 "Latino",
                                 name)
                 ## Créer un genre "World" pour les genres continent/culture (sauf, pour le moment, latino et brésilien...)
                 , name = ifelse(name %in% c("Musique brésilienne", "Latino", "Musique africaine", "Musique asiatique", "Musique arabe", "Musique indienne"),
                                 "World music",
                                 name)
                 
                 )

## TODO: Créer une variable sous_genre pour raffiner le genre
## Pour le genre world, sous-genre == latino, brésilien, africain, asiatique, arabe, indien

## Musiques de "sports" (workout): souvent associé à electro;
## dans ce cas, supprimer sports
## TODO: Sports à suprimer définitivement.
genres <- filter(genres, !(name == "Sports" & ngnr > 1)) %>% 
  ## NA pour les autres sports
  mutate(name = ifelse(name == "Sports", NA, name))

## Grime: pas toujours co-présent avec Hip-hop, alors que dans la plupart des cas,
## les artistes sont définis (sur Wikipedia) comme faisant du hiphop
## (exemple: D12 est électro et grime, alors qu'il inclut Eminem)
## => remplacer toutes les occurences de grime par Rap/Hip Hop
genres <- mutate(genres, name = ifelse(name == "Grime", "Rap/Hip Hop", name))

## Tous les "indé" transformés en "Alternative"
genres <- mutate(genres, name = ifelse(name %in% c("Rock indé", "Pop indé/Folk", "Rock Indé/Pop Rock"), "Alternative", name))

## pop internationale en variété internationale
genres <- mutate(genres, name = ifelse(name == "Pop internationale", "Variété Internationale", name))

## Toutes les ...soul en soul
genres <- mutate(genres, name = ifelse(name %in% c("Soul vieille école", "Soul contemporaine", "Soul & Funk", "R&B vieille école"), "Soul", name))

## Agréger country et folk
genres <- mutate(genres, name = ifelse(name %in% c("Folk", "Country"), "Country & Folk", name))

## Supprimer les doublons produits par le processus de recodage
genres <- distinct(genres, alb_id, name, .keep_all = TRUE)

###### AGRÉGATION DE GENRES ######

# On définit les genres dans l'ordre suivant, correspondant
# à une tentative de raffinement (on part des genres les plus spécifiques
# et on va vers les plus généraux). On perd de l'information, 
# mais on passe à un genre par album/chanson.

## NB: on supprime le genre Variété internationale.
genres_order <- c("Jeunesse", "Comédies musicales", "Chanson française", "Metal", 
                  "Soul", "Jazz", "Blues", "Country & Folk", "World music", "R&B",
                    "Reggae", "Classique", "Rap/Hip Hop", "Dance", "Electro", "Alternative", 
                  "Rock", "Pop", "Films/Jeux vidéo")

genres <- mutate(genres, name = factor(name, levels = genres_order)) %>% 
  group_by(alb_id) %>% 
  arrange(name) %>% 
  slice(1) %>% 
  select(-ngnr)

rm(genres_order)

save(genres, file = "data/genres.RData")
