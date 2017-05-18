library(tidyverse)
load("data/genres_raw.RData")

genres <- bind_rows(l) %>% tbl_df()
## ATTENTION: filtrage des NA est temporaire ; on en aura besoin par la suite pour 
## les autres informations que les genres présents dans la base
## => PB: certaines des commandes ci-dessous les suppriment 
## (especially les !(name %in%), !(name ==))
## ajouter un "| is.na(name)" à chaque ligne?
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

genres_order <- c("Jeunesse", "Comédies musicales", "Chanson française", "Metal", 
                  "Soul", "Jazz", "Blues", "Country & Folk", "World music", "R&B",
                    "Reggae", "Classique", "Rap/Hip Hop", "Dance", "Electro", "Alternative", 
                  "Rock", "Pop", "Films/Jeux vidéo", "Variété Internationale")

genres <- mutate(genres, name = factor(name, levels = genres_order)) %>% 
  group_by(alb_id) %>% 
  arrange(name) %>% 
  slice(1) %>% 
  # reste 4 variété internationale qui traînent... On les agrége à pop
  mutate(name = ifelse(name == "Variété Internationale", "Pop", name) %>% 
           factor(levels = genres_order[-length(genres_order)])) 

save(genres, file = "data/genres.RData")

###### HELPERS ######

## Diverses manières d'explorer les données en vue du recodage

## Fonction pour tester l'avancée du recodage.
## Objectif: 100% des albumes avec un seul genre
test <- function(genres){
  x <- count(genres, alb_id) %>% 
    filter(n == 1L)
  results <- c("Nombre d'albums différents" = length(unique(genres$alb_id)),
               "Nb d'albums à un genre" = nrow(x),
               "Proportion d'albums à un genre" = nrow(x)/length(unique(genres$alb_id))*100,
               "Nombre de genres uniques" = length(unique(genres$name)),
               "Nombre de NA" = sum(is.na(genres$name)))
  return(round(results))

}

## Fonctions pour visualiser les artistes
## à cheval sur plusieurs genres
load("data/songs_artists.RData")
so <- distinct(so, alb_id, .keep_all = TRUE) %>% left_join(ar, by = "art_id") %>% select(alb_id, digital_release, art_name)

pr <- function(na, not = NULL, gnr = genres){
  x <- group_by(gnr, alb_id) %>% 
    filter(any(name == na)) %>% 
    ungroup() %>% 
    left_join(select(so, alb_id, art_name))
  
  if(!is.null(not)){
    x <- group_by(x, alb_id) %>% 
      filter(!(any(name == not))) %>% 
      ungroup()
  }
  
}

pr2 <- function(na, not = NULL, gnr = genres){
  x <- group_by(gnr, alb_id) %>% 
    filter(any(name == na[1]) & any(name == na[2])) %>% 
    ungroup() %>% 
    left_join(select(so, alb_id, art_name)) %>% 
    distinct(art_name) %>% arrange(art_name)
  
  if(!is.null(not)){
    x <- group_by(x, alb_id) %>% 
      filter(!(any(name == not))) %>% 
      ungroup()
  }
  print(x, n=Inf)
  
}


test(genres)
library(questionr)
freq(genres$name, sort = "dec")

genres <- group_by(genres, alb_id) %>% mutate(ngnr = n()) %>% ungroup()

## From rmarkdown
df <- genres %>% 
  select(alb_id, name) %>% 
  filter(!is.na(name)) %>% 
  mutate(x=1) %>% 
  spread(name, x, fill = 0)

library(FactoMineR)
gpca <- PCA(select(df, -alb_id), graph=FALSE)
dimdesc(gpca)
plot(gpca)
hc <- HCPC(gpca, kk = 1000, graph=FALSE)
library(ClustOfVar)

df <- select(df, -alb_id) %>% mutate_all(as.numeric)
dd <- as.matrix(df)
hc <- hclustvar(X.quanti = dd)

library("amap")
dt <- select(df, -alb_id) %>% burt()
db <- as.data.frame(dt)
db <- select(db, ends_with("0")) %>% 
  rownames_to_column() %>% 
  filter(grepl("0$", rowname))
db$rowname <- sub("(.*)\\.0", "\\1", db$rowname)
names(db) <- sub("(.*)\\.0", "\\1", names(db))

db2 <- gather(db, key, value, -rowname) %>% 
  mutate_if(.predicate = is.character, .funs = funs(sub("(.*)\\.0$", "\\1", .))) %>% 
  mutate(value = as.integer(value)) %>% 
  group_by(key) %>%
  filter(!(rowname == key)) %>% 
  group_by(key) %>% 
  arrange(desc(value)) %>% 
  slice(1:4) %>% 
  select(name = key, rowname, value) %>% 
  group_by(name) %>% slice(1)

group_by(db2, key) %>% slice(1)

gt <- group_by(genres, name) %>% 
  summarize(n = n(),
            unique = any(ngnr == 1)) %>% 
  # mutate(f = n / sum(n) * 100) %>% 
  arrange(n) %>% 
  left_join(db2, by = "name")


print(gt, n= 50)

summary(genres$ngnr)

filter(genres, (ngnr > 1 & name %in% gt$name[gt$f < 0.5]))

filter(genres, !(ngnr > 1 & name %in% gt$name[gt$f < 0.1] & name %in% gt$name[!gt$unique])) %>% test()


