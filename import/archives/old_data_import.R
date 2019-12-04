# author: Samuel Coavoux
# date: april 2017

###### Librairies ######
library(tidyverse)
library(lubridate)

###### Data:Users ######
u <- read_delim("./base/orange/users.csv", 
                delim = "|", 
                col_names = c("USER_HASH", "SEX", "AGE", "COUNTRY", "CITY"),
                col_types = "ccncc")

## uniquement les Français
u <- filter(u, COUNTRY == "FR")

## ENLEVER les valeurs abérrantes


###### Data:Streams ######
s <- read_delim("./base/orange/streams.csv", 
                delim = "|",
                col_names = c("USER_HASH", "SNG_ID", "TIMESTAMP", "LISTENING_TIME", "CONTEXT_TYPE", "CONTEXT_ID", "N_SEEK", "N_PAUSE", "OFFLINE_TIMESTAMP", "COUNTRY", "GEOIP_COUNTRY", "GEOIP_CITY"))


#
s <- filter(s, USER_HASH %in% u$USER_HASH)
# semble pas être la bonne origien.
s <- mutate(s, TIMESTAMP = as_datetime(TIMESTAMP, tz = "Europe/Paris"))

## On a 31098 écoutes de temps négatif... soit 1.8% des écoutes
sum(s$LISTENING_TIME < 0)
## soit -1, soit -34
unique(s$LISTENING_TIME[s$LISTENING_TIME < 0])
## Mais pas de dictionnaire des codes...

## Par ailleurs, 282 écoutes de plus d'une heure
sum(s$LISTENING_TIME > 60*60)
## Dont 34 de plus de dix heures, et jusqu'à une de 1021 heures...
sum(s$LISTENING_TIME > 60*60)
max(s$LISTENING_TIME)/3600


## Supprimer toutes les écoutes < 0 ou > à 3600 secondes
s <- filter(s, LISTENING_TIME >= 0, LISTENING_TIME <= 3600)

###### Data:Tracks ######

## Import des chansons ; attention : 31M de lignes!!
# songs <- data.table::fread("base/orange/songs.csv", sep = "|", header = FALSE,
#                   col.names = c("SNG_ID", "ALB_ID", "ART_ID", "SNG_TITLE", "ISRC", "DURATION", "DISK_NUMBER", "TRACK_NUMBER", "PHYSICAL_RELEASE_DATE", "DIGITAL_RELEASE_DATE"))
# ## On réduit donc au 281k chansons écoutées dans notre db
# songs <- filter(songs, songs$SNG_ID %in% s$SNG_ID)
# save(songs, file = "data/songs.RData")
load("data/songs.RData")

s <- select(songs, SNG_ID, DURATION, PHYSICAL_RELEASE_DATE, ALB_ID, ART_ID) %>% 
  right_join(s, by="SNG_ID")

s$DURATION[s$DURATION <= 0] <- NA
s$listen_completion <- s$LISTENING_TIME / s$DURATION
s$listen_completion[s$listen_completion > 1] <- NA

save(u, file = "data/french_users.RData")
save(s, file = "data/streams.RData")
