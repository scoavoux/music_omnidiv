library("lubridate")
library(here)
library("tidyverse")
load(here("data", "streams.RData"))

## On définit des sessions d'écoutes = des séquences de pistes
## écoutées par une personne à la suite, avec un délai maximum
## entre deux écoutes (= entre la fin d'une écoute et le début 
## de la suivante, al fin étant estimée par début + duration)

## Note: ne fonctionne pas, à corriger
max_delay <- 15

# user_dic <- data_frame(user_id = unique(st$user_id),
#                        new = paste0("U", seq_along(user_id)))
# 
# st <- left_join(st, user_dic)
# 
st <- arrange(st, user_id, timestamp)
st <- mutate(st, timestamp = ymd_hms(timestamp),
             duration  = seconds(duration))
st <- group_by(st, user_id)
st <- mutate(st, delay = difftime(timestamp, lag(timestamp), units = "mins"))
st <- mutate(st, change = ifelse(delay > max_delay, 1, 0), 
             change = ifelse(is.na(change), 0, change))
st <- mutate(st, streak = paste(user_id, cumsum(change), sep = "_"))
st <- ungroup(st)
save(st, file = "data/streams_streak.RData")

