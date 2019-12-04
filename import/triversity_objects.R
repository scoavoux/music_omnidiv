library(triversity)

mp <- get_multipartite_from_df(data = filter(st, !is.na(genre), !is.na(art_id)), colnames = c("user_id", "art_id", "genre"))
mp_guid <- get_multipartite_from_df(data = filter(st, guid == "Guidée", !is.na(genre), !is.na(art_id)), colnames = c("user_id", "art_id", "genre"))
mp_noguid <- get_multipartite_from_df(data = filter(st, guid == "Non guidée", !is.na(genre), !is.na(art_id)), colnames = c("user_id", "art_id", "genre"))

save(mp, mp_guid, mp_noguid, file = "data/triversity_objects.RData")
