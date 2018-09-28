uso <- count(st, user_id, genre) %>%
  filter(n > 10) %>% 
  group_by(user_id) %>% 
  summarise(omni_clust = case_when(any(genre == "Classical") & any(genre == "Rock")   & any(genre == "Hip-hop") ~ "Omnivores",
                             any(genre == "Classical") & any(genre == "Rock")                             ~ "No lowbrow",
                             any(genre == "Classical") & any(genre == "Hip-hop")                          ~ "No Middlebrow",
                             any(genre == "Rock")      & any(genre == "Hip-hop")                          ~ "No Highbrow",
                             any(genre == "Classical")                                                    ~ "Highbrow univore",
                             any(genre == "Rock")                                                         ~ "Middlebrow univore",
                             any(genre == "Hip-hop")                                                      ~ "Lowbrow univore",
                             TRUE ~ "Unclassified") %>% 
              factor(levels = c("Highbrow univore", 
                                "Omnivores", 
                                "No lowbrow",
                                "No Middlebrow",
                                "Middlebrow univore",
                                "No Highbrow", 
                                "Lowbrow univore", 
                                "Unclassified"
              ))) %>% 
  left_join(us)




ocd1 <- select(uso, omni_clust, fq, fa, fe, div_artists, div_genre, nb_ecoutes) %>% 
  group_by(omni_clust) %>% 
  summarise_all(funs(mean_sd)) %>% 
  gather(key, value, -omni_clust) %>% 
  spread(omni_clust, value)

ocd2 <- count(uso, omni_clust, gender) %>% 
  filter(!is.na(gender)) %>% 
  group_by(omni_clust) %>% 
  mutate(n = paste0(round(n/sum(n) * 100, 1), "%")) %>% 
  filter(gender == "F") %>%
  spread(omni_clust, n) %>% 
  rename(key = gender)

bind_rows(ocd1, ocd2) %>% 
  kable(caption = "Omnivorism clusters")



ddd <- mutate(st, omni = omni_dic[genre]) %>% 
  filter(omni != "Unclassified", !is.na(omni))
freq(ddd$omni)
