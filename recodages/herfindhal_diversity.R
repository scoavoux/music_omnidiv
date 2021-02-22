# repris 18/12/2020 pour supprimer la diversité standardisée.
load(here("data", "streams.RData"))
load(here("data", "french_users.RData"))

# Compute Herfindhal diversity
# compute_diversity <- function(freq){
#   sum(freq^2)^-1
# }

# Compute Shannon diversity
compute_diversity <- function(freq){
  prod(freq^freq)^-1
}

# Compute overall probability distribution for artists, genre, legit
# distrib <- select(st, user_id, art_id, genre, legit) %>% 
#   mutate(art_id = as.character(art_id)) %>% 
#   pivot_longer(cols = -user_id) %>% 
#   filter(!is.na(value)) %>% 
#   count(user_id, name, value) %>% 
#   group_by(user_id, name) %>% 
#   mutate(f = n / sum(n)) %>% 
#   arrange(user_id, desc(f)) %>% 
#   mutate(rank = paste0(name, "_", row_number())) %>% 
#   ungroup() %>%
#   select(-n, -name, -value) %>% 
#   pivot_wider(names_from = rank, values_from = f, values_fill = 0) %>% 
#   pivot_longer(cols = -user_id, "rank", values_to = "f") %>%
#   mutate(name = str_replace(rank, "(\\w+)_\\d+$", "\\1"),
#          rank = str_extract(rank, "\\d+")) %>% 
#   group_by(name, rank) %>% 
#   summarise(f = mean(f)) %>% 
#   mutate(rank = as.integer(rank)) %>% 
#   arrange(rank) %>% 
#   pivot_wider(names_from = name, values_from = f) %>% 
#   mutate(across(art_id:legit, ~ifelse(is.na(.x), 0, .x)))
# 
# # 
# randomwalk_div <- function(.nb, .data, .prob){
#   if(is.na(.nb)) return(NA)
#   else if(.nb < 100) return(NA)
#   else {sapply(1L, function(y){ # pour simplifier, ne le faire qu'une fois ; à changer pour meilleure estimation
#     sample(.data, .nb, prob = .prob, replace = TRUE) %>% 
#       tibble() %>% 
#       rename(art = ".") %>% 
#       count(art) %>% 
#       mutate(f = n / sum(n)) %>% 
#       summarise(div = compute_diversity(f)) %>% 
#       pull(div)})  %>% 
#       mean() %>% 
#       return()
#   }
# }
# 
usd <- select(us, -starts_with("div"))
# 
# usd <- mutate(usd, 
#               divstd_artists = map(nb_ecoutes, ~randomwalk_div(.x, distrib$rank, distrib$art_id)),
#               divstd_artists_stock = map(nb_stock, ~randomwalk_div(.x, distrib$rank, distrib$art_id)),
#               divstd_artists_rec = map(nb_guid, ~randomwalk_div(.x, distrib$rank, distrib$art_id)),
#               divstd_genre = map(nb_ecoutes, ~randomwalk_div(.x, distrib$rank, distrib$genre)),
#               divstd_genre_stock = map(nb_stock, ~randomwalk_div(.x, distrib$rank, distrib$genre)),
#               divstd_genre_rec = map(nb_guid, ~randomwalk_div(.x, distrib$rank, distrib$genre)),
#               divstd_legit = map(nb_ecoutes, ~randomwalk_div(.x, distrib$rank, distrib$legit)),
#               divstd_legit_stock = map(nb_stock, ~randomwalk_div(.x, distrib$rank, distrib$legit)),
#               divstd_legit_rec = map(nb_guid, ~randomwalk_div(.x, distrib$rank, distrib$legit)))
# 
# usd <- mutate(usd, across(starts_with("divstd"), ~unlist(.x)))
# 
# select(usd, starts_with("divstd_")) %>% pivot_longer(everything(), names_to = "names", values_to = "values") %>% 
#   ggplot(aes(values)) +
#   geom_histogram() +
#   facet_wrap(~names, scales = "free", ncol = 5)

usd <- count(st, user_id, genre) %>% 
  filter(!is.na(genre)) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_genre = compute_diversity(f)) %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

## Diversité des artistes écoutés
usd <- count(st, user_id, art_id) %>% 
  group_by(user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_artists = compute_diversity(f)) %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

## Diversité des niveaux de légitimité
usd <- select(st, legit, user_id) %>% 
  filter(legit != "Unclassified", !is.na(legit)) %>% 
  count(user_id, legit) %>% 
  group_by(user_id) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = compute_diversity(f)) %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

## Diversités stock vs. edit
### Artists
usd <- filter(st, !is.na(guid)) %>% 
  count(user_id, guid, art_id) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_art = compute_diversity(f)) %>% 
  spread(guid, div_art) %>% 
  rename(div_artists_stock = "Non guidée",
         div_artists_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

usd <- filter(st, !is.na(type_guid), type_guid != "Non guidée") %>% 
  count(user_id, type_guid, art_id) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_art = compute_diversity(f)) %>% 
  spread(type_guid, div_art) %>% 
  rename(div_artists_rec_edit = "Guidage",
         div_artists_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

usd <- filter(st, !is.na(guid), !is.na(genre)) %>% 
  count(user_id, guid, genre) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_genre = compute_diversity(f)) %>% 
  spread(guid, div_genre) %>% 
  rename(div_genre_stock = "Non guidée",
         div_genre_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

usd <- filter(st, !is.na(type_guid), type_guid != "Non guidée", !is.na(genre)) %>% 
  count(user_id, type_guid, genre) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_genre = compute_diversity(f)) %>% 
  spread(type_guid, div_genre) %>% 
  rename(div_genre_rec_edit = "Guidage",
         div_genre_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")


usd <- filter(st, !is.na(guid), !is.na(legit)) %>% 
  count(user_id, guid, legit) %>% 
  group_by(user_id, guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = compute_diversity(f)) %>% 
  spread(guid, div_omni) %>% 
  rename(div_omni_stock = "Non guidée",
         div_omni_rec = "Guidée") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

usd <- filter(st, !is.na(type_guid), type_guid != "Non guidée", !is.na(legit)) %>% 
  count(user_id, type_guid, legit) %>% 
  group_by(user_id, type_guid) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = compute_diversity(f)) %>% 
  spread(type_guid, div_omni) %>% 
  rename(div_omni_rec_edit = "Guidage",
         div_omni_rec_algo = "Flux") %>% 
  ungroup() %>% 
  right_join(usd, by = "user_id")

# usd <- mutate(usd,
#               stddiv_omni_rec = div_omni_rec / divstd_legit_rec,
#               stddiv_omni_stock = div_omni_stock / divstd_legit_stock,
#               stddiv_genre_rec = div_genre_rec / divstd_genre_rec,
#               stddiv_genre_stock = div_genre_stock / divstd_genre_stock,
#               stddiv_artists_rec = div_artists_rec / divstd_artists_rec,
#               stddiv_artists_stock = div_artists_stock / divstd_artists_stock,
#               stddiv_omni = div_omni / divstd_legit,
#               stddiv_artists = div_artists / divstd_artists,
#               stddiv_genre = div_genre / divstd_genre)
# 
# # Force stddiv to the same scale
# ## log artists
# usd <- mutate(usd, across(starts_with("stddiv_artists"), ~log(.x)))
# ## sqrt genre
# usd <- mutate(usd, across(starts_with("stddiv_genre"), ~sqrt(.x)))


#### Diversité par mois

st <- mutate(st, month = month(timestamp))

## Diversité des genres
usd <- count(st, month, user_id, genre) %>% 
  filter(!is.na(genre)) %>% 
  group_by(month, user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_genre = compute_diversity(f)) %>% 
  ungroup() %>% 
  mutate(month = paste0("divm_genre_", month)) %>% 
  pivot_wider(names_from = month, values_from = div_genre) %>% 
  right_join(usd, by = "user_id")

## Diversité des artistes écoutés
usd <- count(st, month, user_id, art_id) %>% 
  group_by(month, user_id) %>% 
  mutate(f = n / sum(n)) %>% 
  summarize(div_artists = compute_diversity(f)) %>% 
  ungroup() %>% 
  mutate(month = paste0("divm_art_", month)) %>% 
  pivot_wider(names_from = month, values_from = div_artists) %>% 
  right_join(usd, by = "user_id")

## Diversité des niveaux de légitimité
usd <- select(st, legit, user_id, month) %>% 
  filter(legit != "Unclassified", !is.na(legit)) %>% 
  count(month, user_id, legit) %>% 
  group_by(month, user_id) %>% 
  mutate(f = n/sum(n)) %>% 
  summarise(div_omni = compute_diversity(f)) %>% 
  ungroup() %>% 
  mutate(month = paste0("divm_omni_", month)) %>% 
  pivot_wider(names_from = month, values_from = div_omni) %>% 
  right_join(usd, by = "user_id")

usd <- count(st, user_id, month, guid) %>% 
  mutate(guid = factor(guid, 
                       levels = c("Guidée", "Non guidée"), 
                       labels = c("rec", "stock")),
         month = paste0("n_", guid, month)) %>% 
  filter(!is.na(guid)) %>%
  select(-guid) %>% 
  pivot_wider(names_from = month, values_from = n) %>% 
  right_join(usd, by = "user_id")

save(usd, file = here("data", "herfindhal.RData"))