library(tidyverse)
library(questionr)
library(stargazer)
library(gridExtra)

if(!dir.exists("resultats")) dir.create("resultats")

theme_set(theme_bw(base_size = 18))

load("data/streams.RData")
load("data/french_users.RData")
source("scripts/in_english.R")

### Diversity by use frequency of algorithmic recommandation

gg1 <- ggplot(us, aes(fq, div_artists)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Frequency of use of algorithmic features",
       y = "Diversity (artists)") +
  ylim(0, 750)

gg2 <- ggplot(us, aes(fq, div_genre)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Frequency of use of algorithmic features",
       y = "Diversity (genres)")

grid.arrange(gg1, gg2, ncol=2) %>% ggsave(filename = "resultats/div_algo.png", plot = .)

### Mean diversity by activity profile

filter(us, !is.na(passifs)) %>% 
  group_by(passifs) %>% 
  summarize(`Mean (artists) diversity (sd)` = paste0(round(mean(div_artists), 1), " (", round(sd(div_artists), 1), ")"),
            `Mean (genres) diversity (sd)` = paste0(round(mean(div_genre), 1), " (", round(sd(div_genre), 1), ")")) %>% 
  rename(` ` = passifs) %>% 
  kable()

### Determinants of diversity

dalm <- lm(div_artists ~ age + I(fq*100) + I(freq_nouveaute*100) + I(anciennete_days/365) + I(nb_ecoutes/100), data = us)
dglm <- lm(div_genre ~ age + I(fq*100) + I(freq_nouveaute*100) + I(anciennete_days/365) + I(nb_ecoutes/100), data = us)
library(pander)
stargazer(dalm, dglm)

### Genres and diversity

gg <- count(st, genre, guid) %>% 
  filter(!is.na(guid)) %>% 
  group_by(genre) %>% 
  mutate(f = n / sum(n)) %>% 
  ungroup() %>% 
  filter(guid == "Guidée", !is.na(genre)) %>% 
  arrange(f) %>% 
  mutate(genre = factor(genre, levels = genre)) %>% 
  ggplot(aes(genre, f)) + 
  geom_col() +
  labs(y = "Frequency of use of algorithmic recommendation", 
       x = "Genres") +
  coord_flip() +
  ylim(0, 0.4)

ggsave("resultats/use_genres.png", gg)

### Genres and diversity

df <- count(st, genre, art_id) %>% 
  filter(!is.na(genre)) %>% 
  group_by(genre) %>% 
  mutate(f = n / sum(n)) %>% 
  summarise(n_art = n(),
            div_art = (prod(f^f)^-1) %>% round(1),
            div_art_std = (prod(f^f)^-1 / n_art * 100) %>% round(1)) %>% 
  arrange(div_art_std) %>% 
  mutate(genre = factor(genre, levels = unique(genre)))


gg <- ggplot(df, aes(genre, div_art_std)) +
  geom_point() +
  geom_label(aes(y = 13, label = paste0("N art. = ", format(n_art, big.mark = " "))), hjust=0) +
  ylim(0, 15) +
  labs(x = "Genres",
       y = "Standardized intra-genre diversity") +
  coord_flip()

ggsave("resultats/intra_genre_div.png", gg)
### Listening again

stpf <- group_by(st, user_id, art_id) %>% 
  mutate(n_reecoute = n()) %>% 
  arrange(timestamp) %>% 
  slice(1) %>% 
  filter(!(context_cat %in% c("stock", "unknown", "ND")), !is.na(context_cat)) %>% 
  mutate(type_guid = fct_recode(type_guid,
                                `Algorithmic recommendation` = "Flux",
                                `Editorial recommendation` = "Guidage",
                                `Autonomous exploration` = "Non guidée"))

group_by(stpf, type_guid) %>% 
  summarize(`Mean # of listen` = mean(n_reecoute),
            `Std. dev.` = sd(n_reecoute)) %>% 
  rename(`Mean of discovery` = type_guid) %>% 
  arrange(desc(`Mean # of listen`)) %>%
  kable(caption = "Distribution du nombre de réécoute par dispositif de première écoute / non_favori, seulement les 5 plus forte réécoutes moyennes")
