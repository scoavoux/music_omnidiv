library(here)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(questionr)
library(knitr)

load(here("data", "streams.RData"))
st$context_cat[st$context_cat %in% c("ND", "unknown")] <- NA
st$context_cat <- droplevels(st$context_cat)

## Premier tableau croisé
table(st$genre, st$context_cat) %>% lprop() %>% knitr::kable()

## On supprime les catégories les moins présentes
st$context_cat[st$context_cat %in% c("experts_editor", "explore_release", "feed_track", "feed_smartradio", "player_defaut", "social")] <- NA
st$context_cat <- droplevels(st$context_cat)

table(st$genre, st$context_cat) %>% lprop() %>% knitr::kable()
gd_ca <- CA(table(st$context_cat, st$genre), graph = FALSE, ncp = 3)

fviz_eig(gd_ca)
kable(gd_ca$row$contrib)
kable(gd_ca$col$contrib)
fviz_ca_biplot(gd_ca, axes = c(1, 2))
fviz_ca_biplot(gd_ca, axes = c(1, 3))
