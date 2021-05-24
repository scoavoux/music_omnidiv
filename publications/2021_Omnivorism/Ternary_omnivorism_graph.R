library(tidyverse)
library(here)
library(ggtern)
load(here("data", "french_users.RData"))
# source(here("recodages", "herfindhal_diversity.R"))
# save(usd, file = here("data", "herfindhal.RData"))

load(here("data", "herfindhal.RData"))
source(here("recodages", "in_english.R"))

x <- ggtern(us, aes(Lowbrow, Middlebrow, Highbrow, color = div_omni)) +
  geom_point() +
  theme_bw() +
  labs(color = "Omnivorism")

ggsave(filename = "omnivorism_tern.png", plot = x)


