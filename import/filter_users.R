# Sélection des individus à supprimer de la base

## Supprimer les individus qui n'ont pas de pistes dans st
us <- filter(us, user_id %in% unique(st$user_id))

us <- filter(us, 
             ## Individus identifiés un à un par Sisley
             !(row_number() %in% c(83, 144, 169, 184, 256, 552, 647, 661, 818, 866, 923, 933, 945, 1104, 1239, 1585, 2208, 2295, 2573, 2895, 3157, 3189, 3279, 3389, 3693, 3744, 3764, 3872) ),
             ## Individus qui ont fait plus de 100 ecoutes (nécessaire pour calculer la suite)
             nb_ecoutes > 260,
             nb_ecoutes < 48000 # 50000, mais on veut aussi virer un mec à 49000 qui écoute plus de 24h par jour
             )
# ggplot(u, aes(x = n)) + geom_histogram() + scale_x_log10()
# ggplot(u, aes(x = t)) + geom_histogram() + scale_x_log10()



## enlever de st
st <- filter(st, user_id %in% us$user_id)
