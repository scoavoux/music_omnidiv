## In english
library(forcats)

us <- mutate(us,
             passifs = fct_recode(passifs,
                                  `Mostly active (freq. algo. $\\approx$ 0)` = "Usagers exclusivement actifs",
                                  `Mixed (freq. algo. $\\approx$ 0)` = "Usagers mixtes",
                                  `Mostly passive (freq. algo. >= 0.8)` = "Usagers principalement passifs"))


st <- mutate(st, 
             genre = fct_recode(genre,
                                `Kids' music` = "Jeunesse",
                                Musicals = "Comédies musicales",
                                `French songs` = "Chanson française",
                                Classical = "Classique",
                                `Hip-hop` = "Rap/Hip Hop",
                                Electronic = "Electro",
                                `Movies/video games OST` = "Films/Jeux vidéo"  ))


