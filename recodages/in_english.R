## In english
library(forcats)

if(exists("us")){
us <- mutate(us,
             passifs = fct_recode(passifs,
                                  `Mostly active (freq. algo. =~ 0)` = "Usagers exclusivement actifs",
                                  `Mixed (freq. algo. > 0)` = "Usagers mixtes",
                                  `Mostly passive (freq. algo. >= 0.8)` = "Usagers principalement passifs"))
}

if(exists("usd")){
usd <- mutate(usd,
             passifs = fct_recode(passifs,
                                  `Mostly active (freq. algo. =~ 0)` = "Usagers exclusivement actifs",
                                  `Mixed (freq. algo. > 0)` = "Usagers mixtes",
                                  `Mostly passive (freq. algo. >= 0.8)` = "Usagers principalement passifs"))
}

if(exists("st")){
st <- mutate(st, 
             guid = fct_recode(guid,
                               `Recommended` = "Guidée",
                               `Stock` = "Non guidée"))


st <- mutate(st, type_guid = fct_recode(type_guid,
                                        `Algorithmic recommendation` = "Flux",
                                        `Editorial recommendation` = "Guidage",
                                        `Stock` = "Non guidée"))
}