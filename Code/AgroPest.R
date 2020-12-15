##AGRo
library(haven)
library(tidyverse)



cultperm2016 <- read_sav("Data/cpnac2016.sav")
usonac2016 <- read_sav("Data/usnac2016.sav")
sunac2019 <- read_sav("Data/sunac2019.sav")
cp2019 <- read_sav("Data/cpnac2019.sav")
ct2019 <- read_sav("Data/ctnac2019.sav")


table(cp2019$cp_afito)
table(cp2019$cp_pqui)
table(cp2019$cp_cantidad_her_pq)
