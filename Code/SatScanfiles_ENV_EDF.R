################################################################################
###### Preparing files for Satscan. ENV -EDF
################################################################################
library(tidyverse)
library(rsatscan)
library(haven)

################################################################################

### Loading data
load("Data/env2009_2019.RData")
load("Data/env2009_2019_LBW.RData")
load("Data/env2009_2019_GES.RData")
load("Data/EcuadorCantCent.RData")



##################################
## For Un-adjusted clusters - LBW
##################################


###### Cases files

env2009_2019_LBWcas <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "1") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019_LBWcas <- as.data.frame(env2009_2019_LBWcas)
write.cas(env2009_2019_LBWcas, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas") 

###Controls file
env2009_2019_LBWcont <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "0") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019_LBWcont <- as.data.frame(env2009_2019_LBWcont)
write.ctl(env2009_2019_LBWcont, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont")



##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 
  

Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)



write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")


##################################
## For adjusted clusters - LBW
################################## 

###### Cases files

env2009_2019_LBWcas <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "1") 

#Edad --> edad_mad < 20 ~ "1", edad_mad >= 20 & edad_mad < 35 ~ "2", edad_mad >= 35 ~ "3"

#Niv_inst - Educacion Basica o menos ~ "1",
#         - Educacion Media o mas ~ "2"
#         - Se ignora ~ "9"
                           
#Etnia    - Minoria etnica "1"
#         - Mestizo "2"
#         - Se ignora ~ 9


#1 Cas Ed1, N1, E1 

env2009_2019_LBWcas_Ed1_N1_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N1_E1 <- as.data.frame(env2009_2019_LBWcas_Ed1_N1_E1)
write.cas(env2009_2019_LBWcas_Ed1_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N1_E1") 


 

#2 Cas Ed1, N1, E2

env2009_2019_LBWcas_Ed1_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed1_N1_E2)
write.cas(env2009_2019_LBWcas_Ed1_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N1_E2") 

#3 Cas Ed1, N2, E1

env2009_2019_LBWcas_Ed1_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed1_N2_E1)
write.cas(env2009_2019_LBWcas_Ed1_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N2_E1")  

#4 Cas Ed1, N2, E2

env2009_2019_LBWcas_Ed1_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed1_N2_E2)
write.cas(env2009_2019_LBWcas_Ed1_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N2_E2") 

#5 Cas Ed2, N1, E1 

env2009_2019_LBWcas_Ed2_N1_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N1_E1 <- as.data.frame(env2009_2019_LBWcas_Ed2_N1_E1)
write.cas(env2009_2019_LBWcas_Ed2_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N1_E1")  

#6 Cas Ed2, N1, E2 

env2009_2019_LBWcas_Ed2_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed2_N1_E2)
write.cas(env2009_2019_LBWcas_Ed2_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N1_E2") 

#7 Cas Ed2, N2, E1 

env2009_2019_LBWcas_Ed2_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed2_N2_E1)
write.cas(env2009_2019_LBWcas_Ed2_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N2_E1") 

#8 Cas Ed2, N2, E2 

env2009_2019_LBWcas_Ed2_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed2_N2_E2)
write.cas(env2009_2019_LBWcas_Ed2_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N2_E2") 

#9 Cas Ed3, N1, E1 

env2009_2019_LBWcas_Ed3_N1_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N1_E1 <- as.data.frame(env2009_2019_LBWcas_Ed3_N1_E1)
write.cas(env2009_2019_LBWcas_Ed3_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N1_E1") 

#10 Cas Ed3, N1, E2 

env2009_2019_LBWcas_Ed3_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed3_N1_E2)
write.cas(env2009_2019_LBWcas_Ed3_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N1_E2")

#11 Cas Ed3, N2, E1 

env2009_2019_LBWcas_Ed3_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed3_N2_E1)
write.cas(env2009_2019_LBWcas_Ed3_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N2_E1")

#12 Cas Ed3, N2, E2 

env2009_2019_LBWcas_Ed3_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed3_N2_E2)
write.cas(env2009_2019_LBWcas_Ed3_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N2_E2")




###Controls file
env2009_2019_LBWcont <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "0")  


#1 Con Ed1, N1, E1 

env2009_2019_LBWcont_Ed1_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)


env2009_2019_LBWcont_Ed1_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed1_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed1_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N1_E1") 

#2 Con Ed1, N1, E2

env2009_2019_LBWcont_Ed1_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed1_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed1_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N1_E2") 

#3 Con Ed1, N2, E1

env2009_2019_LBWcont_Ed1_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed1_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed1_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N2_E1") 

#4 Con Ed1, N2, E2

env2009_2019_LBWcont_Ed1_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed1_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed1_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N2_E2") 

#5 Con Ed2, N1, E1 

env2009_2019_LBWcont_Ed2_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed2_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed2_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N1_E1") 

#6 Con Ed2, N1, E2 

env2009_2019_LBWcont_Ed2_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed2_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed2_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N1_E2") 

#7 Con Ed2, N2, E1 

env2009_2019_LBWcont_Ed2_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed2_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed2_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N2_E1") 

#8 Con Ed2, N2, E2 

env2009_2019_LBWcont_Ed2_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed2_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed2_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N2_E2") 

#9 Con Ed3, N1, E1 

env2009_2019_LBWcont_Ed3_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed3_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed3_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N1_E1") 

#10 Con Ed3, N1, E2 

env2009_2019_LBWcont_Ed3_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed3_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed3_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N1_E2")

#11 Con Ed3, N2, E1 

env2009_2019_LBWcont_Ed3_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed3_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed3_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N2_E1")

#12 Con Ed3, N2, E2 

env2009_2019_LBWcont_Ed3_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed3_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed3_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N2_E2")


##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 


Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)


write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")



###############################################################################

###############################################################################


##################################
## For Un-adjusted clusters - GES
##################################


###### Cases files

env2009_2019_GEScas <-  env2009_2019_GES %>%
  mutate(numberCases = rep(1)) %>%
  filter(preterm == "1") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019_GEScas <- as.data.frame(env2009_2019_GEScas)
write.cas(env2009_2019_GEScas, location = "Data/SatscanFiles", filename = "env2009_2019_GEScas") 

###Controls file
env2009_2019_GEScont <-  env2009_2019_GES %>%
  mutate(numberCases = rep(1)) %>%
  filter(preterm == "0") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019_GEScont <- as.data.frame(env2009_2019_GEScont)
write.ctl(env2009_2019_GEScont, location = "Data/SatscanFiles", filename = "env2009_2019_GEScont")



##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 


Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)



write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")


##################################
## For adjusted clusters - GES
################################## 
##### Here on February 7th 2021, missing to set the satscan files for adjusted GES

###### Cases files

env2009_2019_GEScas <-  env2009_2019_GES %>%
  mutate(numberCases = rep(1)) %>%
  filter(preterm == "1") 

#Edad --> edad_mad < 20 ~ "1", edad_mad >= 20 & edad_mad < 35 ~ "2", edad_mad >= 35 ~ "3"

#Niv_inst - Educacion Basica o menos ~ "1",
#         - Educacion Media o mas ~ "2"
#         - Se ignora ~ "9"

#Etnia    - Minoria etnica "1"
#         - Mestizo "2"
#         - Se ignora ~ 9


#1 Cas Ed1, N1, E1 

env2009_2019_GEScas_Ed1_N1_E1 <- env2009_2019_GEScas %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_GEScas_Ed1_N1_E1 <- as.data.frame(env2009_2019_GEScas_Ed1_N1_E1)
write.cas(env2009_2019_GEScas_Ed1_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_GEScas_Ed1_N1_E1") 




#2 Cas Ed1, N1, E2

env2009_2019_LBWcas_Ed1_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed1_N1_E2)
write.cas(env2009_2019_LBWcas_Ed1_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N1_E2") 

#3 Cas Ed1, N2, E1

env2009_2019_LBWcas_Ed1_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed1_N2_E1)
write.cas(env2009_2019_LBWcas_Ed1_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N2_E1")  

#4 Cas Ed1, N2, E2

env2009_2019_LBWcas_Ed1_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed1_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed1_N2_E2)
write.cas(env2009_2019_LBWcas_Ed1_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed1_N2_E2") 

#5 Cas Ed2, N1, E1 

env2009_2019_LBWcas_Ed2_N1_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N1_E1 <- as.data.frame(env2009_2019_LBWcas_Ed2_N1_E1)
write.cas(env2009_2019_LBWcas_Ed2_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N1_E1")  

#6 Cas Ed2, N1, E2 

env2009_2019_LBWcas_Ed2_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed2_N1_E2)
write.cas(env2009_2019_LBWcas_Ed2_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N1_E2") 

#7 Cas Ed2, N2, E1 

env2009_2019_LBWcas_Ed2_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed2_N2_E1)
write.cas(env2009_2019_LBWcas_Ed2_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N2_E1") 

#8 Cas Ed2, N2, E2 

env2009_2019_LBWcas_Ed2_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed2_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed2_N2_E2)
write.cas(env2009_2019_LBWcas_Ed2_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed2_N2_E2") 

#9 Cas Ed3, N1, E1 

env2009_2019_LBWcas_Ed3_N1_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N1_E1 <- as.data.frame(env2009_2019_LBWcas_Ed3_N1_E1)
write.cas(env2009_2019_LBWcas_Ed3_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N1_E1") 

#10 Cas Ed3, N1, E2 

env2009_2019_LBWcas_Ed3_N1_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N1_E2 <- as.data.frame(env2009_2019_LBWcas_Ed3_N1_E2)
write.cas(env2009_2019_LBWcas_Ed3_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N1_E2")

#11 Cas Ed3, N2, E1 

env2009_2019_LBWcas_Ed3_N2_E1 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N2_E1 <- as.data.frame(env2009_2019_LBWcas_Ed3_N2_E1)
write.cas(env2009_2019_LBWcas_Ed3_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N2_E1")

#12 Cas Ed3, N2, E2 

env2009_2019_LBWcas_Ed3_N2_E2 <- env2009_2019_LBWcas %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcas_Ed3_N2_E2 <- as.data.frame(env2009_2019_LBWcas_Ed3_N2_E2)
write.cas(env2009_2019_LBWcas_Ed3_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcas_Ed3_N2_E2")




###Controls file
env2009_2019_LBWcont <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "0")  


#1 Con Ed1, N1, E1 

env2009_2019_LBWcont_Ed1_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)


env2009_2019_LBWcont_Ed1_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed1_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed1_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N1_E1") 

#2 Con Ed1, N1, E2

env2009_2019_LBWcont_Ed1_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed1_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed1_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N1_E2") 

#3 Con Ed1, N2, E1

env2009_2019_LBWcont_Ed1_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed1_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed1_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N2_E1") 

#4 Con Ed1, N2, E2

env2009_2019_LBWcont_Ed1_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "1", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed1_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed1_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed1_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed1_N2_E2") 

#5 Con Ed2, N1, E1 

env2009_2019_LBWcont_Ed2_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed2_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed2_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N1_E1") 

#6 Con Ed2, N1, E2 

env2009_2019_LBWcont_Ed2_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed2_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed2_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N1_E2") 

#7 Con Ed2, N2, E1 

env2009_2019_LBWcont_Ed2_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed2_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed2_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N2_E1") 

#8 Con Ed2, N2, E2 

env2009_2019_LBWcont_Ed2_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "2", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed2_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed2_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed2_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed2_N2_E2") 

#9 Con Ed3, N1, E1 

env2009_2019_LBWcont_Ed3_N1_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N1_E1 <- as.data.frame(env2009_2019_LBWcont_Ed3_N1_E1)
write.ctl(env2009_2019_LBWcont_Ed3_N1_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N1_E1") 

#10 Con Ed3, N1, E2 

env2009_2019_LBWcont_Ed3_N1_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "1", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N1_E2 <- as.data.frame(env2009_2019_LBWcont_Ed3_N1_E2)
write.ctl(env2009_2019_LBWcont_Ed3_N1_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N1_E2")

#11 Con Ed3, N2, E1 

env2009_2019_LBWcont_Ed3_N2_E1 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N2_E1 <- as.data.frame(env2009_2019_LBWcont_Ed3_N2_E1)
write.ctl(env2009_2019_LBWcont_Ed3_N2_E1, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N2_E1")

#12 Con Ed3, N2, E2 

env2009_2019_LBWcont_Ed3_N2_E2 <- env2009_2019_LBWcont %>% 
  filter(edad_cat == "3", niv_inst_cat == "2", etnia_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

env2009_2019_LBWcont_Ed3_N2_E2 <- as.data.frame(env2009_2019_LBWcont_Ed3_N2_E2)
write.ctl(env2009_2019_LBWcont_Ed3_N2_E2, location = "Data/SatscanFiles", filename = "env2009_2019_LBWcont_Ed3_N2_E2")


##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 


Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)


write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")




####################################################
## For Un-adjusted clusters - Fetal death Stillbirth
####################################################
load("Data/env2009_2019control.RData")
load("Data/edf2009_2019case.RData")
load("Data/EcuadorCantCent.RData")



###### Cases files

edf2009_2019case <-  edf2009_2019case %>%
  mutate(numberCases = rep(1)) %>%
  dplyr::select(cant_res, numberCases, anio_fall)  

edf2009_2019case <- as.data.frame(edf2009_2019case)
write.cas(edf2009_2019case, location = "Data/SatscanFiles", filename = "edf2009_2019case") 

###Controls file
env2009_2019control <-  env2009_2019control %>%
  mutate(numberCases = rep(1)) %>%
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019control <- as.data.frame(env2009_2019control)
write.ctl(env2009_2019control, location = "Data/SatscanFiles", filename = "env2009_2019control")



##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 


Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)



write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")





