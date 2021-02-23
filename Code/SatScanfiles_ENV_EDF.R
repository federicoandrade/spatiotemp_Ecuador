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

################################################################################
################################################################################


####################################################
## For Un-adjusted clusters - Fetal death Stillbirth
####################################################
load("Data/env2009_2019control.RData")
load("Data/edf2009_2019case.RData")
load("Data/EcuadorCantCent.RData")



###### Cases files

edf2009_2019case1 <-  edf2009_2019case %>%
  mutate(numberCases = rep(1)) %>%
  dplyr::select(cant_res, numberCases, anio_fall)  

edf2009_2019case1 <- as.data.frame(edf2009_2019case1)
write.cas(edf2009_2019case1, location = "Data/SatscanFiles", filename = "edf2009_2019case1") 

###Controls file
env2009_2019control1 <-  env2009_2019control %>%
  mutate(numberCases = rep(1)) %>%
  dplyr::select(cant_res, numberCases, anio_nac)  

env2009_2019control1 <- as.data.frame(env2009_2019control1)
write.ctl(env2009_2019control1, location = "Data/SatscanFiles", filename = "env2009_2019control1")



##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 


Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)



write.geo(Ecuador_Cantones_GEO, location = "Data/SatscanFiles", filename = "Ecuador_Cantones_GEO")



##################################
## For adjusted clusters - Fetal death Stillbirth
################################## 

###### Cases files

edf2009_2019case <-  edf2009_2019case %>%
mutate(numberCases = rep(1))   

edf2009_2019case <- as.data.frame(edf2009_2019case) 

#Edad --> edad_mad < 20 ~ "1", edad_mad >= 20 & edad_mad < 35 ~ "2", edad_mad >= 35 ~ "3"

#case_when(niv_inst == 0 | niv_inst == 1 ~ "1", 
#          niv_inst == 2 |niv_inst == 3 |niv_inst == 4  ~ "2",
#          niv_inst == 5 |niv_inst == 6  ~ "3",
#          niv_inst == 7 | niv_inst == 8 ~ "4",
#          niv_inst == 9 ~ "9"))


#1 Cas Ed1, N1

edf2009_2019case_Ed1_N1 <- edf2009_2019case  %>% 
  filter(edad_cat == "1", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed1_N1, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed1_N1") 

#2 Cas Ed1, N2

edf2009_2019case_Ed1_N2 <- edf2009_2019case  %>% 
  filter(edad_cat == "1", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed1_N2, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed1_N2")

#3 Cas Ed1, N3

edf2009_2019case_Ed1_N3 <- edf2009_2019case  %>% 
  filter(edad_cat == "1", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed1_N3, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed1_N3")

#4 Cas Ed1, N4

edf2009_2019case_Ed1_N4 <- edf2009_2019case  %>% 
  filter(edad_cat == "1", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed1_N4, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed1_N4")

#5 Cas Ed2, N1

edf2009_2019case_Ed2_N1 <- edf2009_2019case  %>% 
  filter(edad_cat == "2", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed2_N1, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed2_N1") 

#6 Cas Ed2, N2

edf2009_2019case_Ed2_N2 <- edf2009_2019case  %>% 
  filter(edad_cat == "2", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed2_N2, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed2_N2") 

#7 Cas Ed2, N3

edf2009_2019case_Ed2_N3 <- edf2009_2019case  %>% 
  filter(edad_cat == "2", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed2_N3, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed2_N3")

#8 Cas Ed2, N4

edf2009_2019case_Ed2_N4 <- edf2009_2019case  %>% 
  filter(edad_cat == "2", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed2_N4, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed2_N4")

#9 Cas Ed3, N1

edf2009_2019case_Ed3_N1 <- edf2009_2019case  %>% 
  filter(edad_cat == "3", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed3_N1, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed3_N1") 

#10 Cas Ed3, N2

edf2009_2019case_Ed3_N2 <- edf2009_2019case  %>% 
  filter(edad_cat == "3", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed3_N2, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed3_N2")

#11 Cas Ed3, N3

edf2009_2019case_Ed3_N3 <- edf2009_2019case  %>% 
  filter(edad_cat == "3", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed3_N3, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed3_N3")

#12 Cas Ed3, N4

edf2009_2019case_Ed3_N4 <- edf2009_2019case  %>% 
  filter(edad_cat == "3", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_fall)

write.cas(edf2009_2019case_Ed3_N4, location = "Data/SatscanFiles", filename = "edf2009_2019case_Ed3_N4")

#############################################
######### Adjusted Controls Fetal Death
#############################################

env2009_2019control <-  env2009_2019control %>%
  mutate(numberCases = rep(1)) 

env2009_2019control <- as.data.frame(env2009_2019control)



#1 Control Ed1, N1

edf2009_2019cont_Ed1_N1 <- env2009_2019control  %>% 
  filter(edad_cat == "1", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed1_N1, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed1_N1") 

#2 Control Ed1, N2

edf2009_2019cont_Ed1_N2 <- env2009_2019control  %>% 
  filter(edad_cat == "1", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed1_N2, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed1_N2") 



#3 Control Ed1, N3

edf2009_2019cont_Ed1_N3 <- env2009_2019control  %>% 
  filter(edad_cat == "1", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed1_N3, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed1_N3") 

#4 Control Ed1, N4

edf2009_2019cont_Ed1_N4 <- env2009_2019control  %>% 
  filter(edad_cat == "1", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed1_N4, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed1_N4") 

#5 Control Ed2, N1

edf2009_2019cont_Ed2_N1 <- env2009_2019control  %>% 
  filter(edad_cat == "2", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed2_N1, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed2_N1")

#6 Control Ed2, N2

edf2009_2019cont_Ed2_N2 <- env2009_2019control  %>% 
  filter(edad_cat == "2", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed2_N2, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed2_N2")

#7 Control Ed2, N3

edf2009_2019cont_Ed2_N3 <- env2009_2019control  %>% 
  filter(edad_cat == "2", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed2_N3, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed2_N3")

#8 Control Ed2, N4

edf2009_2019cont_Ed2_N4 <- env2009_2019control  %>% 
  filter(edad_cat == "2", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed2_N4, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed2_N4")

#9 Control Ed3, N1

edf2009_2019cont_Ed3_N1 <- env2009_2019control  %>% 
  filter(edad_cat == "3", niv_inst_cat == "1") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed3_N1, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed3_N1")

#10 Control Ed3, N2

edf2009_2019cont_Ed3_N2 <- env2009_2019control  %>% 
  filter(edad_cat == "3", niv_inst_cat == "2") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed3_N2, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed3_N2")

#11 Control Ed3, N3

edf2009_2019cont_Ed3_N3 <- env2009_2019control  %>% 
  filter(edad_cat == "3", niv_inst_cat == "3") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed3_N3, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed3_N3")

#12 Control Ed3, N4

edf2009_2019cont_Ed3_N4 <- env2009_2019control  %>% 
  filter(edad_cat == "3", niv_inst_cat == "4") %>%
  dplyr::select(cant_res, numberCases, anio_nac)

write.ctl(edf2009_2019cont_Ed3_N4, location = "Data/SatscanFiles", filename = "edf2009_2019cont_Ed3_N4")



