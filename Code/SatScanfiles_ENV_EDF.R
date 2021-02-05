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
load("Data/env2009_2019_LBW_par.RData")
load("Data/env2009_2019_GES.RData")
load("Data/env2009_2019_GES_par.RData")
load("Data/EcuadorCantCent.RData")



##################################
## For Un-adjusted clusters - LBW
##################################


###### Cases files

env2009_2019_LBWcas <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "1") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

write.table(env2009_2019_LBWcas, "Data/SatscanFiles/env2009_2019_LBWcas.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE) 

###Controls file
env2009_2019_LBWcont <-  env2009_2019_LBW %>%
  mutate(numberCases = rep(1)) %>%
  filter(lbw == "0") %>% 
  dplyr::select(cant_res, numberCases, anio_nac)  

write.table(env2009_2019_LBWcont, "Data/SatscanFiles/env2009_2019_LBWcont.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)



##### Geo file

Ecuador_Cantones_GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) 
  

Ecuador_Cantones_GEO <- as.data.frame(Ecuador_Cantones_GEO)


write.table(Ecuador_Cantones_GEO, "Data/SatscanFiles/Ecuador_Cantones_GEO.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)

 