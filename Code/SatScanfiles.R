################################################################################
###### Preparing files for Satscan
################################################################################
library(tidyverse)
library(rsatscan)
library(haven)

################################################################################

### Can I do it with the function in R sat scan?
load("Data/eg2011_2019_extVar.RData")
load("Data/populationCanton.RData")
load("Data/EcuadorCantCent.RData")
##reset parameter file satscan
#invisible(ss.options(reset=TRUE))

######################
## For pesticide Intoxication T60
######################


pest_intox2011_2019 <- eg2011_2019_extVar %>% filter(str_detect(cau_cie10, "T60")) 


######################
## For pesticide Intoxication T60
######################


pest_intox2011_2019 <- eg2011_2019_extVar %>% filter(str_detect(cau_cie10, "T60")) 


###### cas files

pest2011_2019CAS <- pest_intox2011_2019 %>% 
  mutate(numberCases = rep(1)) %>% 
  dplyr::select(cant_res, numberCases, anio_ingr)  %>% 
  filter(cant_res != "2302")

write.table(pest2011_2019CAS, "Data/SatscanFiles/pest2011_2019CAS.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE) 

##### Geo file

pest_2011_2019GEO <- CantCentr_df %>% 
  dplyr::select(DPA_CANTON, x, y) %>% 
  filter(DPA_CANTON != "2302")

pest_2011_2019GEO <- as.data.frame(pest_2011_2019GEO)


write.table(pest_2011_2019GEO, "Data/SatscanFiles/pest2011_2019GEO.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)

###population file

pest_2011_2019POP <-proy_pob2010_2020 

write.table(pest_2011_2019POP, "Data/SatscanFiles/pest2011_2019POP.txt", sep= " ", 
            quote = FALSE, row.names = FALSE, col.names = FALSE)

