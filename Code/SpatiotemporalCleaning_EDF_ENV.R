library(haven)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(rgeos)
library(geosphere)
library(rgdal)

#library(raster)
#####
#Solving conflict functions, with devtools
#conflict_prefer("select", "dplyr")


##############################################################################
#Spatiotemporal - Nacimientos vivos y Defunciones fetales 2011-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################

edf2019 <- read_sav("Data/EDF_2019.sav")
edf2018 <- read_sav("Data/EDF_2018.sav")
edf2017 <- read_sav("Data/EDF_2017.sav")
edf2016 <- read_sav("Data/EDF_2016.sav")
edf2015 <- read_sav("Data/EDF_2015.sav")
edf2014 <- read_sav("Data/EDF_2014.sav")
edf2013 <- read_sav("Data/EDF_2013.sav")
edf2012 <- read_sav("Data/EDF_2012.sav")
edf2011 <- read_sav("Data/EDF_2011.sav")


##### Checking consistency in variables and variable's names across years

t2019 <- t(t(sapply(edf2019, class)))
t2018 <- t(t(sapply(edf2018, class)))
t2017 <- t(t(sapply(edf2017, class)))
t2016 <- t(t(sapply(edf2016, class)))
t2015 <- t(t(sapply(edf2015, class)))
t2014 <- t(t(sapply(edf2014, class)))
t2013 <- t(t(sapply(edf2013, class)))
t2012 <- t(t(sapply(edf2012, class)))
t2011 <- t(t(sapply(edf2011, class)))

t2019 <- as.data.frame(t2019)
t2018 <- as.data.frame(t2018)
t2017 <- as.data.frame(t2017)
t2016 <- as.data.frame(t2016)
t2015 <- as.data.frame(t2015)
t2014 <- as.data.frame(t2014)
t2013 <- as.data.frame(t2013)
t2012 <- as.data.frame(t2012)
t2011 <- as.data.frame(t2011)

######## Changing variables clasess for binding

edf2018$fecha_insc <- as.Date(edf2018$fecha_insc)

edf2017_2019 <- bind_rows(edf2017, edf2018, edf2019)
nrow(edf2019) + nrow(edf2018) + nrow(edf2017)


edf2015$fecha_fall <- as.Date(edf2015$fecha_fall)
edf2015_2016 <- bind_rows(edf2015, edf2016)
nrow(edf2016) + nrow(edf2015)

# works!

edf2017_2019 <- edf2017_2019 %>% 
  select(-cod_pais)
edf2015_2016 <- edf2015_2016 %>% 
  select(-cod_pais)

edf2015_2019 <- bind_rows(edf2015_2016, edf2017_2019)
# =) suma de las filas es consistente. 

edf2015_2019 <- edf2015_2019 %>% 
  select(-nom_pais)
edf2014 <- edf2014 %>% 
  select(-cod_pais)

edf2014_2019 <- bind_rows(edf2014, edf2015_2019)
# =), consistent row count. 

edf2013 <- edf2013 %>% 
  unite("fecha_insc", anio_insc, mes_insc, dia_insc, sep = "-", remove = FALSE) %>% 
  unite("fecha_fall", anio_fall, mes_fall, dia_fall, sep = "-", remove =FALSE) %>% 
  unite("fecha_mad", anio_mad, mes_mad, dia_mad, sep = "-", remove = FALSE) %>% 
  select(-cod_esta, -cod_pais)
   
edf2013$fecha_insc <- as.Date(edf2013$fecha_insc)
edf2013$fecha_fall <- as.Date(edf2013$fecha_fall)
edf2013$fecha_mad <- as.character(edf2013$fecha_mad)

edf2013_2019 <- bind_rows(edf2013, edf2014_2019)
