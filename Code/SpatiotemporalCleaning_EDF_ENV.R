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

edf2018 <- edf2018 %>% 
  within(niv_inst[niv_inst == 3] <- 2) %>% 
  within(niv_inst[niv_inst == 5] <- 3) %>%
  within(niv_inst[niv_inst == 6] <- 5) %>%
  within(niv_inst[niv_inst == 7] <- 6) %>% 
  within(niv_inst[niv_inst == 8] <- 7) %>% 
  within(niv_inst[niv_inst == 9] <- 8) %>% 
  within(niv_inst[niv_inst == 99] <- 9)
  
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
  select(-cod_esta, -cod_pais, -ofi_insc, -folio, -acta_insc)
   
edf2013$fecha_insc <- as.Date(edf2013$fecha_insc)
edf2013$fecha_fall <- as.Date(edf2013$fecha_fall)
edf2013$fecha_mad <- as.character(edf2013$fecha_mad)

edf2013_2019 <- bind_rows(edf2013, edf2014_2019)

# preparing 2012
#No hay dato del día  para fecha_insc, y para fecha_fall (le puse 15 a todos =)!
#Se pierden variables de fecha nacimiento madre. Creo que no importa porque está la edad. 
#NOTE 2012 and 2011 fech_insc and fecha_fall DAY is not accurate, all set to 15

edf2012 <- edf2012 %>% 
  mutate(col15 = 15) %>% 
  unite("fecha_insc", anio_insc, mes_insc, col15, sep = "-", remove = FALSE) %>% 
  unite("fecha_fall", anio_def, mes_def, col15, sep = "-", remove =FALSE) %>%
  rename(etnia = p_etnica, hij_viv = hij_vivos, anio_fall = anio_def, mes_fall = mes_def, causa_fetal = cau_10ar) %>% 
  select(-cod_pais, -ofi_insc, -acta_insc, -col15)

edf2012$fecha_insc <- as.Date(edf2012$fecha_insc)
edf2012$fecha_fall <- as.Date(edf2012$fecha_fall)

edf2012_2019 <- bind_rows(edf2012, edf2013_2019)
#SUma es consistente

#preparing 2011

edf2011 <- edf2011 %>% 
  within(lug_fall[lug_fall == "01"] <- "1") %>% 
  within(lug_fall[lug_fall == "02"] <- "2") %>% 
  within(lug_fall[lug_fall == "03"] <- "4") %>% 
  within(lug_fall[lug_fall == "04"] <- "5") %>% 
  within(lug_fall[lug_fall == "05"] <- "6") %>% 
  within(lug_fall[lug_fall == "06"] <- "7") %>% 
  within(sexo[sexo == "01"] <- "1") %>% 
  within(sexo[sexo == "02"] <- "2") %>% 
  within(sabe_leer[sabe_leer == "01"] <- "1") %>% 
  within(sabe_leer[sabe_leer == "02"] <- "2") %>% 
  within(sabe_leer[sabe_leer == "09"] <- "9")

edf2011$lug_fall <- as.numeric(as.character(edf2011$lug_fall))
edf2011$sexo <- as.numeric(as.character(edf2011$sexo))

edf2011 <- edf2011 %>% 
  select(-sem_gest)

edf2011 <- edf2011 %>% 
  mutate(col15 = 15) %>% 
  unite("fecha_insc", anio_insc, mes_insc, col15, sep = "-", remove = FALSE) %>% 
  unite("fecha_fall", anio_fall, mes_fall, col15, sep = "-", remove =FALSE) %>%
  rename(hij_viv = hij_vivos, sem_gest = sem_gest1, causa_fetal = causa, asis_por = asit_por1, lugar_ocur = lug_fall) %>%
  select(-region_res, -residente, -vcausa, -aten_medica, -asit_por, -t, -th, -tm, -act_insc, -col15)

edf2011$fecha_insc <- as.Date(edf2011$fecha_insc)
edf2011$fecha_fall <- as.Date(edf2011$fecha_fall)


edf2011$anio_insc <- as.numeric(edf2011$anio_insc)
edf2011$mes_insc <- as.numeric(as.character(edf2011$mes_insc))
edf2011$anio_fall <- as.numeric(as.character(edf2011$anio_fall))
edf2011$mes_fall <- as.numeric(as.character(edf2011$mes_fall))
edf2011$p_emb <- as.numeric(as.character(edf2011$p_emb))
edf2011$area_fall <- as.numeric(as.character(edf2011$area_fall))
edf2011$sabe_leer <- as.numeric(as.character(edf2011$sabe_leer))
edf2011$niv_inst <- as.numeric(as.character(edf2011$niv_inst))
edf2011$area_res <- as.numeric(as.character(edf2011$area_res))
edf2011$est_civil <- as.numeric(as.character(edf2011$est_civil))
edf2011$sem_gest <- as.numeric(as.character(edf2011$sem_gest))

## Variables no presentes: Num controles prenatales, etnia, dias fechas inscripcion y fallecimiento
# variables fecha nacimiento madre
edf2011_2019 <- bind_rows(edf2011, edf2012_2019)
