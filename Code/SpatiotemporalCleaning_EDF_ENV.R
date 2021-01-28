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
edf2010 <- read_sav("Data/EDF_2010.sav")
edf2009 <- read_sav("Data/EDF_2009.sav")
edf2008 <- read_sav("Data/EDF_2008.sav")
edf2007 <- read_sav("Data/EDF_2007.sav")
edf2006 <- read_sav("Data/EDF_2006.sav")
edf2005 <- read_sav("Data/EDF_2005.sav")
edf2004 <- read_sav("Data/EDF_2004.sav")
edf2003 <- read_sav("Data/EDF_2003.sav")
edf2002 <- read_sav("Data/EDF_2002.sav")
edf2001 <- read_sav("Data/EDF_2001.sav")
edf2000 <- read_sav("Data/EDF_2000.sav")

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
  dplyr::select(-cod_pais)
edf2015_2016 <- edf2015_2016 %>% 
  dplyr::select(-cod_pais)

edf2015_2019 <- bind_rows(edf2015_2016, edf2017_2019)
# =) suma de las filas es consistente. 

edf2015_2019 <- edf2015_2019 %>% 
  dplyr::select(-nom_pais)
edf2014 <- edf2014 %>% 
  dplyr::select(-cod_pais)

edf2014_2019 <- bind_rows(edf2014, edf2015_2019)
# =), consistent row count. 

edf2013 <- edf2013 %>% 
  unite("fecha_insc", anio_insc, mes_insc, dia_insc, sep = "-", remove = FALSE) %>% 
  unite("fecha_fall", anio_fall, mes_fall, dia_fall, sep = "-", remove =FALSE) %>% 
  unite("fecha_mad", anio_mad, mes_mad, dia_mad, sep = "-", remove = FALSE) %>% 
  dplyr::select(-cod_esta, -cod_pais, -ofi_insc, -folio, -acta_insc)
   
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
  dplyr::select(-cod_pais, -ofi_insc, -acta_insc, -col15)

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
  dplyr::select(-sem_gest)

edf2011 <- edf2011 %>% 
  mutate(col15 = 15) %>% 
  unite("fecha_insc", anio_insc, mes_insc, col15, sep = "-", remove = FALSE) %>% 
  unite("fecha_fall", anio_fall, mes_fall, col15, sep = "-", remove =FALSE) %>%
  rename(hij_viv = hij_vivos, sem_gest = sem_gest1, causa_fetal = causa, asis_por = asit_por1, lugar_ocur = lug_fall) %>%
  dplyr::select(-region_res, -residente, -vcausa, -aten_medica, -asit_por, -t, -th, -tm, -act_insc, -col15)

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

## Verificando si hay algún caso en los territorios que no son cantón
#9001, 9003, 9004
sum(edf2011_2019$cant_res == "9001"| edf2011_2019$cant_res == "9002" | edf2011_2019$cant_res == "9003")

### Solo anios fallecimiento en el rango (2011-2019)

edf2011_2019 <- edf2011_2019 %>%  
                filter(anio_fall > 2010)
  
  


##############################################################################
#NACIMIENTOS VIVOS
###############################################################################

env2019 <- read_sav("Data/ENV_2019.sav")
env2018 <- read_sav("Data/ENV_2018.sav")
env2017 <- read_sav("Data/ENV_2017.sav")
env2016 <- read_sav("Data/ENV_2016.sav")
env2015 <- read_sav("Data/ENV_2015.sav")
env2014 <- read_sav("Data/ENV_2014.sav")
env2013 <- read_sav("Data/ENV_2013.sav")
env2012 <- read_sav("Data/ENV_2012.sav")
env2011 <- read_sav("Data/ENV_2011.sav")
env2010 <- read_sav("Data/ENV_2010.sav")
env2009 <- read_sav("Data/ENV_2009.sav")
env2008 <- read_sav("Data/ENV_2008.sav")
env2007 <- read_sav("Data/ENV_2007.sav")
env2006 <- read_sav("Data/ENV_2006.sav")
env2005 <- read_sav("Data/ENV_2005.sav")
env2004 <- read_sav("Data/ENV_2004.sav")
env2003 <- read_sav("Data/ENV_2003.sav")
env2002 <- read_sav("Data/ENV_2002.sav")
env2001 <- read_sav("Data/ENV_2001.sav")
env2000 <- read_sav("Data/ENV_2000.sav")

##### Checking consistency in variables and variable's names across years

#tenv2019 <- t(t(sapply(env2019, class)))
#tenv2018 <- t(t(sapply(env2018, class)))
#tenv2017 <- t(t(sapply(env2017, class)))
#tenv2016 <- t(t(sapply(env2016, class)))
#tenv2015 <- t(t(sapply(env2015, class)))
#tenv2014 <- t(t(sapply(env2014, class)))
#tenv2013 <- t(t(sapply(env2013, class)))
#tenv2012 <- t(t(sapply(env2012, class)))
#tenv2011 <- t(t(sapply(env2011, class)))

#tenv2019 <- as.data.frame(tenv2019)
#tenv2018 <- as.data.frame(tenv2018)
#tenv2017 <- as.data.frame(tenv2017)
#tenv2016 <- as.data.frame(tenv2016)
#tenv2015 <- as.data.frame(tenv2015)
#tenv2014 <- as.data.frame(tenv2014)
#tenv2013 <- as.data.frame(tenv2013)
#tenv2012 <- as.data.frame(tenv2012)
#tenv2011 <- as.data.frame(tenv2011)

######## Changing variables clasess for binding 

env2019$fecha_nac <- as.Date(env2019$fecha_nac)
env2017$fecha_nac <- as.Date(env2017$fecha_nac)
env2017$cod_pais <- as.character(env2017$cod_pais)
env2014$fecha_insc <- as.character(env2014$fecha_insc)
env2013$fecha_insc <- as.character(env2013$fecha_insc)
env2013$fecha_mad <- as.character(env2013$fecha_mad)

env2018_2019 <- bind_rows(env2018, env2019)
env2017_2019 <- bind_rows(env2017, env2018_2019)
env2015_2019 <- bind_rows(env2015, env2016, env2017_2019)

env2014 <- env2014 %>% 
          dplyr::select(-tip_insc, - ofi_insc)  
env2013 <- env2013 %>% 
  dplyr::select(-folio, - ofi_insc, -acta_insc, -cod_esta)  

env2012 <- env2012 %>% 
  rename(etnia = p_etnica, hij_viv = hij_vivos) %>% 
  dplyr::select(- ofi_insc, -acta_insc, -aten_medica, -tipo_insc)



env2014_2019 <- bind_rows(env2014, env2015_2019)
env2013_2019 <- bind_rows(env2013, env2014_2019)
env2012_2019 <- bind_rows(env2012, env2013_2019)
sum(env2012_2019$anio_nac > 2011)

## Not including previous years data bases as covariates of interest not available for edf before 2012
#Filtering those who were born before the study period 
env2012_2019 <- env2012_2019 %>%
              filter(anio_nac > 2011)

hist(env2012_2019$peso)
sum(is.na(env2012_2019$peso))
sum(is.na(env2012_2019$num_emb))
sum(is.na(env2012_2019$cant_res))
sum(is.na(env2012_2019$con_pren))
sum(is.na(env2012_2019$etnia))
sum(is.na(env2012_2019$niv_inst))
sum(is.na(env2012_2019$est_civil))
sum(is.na(env2012_2019$sabe_leer))

table(env2012_2019$hij_vivm)
table(env2012_2019$hij_nacm)
table(env2012_2019$hij_viv)
table(env2012_2019$con_pren)
table(env2012_2019$area_res)

boxplot(env2012_2019$peso ~ env2012_2019$area_res)

env2012_2019Box <- env2012_2019 %>% 
  filter(peso > 99 & peso < 9999 & sem_gest <99)

boxplot(env2012_2019Box$peso ~ env2012_2019Box$area_res)
boxplot(env2012_2019Box$peso ~ env2012_2019Box$etnia)
plot(env2012_2019Box$peso, env2012_2019Box$sem_gest)
cor(env2012_2019Box$peso, env2012_2019Box$sem_gest)


##### Missing: checking NA (when the birth registered is from an earlier year it does not have the covariates) 
