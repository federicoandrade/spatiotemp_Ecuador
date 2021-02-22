library(haven)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(rgeos)
library(geosphere)
library(rgdal)
library(raster)

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
#edf2008 <- read_sav("Data/EDF_2008.sav")
#edf2007 <- read_sav("Data/EDF_2007.sav")
#edf2006 <- read_sav("Data/EDF_2006.sav")
#edf2005 <- read_sav("Data/EDF_2005.sav")
#edf2004 <- read_sav("Data/EDF_2004.sav")
#edf2003 <- read_sav("Data/EDF_2003.sav")
#edf2002 <- read_sav("Data/EDF_2002.sav")
#edf2001 <- read_sav("Data/EDF_2001.sav")
#edf2000 <- read_sav("Data/EDF_2000.sav")

##### Checking consistency in variables and variable's names across years

#t2019 <- t(t(sapply(edf2019, class)))
#t2018 <- t(t(sapply(edf2018, class)))
#t2017 <- t(t(sapply(edf2017, class)))
#t2016 <- t(t(sapply(edf2016, class)))
#t2015 <- t(t(sapply(edf2015, class)))
#t2014 <- t(t(sapply(edf2014, class)))
#t2013 <- t(t(sapply(edf2013, class)))
#t2012 <- t(t(sapply(edf2012, class)))
#t2011 <- t(t(sapply(edf2011, class)))
#t2010 <- t(t(sapply(edf2010, class)))
#t2009 <- t(t(sapply(edf2009, class)))



#t2019 <- as.data.frame(t2019)
#t2018 <- as.data.frame(t2018)
#t2017 <- as.data.frame(t2017)
#t2016 <- as.data.frame(t2016)
#t2015 <- as.data.frame(t2015)
#t2014 <- as.data.frame(t2014)
#t2013 <- as.data.frame(t2013)
#t2012 <- as.data.frame(t2012)
#t2011 <- as.data.frame(t2011)
#t2010 <- as.data.frame(t2010)
#t2009 <- as.data.frame(t2009)



######## Changing variables clases for binding

## Niv Inst in 2019 and 2018 categories need to change
# 2019 is not ok in the catalogue, but it is ok in the data so no changes needed. 
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

#### TO Change Lugar occur categories to fit 2010 (1	Establecimiento del Ministerio de Salud
#2	Establecimiento del IESS
#3	Otro Establecimiento del Estado
#4	Hospital, Clinica o Consultorio Particular
#5	Casa
#6	Otro
#9	Ignorado)


edf2011_2019 <- edf2011_2019 %>% 
mutate(lugar_ocur = as.numeric(lugar_ocur)) %>% 
  within(lugar_ocur[lugar_ocur == 3] <- 8) %>% 
  within(lugar_ocur[lugar_ocur == 4] <- 3) %>% 
  within(lugar_ocur[lugar_ocur == 5] <- 4) %>% 
  within(lugar_ocur[lugar_ocur == 6] <- 5) %>% 
  within(lugar_ocur[lugar_ocur == 7] <- 6) %>% 
  within(lugar_ocur[lugar_ocur == 8] <- 6)




edf2010 <- edf2010 %>%
  unite("cant_res", PROVINCIA_RESIDENCIA, CANTON_RESIDENCIA, sep = "", remove = FALSE) %>%
  unite("parr_res", PROVINCIA_RESIDENCIA, CANTON_RESIDENCIA, PARROQUIA_RESIDENCIA, sep = "", remove = FALSE) %>% 
  unite("cant_insc", COD_PROVINCIA, COD_CANTON, sep = "", remove = FALSE) %>% 
  unite("parr_insc", COD_PROVINCIA, COD_CANTON, COD_PARROQUIA, sep = "", remove = FALSE) %>%
  unite("cant_fall", PROVINCIA_OCURRENCIA, CANTON_OCURRENCIA, sep = "", remove = FALSE) %>% 
  unite("parr_fall", PROVINCIA_OCURRENCIA, CANTON_OCURRENCIA, PARROQUIA_OCURRENCIA, sep = "", remove = FALSE) %>% 
  rename(prov_insc = COD_PROVINCIA, anio_insc = COD_ANIO, mes_insc = MES_REGISTRO, 
         sexo = COD_SEXO, anio_fall = ANIO_OCURRENCIA, 
         mes_fall = MES_OCURRENCIA, sem_gest = SEM_GESTACION,
         lugar_ocur = COD_LUGAR_OCURRENCIA, asis_por = COD_ASI_POR, edad_mad = EDAD_MADRE,
         hij_viv = NUM_HIJOS, hij_vivm = HIJOS_NAC_VIVOS_MUERTOS, hij_nacm = HIJOS_NACIDOS_MUERTOS,
         sabe_leer = COD_LEER_ESCRIBIR, niv_inst = COD_INSTRUCCION, prov_res = PROVINCIA_RESIDENCIA, 
         prov_fall = PROVINCIA_OCURRENCIA,
         area_res = AREA_RESIDENCIA, causa_fetal = CAU_10AREV,
         p_emb = COD_TIP_NACIMIENTO) %>% 
  dplyr::select( -COD_APL_EST, -COD_SOL_INSCRIPCION, -ACT_INSCRIPCION, 
                 -COD_CANTON, -COD_PARROQUIA,  
                 -CANTON_RESIDENCIA, -PARROQUIA_RESIDENCIA,
                 -COD_ATENCION_PROFESIONAL, -COD_RESIDENTE, -CANTON_OCURRENCIA, 
                 -PARROQUIA_OCURRENCIA, -AREA_OCURRENCIA) %>% 
  mutate(anio_insc = as.numeric(anio_insc)) %>% 
  mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
  mutate(anio_fall = as.numeric(anio_fall)) %>% 
  mutate(mes_fall = as.numeric(as.character(mes_fall))) %>% 
  mutate(area_res = as.numeric(as.character(area_res)))




  edf2010_2019 <- bind_rows(edf2010, edf2011_2019)

############# TO DO 2009

  edf2009 <- edf2009 %>% 
    mutate(COD_PROVINCIA = as.character(COD_PROVINCIA)) %>% 
    mutate(COD_CANTON = as.character(COD_CANTON)) %>% 
    mutate(COD_PARROQUIA = as.character(COD_PARROQUIA)) %>% 
    mutate(PROVINCIA_OCURRENCIA = as.character(PROVINCIA_OCURRENCIA)) %>% 
    mutate(CANTON_OCURRENCIA = as.character(CANTON_OCURRENCIA)) %>% 
    mutate(PARROQUIA_OCURRENCIA = as.character(PARROQUIA_OCURRENCIA)) %>% 
    mutate(PROVINCIA_RESIDENCIA = as.character(PROVINCIA_RESIDENCIA)) %>% 
    mutate(CANTON_RESIDENCIA = as.character(CANTON_RESIDENCIA)) %>% 
    mutate(PARROQUIA_RESIDENCIA = as.character(PARROQUIA_RESIDENCIA)) %>% 
    mutate(COD_PROVINCIA = str_pad(COD_PROVINCIA, width = 2, pad = "0")) %>% 
    mutate(COD_CANTON = str_pad(COD_CANTON, width = 2, pad = "0"))  %>% 
    mutate(PROVINCIA_OCURRENCIA = str_pad(PROVINCIA_OCURRENCIA, width = 2, pad = "0")) %>% 
    mutate(CANTON_OCURRENCIA = str_pad(CANTON_OCURRENCIA, width = 2, pad = "0")) %>% 
    mutate(PROVINCIA_RESIDENCIA = str_pad(PROVINCIA_OCURRENCIA, width = 2, pad = "0")) %>% 
    mutate(CANTON_RESIDENCIA = str_pad(CANTON_OCURRENCIA, width = 2, pad = "0"))
    
  
  
  
  
  edf2009 <- edf2009 %>%
    unite("cant_res", PROVINCIA_RESIDENCIA, CANTON_RESIDENCIA, sep = "", remove = FALSE) %>%
    unite("parr_res", PROVINCIA_RESIDENCIA, CANTON_RESIDENCIA, PARROQUIA_RESIDENCIA, sep = "", remove = FALSE) %>% 
    unite("cant_insc", COD_PROVINCIA, COD_CANTON, sep = "", remove = FALSE) %>% 
    unite("parr_insc", COD_PROVINCIA, COD_CANTON, COD_PARROQUIA, sep = "", remove = FALSE) %>%
    unite("cant_fall", PROVINCIA_OCURRENCIA, CANTON_OCURRENCIA, sep = "", remove = FALSE) %>% 
    unite("parr_fall", PROVINCIA_OCURRENCIA, CANTON_OCURRENCIA, PARROQUIA_OCURRENCIA, sep = "", remove = FALSE) %>% 
    rename(prov_insc = COD_PROVINCIA, anio_insc = COD_ANIO, mes_insc = MES_REGISTRO, 
           sexo = COD_SEXO, anio_fall = ANIO_OCURRENCIA, 
           mes_fall = MES_OCURRENCIA, sem_gest = SEM_GESTACION,
           lugar_ocur = COD_LUGAR_OCURRENCIA, asis_por = COD_ASI_POR, edad_mad = EDAD_MADRE,
           hij_viv = NUM_HIJOS, hij_vivm = HIJOS_NAC_VIVOS_MUERTOS, hij_nacm = HIJOS_NACIDOS_MUERTOS,
           sabe_leer = COD_LEER_ESCRIBIR, niv_inst = COD_INSTRUCCION, prov_res = PROVINCIA_RESIDENCIA, 
           prov_fall = PROVINCIA_OCURRENCIA,
           area_res = AREA_RESIDENCIA, causa_fetal = CAU_10AREV,
           p_emb = COD_TIP_NACIMIENTO) %>% 
    dplyr::select( -COD_APL_EST, -COD_SOL_INSCRIPCION, -ACT_INSCRIPCION, 
                   -COD_CANTON, -COD_PARROQUIA,  
                   -CANTON_RESIDENCIA, -PARROQUIA_RESIDENCIA,
                   -COD_ATENCION_PROFESIONAL, -COD_RESIDENTE, -CANTON_OCURRENCIA, 
                   -PARROQUIA_OCURRENCIA, -AREA_OCURRENCIA) %>% 
    mutate(anio_insc = as.numeric(anio_insc)) %>% 
    mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
    mutate(anio_fall = as.numeric(anio_fall)) %>% 
    mutate(mes_fall = as.numeric(as.character(mes_fall))) %>% 
    mutate(area_res = as.numeric(as.character(area_res))) %>% 
    mutate(prov_insc = as.character(prov_insc)) %>% 
    mutate(prov_fall = as.character(prov_fall)) %>% 
    mutate(prov_res = as.character(prov_res))


edf2009_2019 <- bind_rows(edf2009, edf2010_2019)

nrow(edf2010_2019) + nrow(edf2009)
nrow(edf2009_2019)
  
  
## Verificando si hay algún caso en los territorios que no son cantón
#9001, 9003, 9004
sum(edf2009_2019$cant_res == "9001"| edf2009_2019$cant_res == "9002" | edf2009_2019$cant_res == "9003")



####################################################################################
#####FILTERING 2009-2019 ONLY National residentes for the LBW clusters, and the gestational age (Separately)
## 1.Excluding: 
#    - Excluding years before the the time range of interest.
##   - international residence (making cantones consistent)
##   - Singleton, only one baby
#    - tipo parto? solo los que no son cesarea?
#    - Numero embarazos? menosr que 15? Numero hijos antes? Pregunta para grupo. 
#    
# 1.b.Create extra variables: 
#     -Create Binary variables with outcomes.
#     - 
## 2. Outcome: fetal death (I could later separate from 28 weeks and below)
##  
# 3. Excluding rows with invalid main explanatory variable (cant_res) 
# and cant_res international
# 4. Excluding rows with invalid covariates
# 5. filtering out unused variables




#1.Solo anios fallecimiento en el rango (2009-2019)

edf2009_2019 <- edf2009_2019 %>%  
  filter(anio_fall > 2008)

#1. Only residents of ecuador 
#Renaming CANTON 8800 (Exterior)
#Haciendo consistente La concordia, antes 0808 ahora 2302 provincia 23

sum(edf2009_2019$cant_res == "8800")
sum(edf2009_2019$cant_res == "0808")

edf2009_2019 <- edf2009_2019 %>% subset(cant_res != "8800") %>% 
  mutate(prov_insc = replace(prov_insc, cant_insc == "0808", "23")) %>% 
  mutate(prov_fall = replace(prov_fall, cant_fall == "0808", "23")) %>% 
  mutate(prov_res = replace(prov_res, cant_res == "0808", "23")) %>%
  within(cant_insc[cant_insc == "0808"] <- "2302") %>%
  within(cant_fall[cant_fall == "0808"] <- "2302") %>% 
  within(cant_res[cant_res == "0808"] <- "2302") %>% 
  within(parr_insc[parr_insc == "080850"] <- "230250") %>% 
  within(parr_insc[parr_insc == "080851"] <- "230251") %>% 
  within(parr_insc[parr_insc == "080852"] <- "230252") %>% 
  within(parr_insc[parr_insc == "080853"] <- "230253") %>% 
  within(parr_fall[parr_fall == "080850"] <- "230250") %>% 
  within(parr_fall[parr_fall == "080851"] <- "230251") %>% 
  within(parr_fall[parr_fall == "080852"] <- "230252") %>% 
  within(parr_fall[parr_fall == "080853"] <- "230253") %>% 
  within(parr_res[parr_res == "080850"] <- "230250") %>% 
  within(parr_res[parr_res == "080851"] <- "230251") %>% 
  within(parr_res[parr_res == "080852"] <- "230252") %>% 
  within(parr_res[parr_res == "080853"] <- "230253") %>% 
  mutate(niv_inst = as.factor(niv_inst))

####Adding location
#Including cantones location variables INFO
cantones_shp <- readOGR("Data/Cantones2014_INEC", "nxcantones")


#plot(cantones_shp)
centroidCant <- gCentroid(cantones_shp, byid = TRUE)

SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 

CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_dfext <- CantCentr_df %>% dplyr::select(DPA_CANTON, DPA_DESCAN, DPA_DESPRO, x, y) 
CantCentr_df <- CantCentr_df %>% dplyr::select(DPA_CANTON, DPA_DESCAN, x, y) 

save(CantCentr_df, file = "Data/EcuadorCantCent.RData")

class(CantCentr_df$x)
class(CantCentr_df$y)

edf2009_2019 <- edf2009_2019  %>% left_join(y = CantCentr_dfext, by = c("cant_res" = "DPA_CANTON")) %>% 
  rename(CordXres = x, CordYres = y, Nom_CantRes = DPA_DESCAN, Nom_ProvRes = DPA_DESPRO)


#2. All in the data base are fetal death.... could exclude stillbirth later


#3. Exploring cant_res
sum(is.na(edf2009_2019$cant_res))


# 4. Niv_inst
table(edf2009_2019$niv_inst)
sum(is.na(edf2009_2019$niv_inst))

edf2009_2019 %>% 
  group_by(niv_inst) %>%
  summarise(n=n())

edf2009_2019 <- edf2009_2019 %>% 
  filter(!is.na(niv_inst)) %>% 
  subset(niv_inst != "9")

# Edad de la madre (maternal age)

sum(is.na(edf2009_2019$edad_mad))

edf2009_2019 <- edf2009_2019 %>% 
  subset(edad_mad != 99)

## Creating Covariates categorical variables Age and Education.
# 3 categories for age, 4 categories for niv_inst

edf2009_2019 <- edf2009_2019 %>% 
  mutate(edad_cat = as.factor(case_when(edad_mad < 20 ~ "1", 
                                        edad_mad >= 20 & edad_mad < 35 ~ "2",
                                        edad_mad >= 35 ~ "3"))) %>% 
  mutate(niv_inst_cat_st = as.factor(case_when(niv_inst == 0 | niv_inst == 1 ~ "1", 
                                            niv_inst == 2 |niv_inst == 3 |niv_inst == 4  ~ "2",
                                            niv_inst == 5 |niv_inst == 6  ~ "3",
                                            niv_inst == 7 | niv_inst == 8 ~ "4",
                                            niv_inst == 9 ~ "9")))

edf2009_2019case <- edf2009_2019 

##################################################
####### Saving files for preparing for SATSCAN
##################################################

save(edf2009_2019case, file = "Data/edf2009_2019case.RData")
#save(env2009_2019_LBW, file = "Data/env2009_2019_LBW.RData")
#save(env2009_2019_GES, file = "Data/env2009_2019_GES.RData")

