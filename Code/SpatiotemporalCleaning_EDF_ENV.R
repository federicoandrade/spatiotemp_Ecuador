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
#env2007 <- read_sav("Data/ENV_2007.sav")
#env2006 <- read_sav("Data/ENV_2006.sav")
#env2005 <- read_sav("Data/ENV_2005.sav")
#env2004 <- read_sav("Data/ENV_2004.sav")
#env2003 <- read_sav("Data/ENV_2003.sav")
#env2002 <- read_sav("Data/ENV_2002.sav")
#env2001 <- read_sav("Data/ENV_2001.sav")
#env2000 <- read_sav("Data/ENV_2000.sav")

##### Checking consistency in variables and variable's names across years


tenv2019 <- t(t(sapply(env2019, class)))
tenv2018 <- t(t(sapply(env2018, class)))
tenv2017 <- t(t(sapply(env2017, class)))
tenv2016 <- t(t(sapply(env2016, class)))
tenv2015 <- t(t(sapply(env2015, class)))
tenv2014 <- t(t(sapply(env2014, class)))
tenv2013 <- t(t(sapply(env2013, class)))
tenv2012 <- t(t(sapply(env2012, class)))
tenv2011 <- t(t(sapply(env2011, class)))
tenv2010 <- t(t(sapply(env2010, class)))
tenv2009 <- t(t(sapply(env2009, class)))
#tenv2008 <- t(t(sapply(env2008, class)))

tenv2019 <- as.data.frame(tenv2019)
tenv2018 <- as.data.frame(tenv2018)
tenv2017 <- as.data.frame(tenv2017)
tenv2016 <- as.data.frame(tenv2016)
tenv2015 <- as.data.frame(tenv2015)
tenv2014 <- as.data.frame(tenv2014)
tenv2013 <- as.data.frame(tenv2013)
tenv2012 <- as.data.frame(tenv2012)
tenv2011 <- as.data.frame(tenv2011)
tenv2010 <- as.data.frame(tenv2010)
tenv2009 <- as.data.frame(tenv2009)
#tenv2008 <- as.data.frame(tenv2008)


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

env2011 <- env2011 %>% 
  rename(etnia = p_etnica, hij_viv = hij_vivos) %>% 
  dplyr::select(- ofi_insc, -acta_insc, -aten_medica, -tipo_insc, 
                - fac_rh, -nac_pad, - edad_pad, -tipo_san) %>% 
  mutate(anio_insc = as.numeric(anio_insc)) %>% 
  mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
  mutate(sexo = as.numeric(as.character(sexo))) %>% 
  mutate(tipo_part = as.numeric(as.character(tipo_part))) %>% 
  mutate(anio_nac = as.numeric(anio_nac)) %>% 
  mutate(mes_nac = as.numeric(as.character(mes_nac))) %>% 
  mutate(p_emb = as.numeric(as.character(p_emb))) %>% 
  mutate(lugar_ocur = as.numeric(as.character(lugar_ocur))) %>% 
  mutate(asis_por = as.numeric(as.character(asis_por))) %>% 
  mutate(area_nac = as.numeric(as.character(area_nac))) %>% 
  mutate(nac_mad = as.numeric(as.character(nac_mad))) %>% 
  mutate(etnia = as.numeric(as.character(etnia))) %>% 
  mutate(est_civil = as.numeric(as.character(est_civil))) %>% 
  mutate(sabe_leer = as.numeric(as.character(sabe_leer))) %>% 
  mutate(niv_inst = as.numeric(as.character(niv_inst))) %>% 
  mutate(area_res = as.numeric(as.character(area_res))) %>% 
  mutate(residente = as.numeric(as.character(residente)))


env2010 <- env2010 %>%
  unite("cant_res", PRO_RES, CAN_RES, sep = "", remove = FALSE) %>%
  unite("parr_res", PRO_RES, CAN_RES, PAR_RES, sep = "", remove = FALSE) %>% 
  unite("cant_insc", PROV_INS, CAN_INS, sep = "", remove = FALSE) %>% 
  unite("parr_insc", PROV_INS, CAN_INS, PAR_INS, sep = "", remove = FALSE) %>%
  rename(prov_insc = PROV_INS, anio_insc = ANIO_INS, mes_insc = MES_INS, 
         sexo = SEXO, talla = TALLA, peso = PESO, anio_nac = ANIO_NAC, 
         mes_nac = MES_NAC, sem_gest = SEM_GEST,
         lugar_ocur = LUGAR_OCUR, asis_por = ASIS_POR, prov_nac = PRO_NAC, 
         cant_nac = CAN_NAC, parr_nac = PAR_NAC, area_nac = AREA_NAC, edad_mad = EDAD_MAD,
         hij_viv = HIJ_VIVOS, hij_vivm = HIJ_VIVM, hij_nacm = HIJ_NACM, etnia = P_ETNICA,
         est_civil = EST_CIVIL, sabe_leer = SABE_LEER, niv_inst = NIV_INST, prov_res = PRO_RES,
         area_res = AREA_RES, residente = RESIDENTE,
         p_emb = TIPO_NAC) %>% 
  dplyr::select( -CAN_INS, -PAR_INS, -CAN_RES, -PAR_RES,-ACTA_INS, -ATEN_MEDICA, -OFICINA, -REG_INS, -ENCUESTA) %>% 
  mutate(anio_insc = as.numeric(anio_insc)) %>% 
  mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
  mutate(sexo = as.numeric(as.character(sexo))) %>% 
  mutate(anio_nac = as.numeric(anio_nac)) %>% 
  mutate(mes_nac = as.numeric(as.character(mes_nac))) %>% 
  mutate(p_emb = as.numeric(as.character(p_emb))) %>% 
  mutate(lugar_ocur = as.numeric(as.character(lugar_ocur))) %>% 
  mutate(asis_por = as.numeric(as.character(asis_por))) %>% 
  mutate(area_nac = as.numeric(as.character(area_nac))) %>% 
  mutate(etnia = as.numeric(as.character(etnia))) %>% 
  mutate(est_civil = as.numeric(as.character(est_civil))) %>% 
  mutate(sabe_leer = as.numeric(as.character(sabe_leer))) %>% 
  mutate(niv_inst = as.numeric(as.character(niv_inst))) %>% 
  mutate(area_res = as.numeric(as.character(area_res))) %>% 
  mutate(residente = as.numeric(as.character(residente))) %>% 
  within(etnia[etnia == "0"] <- "9") %>% 
  within(lugar_ocur[lugar_ocur == "0"] <- "9")
  

env2009 <- env2009 %>% 
  unite("cant_res", PRO_RES, CAN_RES, sep = "", remove = FALSE) %>%
  unite("parr_res", PRO_RES, CAN_RES, PAR_RES, sep = "", remove = FALSE) %>% 
  unite("cant_insc", PROV_INS, CAN_INSC, sep = "", remove = FALSE) %>% 
  unite("parr_insc", PROV_INS, CAN_INSC, PAR_INSC, sep = "", remove = FALSE) %>%
  rename(prov_insc = PROV_INS, anio_insc = ANIO_INS, mes_insc = MES_INSC, 
         sexo = SEXO, talla = TALLA, peso = PESO, anio_nac = ANIO_NAC, 
         mes_nac = MES_NAC, sem_gest = SEM_GEST,
         lugar_ocur = LUG_ACAE, asis_por = ASIS_POR, prov_nac = PRO_NAC, 
         cant_nac = CAN_NAC, parr_nac = PAR_NAC, area_nac = AREA_NAC, edad_mad = EDAD_MAD,
         hij_viv = HIJOSVIV, hij_vivm = HVIVMUE, hij_nacm = HNACMUE, etnia = P_ETNICA,
         est_civil = EST_CIV, sabe_leer = LEE, niv_inst = NIV_INST, prov_res = PRO_RES,
         area_res = AREA_RES, residente = RESIDENT,
         p_emb = TIPO_PAR) %>% 
  dplyr::select(-CAN_INSC, -PAR_INSC, -PAR_RES, -CAN_RES, -ACTA_INS, -ATEN_MED, -OFI_INSC, -REG_INSC, -ENCUESTA, -DIAM_CEF) %>% 
  mutate(anio_insc = as.numeric(anio_insc)) %>% 
  mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
  mutate(sexo = as.numeric(as.character(sexo))) %>% 
  mutate(anio_nac = as.numeric(anio_nac)) %>% 
  mutate(mes_nac = as.numeric(as.character(mes_nac))) %>% 
  mutate(p_emb = as.numeric(as.character(p_emb))) %>% 
  mutate(lugar_ocur = as.numeric(as.character(lugar_ocur))) %>% 
  mutate(asis_por = as.numeric(as.character(asis_por))) %>% 
  mutate(area_nac = as.numeric(as.character(area_nac))) %>% 
  mutate(etnia = as.numeric(as.character(etnia))) %>% 
  mutate(est_civil = as.numeric(as.character(est_civil))) %>% 
  mutate(sabe_leer = as.numeric(as.character(sabe_leer))) %>% 
  mutate(niv_inst = as.numeric(as.character(niv_inst))) %>% 
  mutate(area_res = as.numeric(as.character(area_res))) %>% 
  mutate(residente = as.numeric(as.character(residente))) %>% 
  within(etnia[etnia == "0"] <- "9") %>% 
  within(lugar_ocur[lugar_ocur == "0"] <- "9")

#env2008 <- env2008 %>% 
 # rename(prov_insc = PROV_INS, cant_insc = CAN_INSC, parr_insc = PAR_INSC,
  #       anio_insc = ANIO_INS, mes_insc = MES_INSC, sexo = SEXO, talla = TALLA,
   #      peso = PESO, anio_nac = ANIO_NAC, mes_nac = MES_NACI, sem_gest = SEM_GEST,
    #     lugar_ocur = LUG_ACAE, asis_por = ASIS_POR, prov_nac = PRO_NAC, 
     #    cant_nac = CAN_NAC, parr_nac = PAR_NAC, area_nac = AREA_NAC, edad_mad = EDAD_MAD,
      #   hij_viv = HIJOSVIV, hij_vivm = HVIVMUE, hij_nacm = HNACMUE,
       #  sabe_leer = LEE, niv_inst = NIV_INST, prov_res = PRO_RES,
        # cant_res = CAN_RES, parr_res = PAR_RES, area_res = AREA_RES, residente = RESIDENT,
         #p_emb = TIPO_PAR) %>% 
#  dplyr::select(-ESTADIST, -REG_INSC, -ACTA_INS, -OFI_INSC, -APGAR1, -APGAR2, -ATEN_MED, -DIAM_CEF, -INSCRIBE) %>% 
 # mutate(anio_insc = as.numeric(anio_insc)) %>% 
  #mutate(mes_insc = as.numeric(as.character(mes_insc))) %>% 
  #mutate(sexo = as.numeric(as.character(sexo))) %>% 
  #mutate(anio_nac = as.numeric(anio_nac)) %>% 
  #mutate(mes_nac = as.numeric(as.character(mes_nac))) %>% 
  #mutate(p_emb = as.numeric(as.character(p_emb))) %>% 
  #mutate(lugar_ocur = as.numeric(as.character(lugar_ocur))) %>% 
  #mutate(asis_por = as.numeric(as.character(asis_por))) %>% 
  #mutate(area_nac = as.numeric(as.character(area_nac))) %>% 
  #mutate(sabe_leer = as.numeric(as.character(sabe_leer))) %>% 
  #mutate(niv_inst = as.numeric(as.character(niv_inst))) %>% 
  #mutate(area_res = as.numeric(as.character(area_res))) %>% 
  #mutate(residente = as.numeric(as.character(residente)))


env2014_2019 <- bind_rows(env2014, env2015_2019)
env2013_2019 <- bind_rows(env2013, env2014_2019)
env2012_2019 <- bind_rows(env2012, env2013_2019)
env2011_2019 <- bind_rows(env2011, env2012_2019)

env2011_2019 <- env2011_2019 %>% 
  within(etnia[etnia == "3"] <- "2") %>% 
  within(etnia[etnia == "4"] <- "2") %>% 
  within(etnia[etnia == "5"] <- "3") %>% 
  within(etnia[etnia == "6"] <- "3") %>% 
  within(etnia[etnia == "7"] <- "4") %>% 
  within(etnia[etnia == "8"] <- "4") %>% 
  mutate(lugar_ocur = as.numeric(lugar_ocur)) %>% 
  within(lugar_ocur[lugar_ocur == "3"] <- "8") %>% 
  within(lugar_ocur[lugar_ocur == "4"] <- "3") %>% 
  within(lugar_ocur[lugar_ocur == "5"] <- "4") %>% 
  within(lugar_ocur[lugar_ocur == "6"] <- "5") %>% 
  within(lugar_ocur[lugar_ocur == "7"] <- "6") %>% 
  within(lugar_ocur[lugar_ocur == "8"] <- "6")



env2010_2019 <- bind_rows(env2010, env2011_2019)
env2009_2019 <- bind_rows(env2009, env2010_2019)

##### Exploring CAntones classification
#env2011$cant_res <- as.factor(env2011$cant_res)
#levels(env2011$cant_res)

#env2010$cant_res <- as.factor(env2010$cant_res)
#levels(env2010$cant_res)

#env2009$cant_res <- as.factor(env2009$cant_res)
#levels(env2009$cant_res)





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
## 2. Excluding rows with Invalid outcome (peso 99 nd 9999, sem gest 99)
##  
# 3. Excluding rows with invalid main explanatory variable (cant_res) 
# and cant_res international
# 4. Excluding rows with invalid covariates
# 5. filtering out unused variables


env2009_2019 <- bind_rows(env2009, env2010_2019)
nrow(env2009_2019)

#1.

# Filter Time range of interest
env2009_2019 <- env2009_2019 %>%
  filter(anio_nac >= 2009)
nrow(env2009_2019)

#Renaming CANTON 8800 (Exterior)
#Haciendo consistente La concordia, antes 0808 ahora 2302 provincia 23

sum(env2009_2019$cant_res == "8800")
sum(env2009_2019$cant_res == "0808")
# NO missing, son "8800" que significa EXTERIOR. Puedo quitarlos =)

env2009_2019 <- env2009_2019 %>% subset(cant_res != "8800") %>% 
  mutate(prov_insc = replace(prov_insc, cant_insc == "0808", "23")) %>% 
  mutate(prov_res = replace(prov_res, cant_res == "0808", "23")) %>%
  within(cant_insc[cant_insc == "0808"] <- "2302") %>% 
  within(cant_res[cant_res == "0808"] <- "2302") 

nrow(env2009_2019)

#Only Singleton
env2009_2019 <- env2009_2019 %>%
  filter(p_emb == 1)
nrow(env2009_2019)

#Only cesarea? Not sure how good the data quality is.
# WHO recomendations is 10-15% ....At pop level Above 10% there is no benefit of cesarea on morality...
table(env2009_2019$tipo_part)


# Creating Categorical variables for SatScan (does not take numeric variables), and collapsing levels
# - Create -> Preterm, Lbw, Parto previo, age cat
# - Re categorize -> Etnia, nivel educacion

env2009_2019 <- env2009_2019%>% 
  mutate(preterm = as.factor(case_when(sem_gest >= 37 ~ "0",
                                       sem_gest < 37 ~ "1")))  %>% 
  mutate(edad_cat = as.factor(case_when(edad_mad < 20 ~ "1", 
                                        edad_mad >= 20 & edad_mad < 35 ~ "2",
                                        edad_mad >= 35 ~ "3"))) %>% 
  mutate(niv_inst_cat = as.factor(case_when(niv_inst == 0 | niv_inst == 1 | niv_inst == 2 |niv_inst == 3| niv_inst == 4 ~ "1", 
                                            niv_inst == 5 |niv_inst == 6 |niv_inst == 7 |niv_inst == 8 ~ "2",
                                            niv_inst == 9 ~ "9"))) %>% 
  mutate(par_previo = as.factor(case_when(num_par == 1  ~ "0", 
                                          num_par > 1 & num_par < 99  ~ "1"))) %>% 
  mutate(etnia_cat = as.factor(case_when(etnia == "1" | etnia == "2" | etnia == "4" ~ "1", 
                                            etnia == "3"  ~ "2",
                                            etnia == "9" ~ "9")))



#Including cantones location variables INFO
cantones_shp <- readOGR("Data/Cantones2014_INEC", "nxcantones")
cantonesLatLong_shp <- readOGR("Data/Cantones2014_INECLatLong", "cantonesloglat")



#plot(cantones_shp)
centroidCant <- gCentroid(cantones_shp, byid = TRUE)
centroidCantLatLong <- gCentroid(cantonesLatLong_shp, byid = TRUE)


SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 

SpDF_centroidLatLong <- SpatialPointsDataFrame(centroidCantLatLong, cantonesLatLong_shp@data,
                                               proj4string = CRS("+init=epsg:4326"))
SpDF_centroidLatLong


CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_df <- CantCentr_df %>% dplyr::select(DPA_CANTON, DPA_DESCAN, x, y) 
CantCentr_dfext <- CantCentr_df %>% dplyr::select(DPA_CANTON, DPA_DESCAN, DPA_DESPRO, x, y) 


save(CantCentr_df, file = "Data/EcuadorCantCent.RData")

class(CantCentr_df$x)
class(CantCentr_df$y)


#Trying Lat Long, did not work
CantCentrLatLong_df <- as.data.frame(SpDF_centroidLatLong)





env2009_2019 <- env2009_2019  %>% left_join(y = CantCentr_dfext, by = c("cant_res" = "DPA_CANTON")) %>% 
  rename(CordXres = x, CordYres = y, Nom_CantRes = DPA_DESCAN, Nom_ProvRes = DPA_DESPRO)




#2 and 2a
# 2500 cut off is based on 10th percentile from peso. 
# 37 is based on  literature.
#"with extreme or implausible gestational ages (<20 weeks or >45 weeks)" (Ling, 2018)
# "or birthweights (<500 g or >6800 g)" (Ling, 2018)
# Variable par_previo SI = 1, NO = 0

env2009_2019_LBW <- env2009_2019 %>% 
  filter(peso > 499 & peso < 6800)  %>% 
  mutate(lbw = as.factor(case_when(peso >=2500 ~ "0",
                                   peso < 2500 ~ "1"))) 

nrow(env2009_2019_LBW)

env2009_2019_GES <- env2009_2019 %>% 
  filter(sem_gest <45 | sem_gest > 20) %>% 
  mutate(lbw = as.factor(case_when(peso >=2500 ~ "0",
                                   peso < 2500 ~ "1"))) 

nrow(env2009_2019_GES)

#3
sum(is.na(env2009_2019_GES$cant_res))
sum(is.na(env2009_2019_LBW$cant_res))

# 4.Covariates
tenv2009_2019 <- t(t(sapply(env2009_2019, class)))

tenv2009_2019 <- as.data.frame(tenv2009_2019)

# age
sum(is.na(env2009_2019_GES$edad_mad))
sum(is.na(env2009_2019_LBW$edad_mad))


#etnia
sum(is.na(env2009_2019_GES$etnia))
sum(is.na(env2009_2019_LBW$etnia))


#nivel educacion
sum(is.na(env2009_2019_GES$niv_inst))
sum(is.na(env2009_2019_LBW$niv_inst))

env2009_2019_LBW <- env2009_2019_LBW %>% 
  filter(!is.na(niv_inst))

env2009_2019_GES <- env2009_2019_GES %>% 
  filter(!is.na(niv_inst))

#Numero de partos previos (Data Only from 2013. To consider for the spatial model)
summary(env2009_2019_GES$num_par)
table(env2009_2019_GES$num_par)

summary(env2009_2019_LBW$num_par)
table(env2009_2019_LBW$num_par)

sum(is.na(env2009_2019_GES$num_par))
sum(is.na(env2009_2019_LBW$num_par))


##################################################
####### Saving files for preparing for SATSCAN
##################################################

#save(env2009_2019, file = "Data/env2009_2019.RData")
#save(env2009_2019_LBW, file = "Data/env2009_2019_LBW.RData")
#save(env2009_2019_GES, file = "Data/env2009_2019_GES.RData")

#############################
## Graphs summary statistics 
#############################

#Mean peso pear year


hist(env2009_2019_LBW$peso)


env2009_2019_LBW %>% 
  mutate(anio_nac = as.factor(anio_nac)) %>% 
  mutate(peso = as.numeric(peso)) %>% 
  ggplot(aes(x = anio_nac, y = peso)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .3)  +
  stat_summary(fun = "mean", geom = "point", colour = "orange") +
  theme_bw(labs(title = "Birth Weigth vs Year of Birth")) +
  xlab("Year of birth") +
  ylab("Birth Weight (g)")  


env2009_2019_LBW %>% 
  mutate(Nom_ProvRes = as.factor(Nom_ProvRes)) %>% 
  mutate(peso = as.numeric(peso)) %>% 
  group_by(Nom_ProvRes) %>% 
  ggplot(aes(x = reorder(Nom_ProvRes, peso), y = peso)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .3)  +
  stat_summary(fun = "mean", geom = "point", colour = "orange") +
  theme_bw() +
  xlab("Province of Residence") +
  ylab("Birth Weight (g)") + 
  theme(axis.text.y = element_text(size = 7), axis.title = element_text(size =10), 
        labs(title = ""))  +
  coord_flip()

env2009_2019_LBW %>% 
  mutate(Nom_ProvRes = as.factor(Nom_ProvRes)) %>% 
  mutate(Nom_CantRes = as.factor(Nom_CantRes)) %>% 
  mutate(peso = as.numeric(peso)) %>% 
  ggplot(aes(x = reorder(Nom_CantRes, peso), y = peso)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .3)  +
  stat_summary(fun = "mean", geom = "point", colour = "orange") +
  theme_bw() +
  xlab("Cantones (n = 224)") +
  ylab("Birth Weight (g)") + 
  theme(axis.text.y = element_blank(), axis.title.x = element_text(size =10)) +
  coord_flip()

env2009_2019_LBW %>% 
  mutate(Nom_ProvRes = as.factor(Nom_ProvRes)) %>% 
  mutate(Nom_CantRes = as.factor(Nom_CantRes)) %>% 
  mutate(peso = as.numeric(peso)) %>% 
  filter(Nom_ProvRes == "AZUAY") %>% 
  ggplot(aes(x = reorder(Nom_CantRes, peso), y = peso)) +
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = .3)  +
  stat_summary(fun = "mean", geom = "point", colour = "orange") +
  theme_bw() +
  xlab("Cantones (n = 224)") +
  ylab("Birth Weight (g)") + 
  theme(axis.text.y = element_text(size = 7), axis.title.x = element_text(size =10)) +
  coord_flip()



##Initial exploration outocome variable (peso) 
hist(env2009_2019$peso)
sum(is.na(env2009_2019$peso))
sum(is.na(env2009_2019$num_emb))
sum(is.na(env2009_2019$cant_res))
sum(is.na(env2009_2019$con_pren))
sum(is.na(env2009_2019$etnia))
sum(is.na(env2009_2019$niv_inst))
sum(is.na(env2009_2019$est_civil))
sum(is.na(env2009_2019$sabe_leer))

table(env2009_2019$hij_vivm)
table(env2009_2019$hij_nacm)
table(env2009_2019$hij_viv)
table(env2009_2019$con_pren)
table(env2009_2019$area_res)



#boxplot(env2009_2019$peso ~ env2012_2019$area_res)

#boxplot(env2009_2019$peso ~ env2009_2019$area_res)
#boxplot(env2009_2019$peso ~ env2009_2019$etnia)
#plot(env2009_2019$peso, env2009_2019$sem_gest)
#cor(env2009_2019$peso, env2009_2019$sem_gest)

#lm_peso_etnia <- lm(env2009_2019$peso ~ env2009_2019$etnia)
#summary(lm_peso_etnia)

#lm_peso_nivinst <- lm(env2009_2019$peso ~ env2009_2019$niv_inst)
#summary(lm_peso_nivinst)

#lm_peso_edad <- lm(env2009_2019$peso ~ env2009_2019$edad_mad)
#summary(lm_peso_edad)

#quantile(env2009_2019$peso, prob = c(0.1, 0.25, 0.5, 0.75, 0.90))



