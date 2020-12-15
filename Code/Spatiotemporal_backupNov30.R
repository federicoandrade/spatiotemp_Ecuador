library(haven)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(rgeos)
library(geosphere)
library(rgdal)
library(raster)
library(ggplot2)

##############################################################################
#Spatiotemporal - EGRESOS 2011-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################

##
# Importing and exploring data EGRESOS
###

egresos2010 <- read_sav("Data/Egresos_hospitalarios2010.sav")
glimpse(egresos2010)
str(egresos2010)


egresos2011 <- read_sav("Data/Egresos_hospitalarios2011.sav")
glimpse(egresos2011)
str(egresos2011)

defunciones2001 <- read_sav("Data/Defunciones_2001.sav")

eg2011 <- read_sav("Data/Egresos_hospitalarios2011.sav")
eg2012 <- read_sav("Data/Egresos_hospitalarios2012.sav")
eg2013 <- read_sav("Data/Egresos_hospitalarios2013.sav")
eg2014 <- read_sav("Data/Egresos_hospitalarios2014.sav")
eg2015 <- read_sav("Data/Egresos_hospitalarios2015.sav")
eg2016 <- read_sav("Data/Egresos_hospitalarios2016.sav")
eg2017 <- read_sav("Data/Egresos_hospitalarios2017.sav")
eg2018 <- read_sav("Data/Egresos_hospitalarios2018.sav")
eg2019 <- read_sav("Data/Egresos_hospitalarios2019.sav")



#######################################
#Capture variable names for 2011- 2015
#######################################

capture.output(names(eg2015), file = "variables2015.txt")
capture.output(names(eg2014), file = "variables2014.txt")
capture.output(names(eg2013), file = "variables2013.txt")
capture.output(names(eg2012), file = "variables2012.txt")
capture.output(names(eg2011), file = "variables2011.txt")

#########################################
#### Exploring Bind variables that have same variables' names (2015-2019)
#########################################

#LIst of variables class

t(t(sapply(eg2019, class)))
t(t(sapply(eg2018, class)))
t(t(sapply(eg2017, class)))
t(t(sapply(eg2016, class)))
t(t(sapply(eg2015, class)))

eg2017$fecha_egr <- as.character(eg2017$fecha_egr)
eg2017$fecha_ingr <- as.character(eg2017$fecha_ingr) 
eg2017 <- eg2017 %>% mutate(fecha_ingr = str_replace_all(fecha_ingr, "-", "/")) %>% 
  mutate(fecha_egr = str_replace_all(fecha_egr, "-", "/"))

eg2016$fecha_egr <- as.character(eg2016$fecha_egr)

eg2016$fecha_ingr <- as.character(eg2016$fecha_ingr) 
eg2016 <- eg2016 %>% mutate(fecha_ingr = str_replace_all(fecha_ingr, "-", "/")) %>% 
  mutate(fecha_egr = str_replace_all(fecha_egr, "-", "/"))


eg2015$fecha_egr <- as.character(eg2015$fecha_egr)
eg2015$fecha_ingr <- as.character(eg2015$fecha_ingr)
eg2015 <- eg2015 %>% mutate(fecha_ingr = str_replace_all(fecha_ingr, "-", "/")) %>% 
  mutate(fecha_egr = str_replace_all(fecha_egr, "-", "/"))



eg2018_2019 <- bind_rows(eg2018, eg2019)
#Works!

eg2017_2019 <- bind_rows(eg2017, eg2018_2019)
#Works!

eg2017_2019$nac_pac <- as.character(eg2017_2019$nac_pac)


eg2016_2019 <- bind_rows(eg2016, eg2017_2019)
#Works!


t(t(sapply(eg2018_2019, class)))
t(t(sapply(eg2017_2019, class)))
t(t(sapply(eg2016_2019, class)))

######################
### Prepare to bind other year (2011-2014) NOV26_2020
### Changing name and variables class
######################

t(t(sapply(eg2014, class)))

eg2014 <- eg2014 %>% mutate_if(is.Date, as.character) %>% 
  select(prov_ubi, cant_ubi, parr_ubi, clase, tipo, entidad, sector, 
         mes_inv, sexo, cod_edad, edad, etnia, prov_res, cant_res, parr_res,
         anio_ingr, mes_ingr, dia_ingr, fecha_ingr, anio_egr, mes_egr, 
         dia_egr, fecha_egr, dia_estad, con_egrpa, cau_cie10, causa3, 
         cap221rx, cau221rx, cau298rx) %>% 
  mutate(fecha_ingr = str_replace_all(eg2014$fecha_ingr, "-", "/")) %>% 
  mutate(fecha_egr = str_replace_all(eg2014$fecha_egr, "-", "/"))

eg2013 <- eg2013 %>% mutate_if(is.Date, as.character) %>% 
  select(prov_ubi, cant_ubi, parr_ubi, clase, tipo, entidad, sector, 
         mes_inv, sexo, cod_edad, edad, prov_res, cant_res, parr_res,
         anio_ingr, mes_ingr, dia_ingr, fecha_ingr, anio_egr, mes_egr, 
         dia_egr, fecha_egr, dia_estad, con_egrpa, cau_cie10, causa3, 
         cap221rx, cau221rx, cau298rx) %>% 
  mutate(cod_edad = cod_edad +1) %>% 
  within(cod_edad[cod_edad == 10] <- 4)


eg2012 <- eg2012 %>% mutate_if(is.Date, as.character) %>% 
  unite("fecha_ingr", Anio_ingr, Mes_ingr, Dia_ingr, sep = "/", remove = FALSE) %>% 
  select(Prov_ubie, Cant_ubie, Parr_ubie, Clase, Tipo, Entidad, Sector, 
         Mes_inv, Sexo_pac, Cond_edad, Edad_pac, Prov_pac, Cant_pac, Parr_pac,
         Anio_ingr, Mes_ingr, Dia_ingr, fecha_ingr, Mes_egr, 
         Dia_egr, Dia_estad, Con_egrpa, Cau_cie10, Causa3, 
         Cap221rx, Cau221rx, Cau298rx) %>% 
  rename(prov_ubi = Prov_ubie, cant_ubi = Cant_ubie, parr_ubi = Parr_ubie, 
         clase = Clase, tipo = Tipo, entidad = Entidad, sector = Sector, 
         mes_inv = Mes_inv, sexo = Sexo_pac, cod_edad = Cond_edad, edad = Edad_pac,
         prov_res = Prov_pac, cant_res = Cant_pac, parr_res = Parr_pac,
         anio_ingr = Anio_ingr, mes_ingr = Mes_ingr, dia_ingr = Dia_ingr, 
         mes_egr = Mes_egr, dia_egr = Dia_egr, dia_estad = Dia_estad, 
         con_egrpa = Con_egrpa, cau_cie10 = Cau_cie10, causa3 = Causa3, 
         cap221rx = Cap221rx, cau221rx = Cau221rx, cau298rx = Cau298rx) %>% 
  mutate(entidad = as.numeric(as.character(entidad))) %>% 
  mutate(sector = as.numeric(as.character(sector))) %>% 
  mutate(sexo = as.numeric(as.character(sexo))) %>% 
  mutate(cod_edad = cod_edad +1) %>% 
  within(cod_edad[cod_edad == 10] <- 4)

eg2011 <- eg2011 %>% mutate_if(is.Date, as.character) %>% 
  unite("fecha_ingr", anio_ingr, mes_ingr, dia_ingr, sep = "/", remove = FALSE) %>% 
  select(prov_ubie, cant_ubie, par_ubie, clase, tipo, entidad, sector, 
         mes_inv, sexo_pac, cond_edad, edad_pac, prov_pac, cant_pac, parr_pac,
         anio_ingr, mes_ingr, dia_ingr, fecha_ingr, mes_egr, 
         dia_egr, dia_estad, con_egrpa, cau_cie10, 
         cap221rx, cau221rx, cau298rx) %>% 
  rename(prov_ubi = prov_ubie, cant_ubi = cant_ubie, parr_ubi = par_ubie, 
         sexo = sexo_pac, cod_edad = cond_edad, edad = edad_pac,
         prov_res = prov_pac, cant_res = cant_pac, parr_res = parr_pac)%>%   
  mutate(cod_edad = as.numeric(as.character(cod_edad))) %>% 
  mutate(clase = as.numeric(as.character(clase))) %>% 
  mutate(tipo = as.numeric(as.character(tipo))) %>% 
  mutate(entidad = as.numeric(as.character(entidad))) %>% 
  mutate(sector = as.numeric(as.character(sector))) %>% 
  mutate(mes_inv = as.numeric(as.character(mes_inv))) %>% 
  mutate(sexo = as.numeric(as.character(sexo))) %>% 
  mutate(mes_ingr = as.numeric(as.character(mes_ingr))) %>% 
  mutate(mes_egr = as.numeric(as.character(mes_egr))) %>% 
  mutate(con_egrpa = as.numeric(as.character(con_egrpa))) %>% 
  mutate(cap221rx = as.numeric(as.character(cap221rx))) %>% 
  mutate(cau221rx = as.numeric(as.character(cau221rx))) %>% 
  mutate(cau298rx = as.numeric(as.character(cau298rx))) %>% 
  mutate(cod_edad = cod_edad +1) %>% 
  within(cod_edad[cod_edad == 10] <- 4)




#######################################################
#### Putting together all the years for Spatiomtemporal


### 2015 -2019 
###Todas las variables completas
###Rural vs Urbano en todos los anios (area_ubi, 1 = Urban, 2 = Rural)
eg2015_2019 <- bind_rows(eg2015, eg2016_2019)  

nrow(eg2015)
nrow(eg2016)
nrow(eg2017)
nrow(eg2018)
nrow(eg2019)
nrow(eg2015_2019)
sum(nrow(eg2015), nrow(eg2016), nrow(eg2017), nrow(eg2018), nrow(eg2019))
#works! SUm of number of observations is correct

##### 2014 -2019 Etnia in all databases, sin Rural vs Urban
#####Cod_edad 4 niveles(1=hora, 2 dia, 3 mes, 4 anio, 9 ignorado)

eg2014_2019 <- bind_rows(eg2014, eg2015_2019)
nrow(eg2014_2019)
sum(nrow(eg2014) + nrow(eg2015_2019))

##### 2014- 2019 Cambiando niveles cod_edad. Si horas, cambiar edad a 1, 
##### Y cambiar cod_edad de horas (1) a dia (2). Quedan 2=dia, 3=mes, 4=anio
table(eg2014_2019$cod_edad)

eg2014_2019 <- eg2014_2019 %>%  mutate(edad = if_else(cod_edad == 1, 1, edad ))
eg2014_2019 <- eg2014_2019 %>% within(cod_edad[cod_edad == 1] <- 2)

table(eg2014_2019$cod_edad)
#Consistentes los cod_edad antes y despues del cambio 

##### 2013 - 2019, Buena información del egreso (2011-2012 no tienen anio egreso). 
eg2013_2019 <- bind_rows(eg2013, eg2014_2019)

nrow(eg2013_2019)
sum(nrow(eg2013) + nrow(eg2014_2019))

##### 2011 - 2019, Todas las variables 
##todos los anios, subset ANios solo anios despues 2010

eg2011_2019 <- bind_rows(eg2011, eg2012, eg2013_2019)
eg2011_2019 <- eg2011_2019 %>% subset(anio_ingr > 2010)

nrow(eg2011_2019)
sum(nrow(eg2011) + nrow(eg2012) + nrow(eg2013_2019))

### Dropping variables that are NOT in all years. 
eg_2011_2019_all <- eg2011_2019 %>% 
  select(-c(causa3, anio_egr, fecha_egr, etnia, area_ubi, nac_pac, nom_pais, 
            cod_pais, area_res, esp_egrpa))

###############################################
###Calculating Centroids from shapefile 
####### 

cantones_shp <- readOGR("Data/Cantones2014_INEC", "nxcantones")
plot(cantones_shp)
centroidCant <- gCentroid(cantones_shp, byid = TRUE)

SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 

plot(cantones_shp)
plot(SpDF_centroid, add = TRUE)

CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_df <- CantCentr_df %>% select(DPA_CANTON, DPA_DESCAN, x, y) 

proy_pob2010_2020 <- read.csv("Data/proyeccion_cantonal_total_2010-2020.csv", sep = " ", header= TRUE,  stringsAsFactors=FALSE)

###### Join Canton Ubi
eg2011_2019_extVar0 <- eg2011_2019 %>% left_join(y = CantCentr_df, by = c("cant_ubi" = "DPA_CANTON")) %>% 
  rename(CordXubi = x, CordYubi = y, Nom_CantUbi = DPA_DESCAN) 

eg2011_2019_extVar <- eg2011_2019_extVar0  %>% left_join(y = CantCentr_df, by = c("cant_res" = "DPA_CANTON")) %>% 
  rename(CordXres = x, CordYres = y, Nom_CantRes = DPA_DESCAN)

### ACAA NOV 30 missing to ver como usar COALESCE para variable residencias faltantes. 


#select(prov_ubi, cant_ubi, parr_ubi, clase, tipo, entidad, sector, 
#      mes_inv, sexo, cod_edad, edad, etnia, prov_res, cant_res, parr_res,
#     anio_ingr, mes_ingr, dia_ingr, fecha_ingr, anio_egr, mes_egr, 
#    dia_egr, fecha_egr, dia_estad, con_egrpa, cau_cie10, causa3, 
#   cap221rx, cau221rx, cau298rx)


t(t(sapply(eg2011_2019, class)))

##### Join Canton Res

##### Join Proyeccion Poblacion Cant Res




##READY down to here, missing to add Population projections 


#############################################
### Exploring diseases

childLeuk <- eg2011 %>% filter(edad_pac < 15) %>% filter(str_detect(cau_cie10, "C95"))

hist(childLeuk$edad_pac)

parkinsons <- eg2011 %>% filter(str_detect(cau_cie10, "G20"))
hist(parkinsons$edad_pac)

NHlym <- eg2011 %>% filter(str_detect(cau_cie10, "C85"))
hist(NHlym$edad_pac)

pest_intox <- eg2011 %>% filter(str_detect(cau_cie10, "T60"))
hist(pest_intox$edad_pac)

#################################################

summary(pest_intox$dia_estad)
summary(childLeuk$dia_estad)
summary(NHlym$dia_estad)
summary(parkinsons$dia_estad)


boxplot(pest_intox$edad_pac ~ pest_intox$con_egrpa)
boxplot(childLeuk$edad_pac ~ childLeuk$con_egrpa)
boxplot(NHlym$edad_pac ~ NHlym$con_egrpa)
boxplot(parkinsons$edad_pac ~ parkinsons$con_egrpa)

cod_estFactorPest <- as.factor(pest_intox$cod_est)
summary(cod_estFactorPest)

cant_FactorPest <- as.factor(pest_intox$cant_ubie)
table(cant_FactorPest)

plot(cant_FactorPest)

##############################################################################
#Spatiotemporal - DEFUNCIONES 2011-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################



####################
# Importing and exploring data DEFUNCIONES


edg2019 <- read.csv("Data/BDD_EDG_2019.csv", header =  TRUE, sep = ';')

Mort_cLeuk2019 <- edg2019 %>% filter(edad < 15) %>% filter(str_detect(causa, "C95"))
hist(Mort_cLeuk2019$edad)

Mort_Park2019 <- edg2019 %>% filter(str_detect(causa, "G20"))
hist(Mort_Park2019$edad)

Mort_NHlym2019 <- edg2019 %>% filter(str_detect(causa, "C85"))
hist(Mort_NHlym2019$edad)

Mort_pesttox2019 <- edg2019 %>% filter(str_detect(causa, "T60"))



##############################################################################
#Temporal - 2000-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################

