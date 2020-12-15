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
#Spatiotemporal - EGRESOS 2011-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################

##
# Importing and exploring data EGRESOS
###


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

#capture.output(names(eg2015), file = "variables2015.txt")
#capture.output(names(eg2014), file = "variables2014.txt")
#capture.output(names(eg2013), file = "variables2013.txt")
#capture.output(names(eg2012), file = "variables2012.txt")
#capture.output(names(eg2011), file = "variables2011.txt")

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
eg2018_2019 <- eg2018_2019 %>% 
  unite("fecha_ingr", anio_ingr, mes_ingr, dia_ingr, sep = "/", remove = FALSE)


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

###### Exploring Missing Values in Cant_RES 
sum(is.na(eg2011_2019$cant_res))
sum(eg2011_2019$cant_res == "8800")
#11535
# NO missing, son "8800" que significa EXTERIOR. Puedo quitarlos =)

eg2011_2019 <- eg2011_2019 %>% subset(cant_res != "8800")
sum(eg2011_2019$cant_res == "8800")

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

#plot(cantones_shp)
#plot(SpDF_centroid, add = TRUE)

CantCentr_df <- as.data.frame(SpDF_centroid)


CantCentr_df <- CantCentr_df %>% select(DPA_CANTON, DPA_DESCAN, x, y) 

save(CantCentr_df, file = "Data/EcuadorCantCent.RData")

class(CantCentr_df$x)
class(CantCentr_df$y)

###PARECE QUE TODO BIEN HASTA ACA CON CORDS REVISAR SI ES ABAJO AL MANIPULAR LA VARIABLE
#There was a conflict with select (probably with raster), alternative is conflict_prefer(),
#pero decid no llamar el paquete raster() que era el del conflicto

proy_pob2010_2020unitdy <- read.csv("Data/proyeccion_cantonal_total_2010-2020.csv", sep = " ", header= TRUE,
                              colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
              
proy_pob2010_2020 <- proy_pob2010_2020unitdy %>% 
  pivot_longer(c(X2010, X2011, X2012, X2013, 
                                    X2014, X2015, X2016, X2017, 
                                    X2018, X2019, X2020),
                                    names_to = "proy_year", values_to = "proy_pob") %>% 
  mutate(proy_year = as.numeric(gsub("X", "", proy_year)))

#save(proy_pob2010_2020, file = "Data/populationCanton.RData")


eg2011_2019_extVar0 <- eg2011_2019 %>% left_join(y = CantCentr_df, by = c("cant_ubi" = "DPA_CANTON")) %>% 
  rename(CordXubi = x, CordYubi = y, Nom_CantUbi = DPA_DESCAN) 

eg2011_2019_extVar <- eg2011_2019_extVar0  %>% left_join(y = CantCentr_df, by = c("cant_res" = "DPA_CANTON")) %>% 
  rename(CordXres = x, CordYres = y, Nom_CantRes = DPA_DESCAN) %>% 
  left_join(y = proy_pob2010_2020, by = c("cant_res" = "DPA_CANTON","anio_ingr" = "proy_year") ) %>% 
  rename(proy_pobRes = proy_pob) %>% 
  left_join(y = proy_pob2010_2020, by = c("cant_ubi" = "DPA_CANTON","anio_ingr" = "proy_year") ) %>%
  rename(proy_pobUbi = proy_pob) %>% 
  select(prov_ubi, cant_ubi, Nom_CantUbi, CordXubi, CordYubi, proy_pobUbi, parr_ubi, area_ubi, clase, tipo, 
         entidad, sector, mes_inv, sexo, cod_edad, edad, etnia, nac_pac, nom_pais, cod_pais, prov_res, cant_res, 
         Nom_CantRes, CordXres, CordYres, proy_pobRes, parr_res, area_res, anio_ingr, mes_ingr, dia_ingr, 
         fecha_ingr, anio_egr, mes_egr, dia_egr, fecha_egr, dia_estad, con_egrpa, cau_cie10, causa3, 
         cap221rx, cau221rx, cau298rx, esp_egrpa)


t(t(sapply(eg2011_2019_extVar, class)))

### CODE TO SAVE FILE
#save(eg2011_2019_extVar, file = "Data/eg2011_2019_extVar.RData")
#write_csv(eg2011_2019_extVar, "Data/eg2011_2019_extVar.csv")
#write_sav(eg2011_2019_extVar, "Data/eg2011_2019_extVar.sav")
#write_sas(eg2011_2019_extVar, "Data/eg2011_2019_extVar.sas7bdat")



#############################################
### Exploring diseases

childLeuk <- eg2011_2019_extVar %>% filter((edad < 15 & cod_edad == "4") | cod_edad == "3" | cod_edad == "2") %>%
  filter(str_detect(cau_cie10, "C95"))
plot(childLeuk$anio_ingr)
range(childLeuk$edad)

ggplot(childLeuk) +
  geom_bar(aes(x = factor(anio_ingr), fill = factor(sexo)), position = position_dodge(), width = 0.7) +
  theme(legend.position="right") +
  labs(title = 'Child Leukemia frequency (under 15years)') 


parkinsons <- eg2011_2019_extVar %>% filter(str_detect(cau_cie10, "G20"))
ggplot(parkinsons) +
  geom_bar(aes(x = factor(anio_ingr), fill = factor(sexo)), position = position_dodge(), width = 0.7) +
  theme(legend.position="right") +
  labs(title = "Parkinson's Disease frequency") 

NHlym <- eg2011_2019_extVar %>% filter(str_detect(cau_cie10, "C85"))
ggplot(NHlym) +
  geom_bar(aes(x = factor(anio_ingr), fill = factor(sexo)), position = position_dodge(), width = 0.7) +
  theme(legend.position="right") +
  labs(title = 'Non-Hodgkin lymphoma frequency') 

pest_intox <- eg2011_2019_extVar %>% filter(str_detect(cau_cie10, "T60"))
ggplot(pest_intox) +
  geom_bar(aes(x = factor(anio_ingr), fill = factor(sexo)), position = position_dodge(), width = 0.7) +
  theme(legend.position="right") +
  labs(title = 'Pesticides intoxication frequency')



