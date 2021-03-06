library(haven)
library(tidyverse)
library(lubridate)
library(sp)
library(sf)
library(rgeos)
library(geosphere)
library(rgdal)
library(raster)

############################
#Trying to see where the issue is with GADM... when joining, more obs
#EDF


## Prueba con GADM


#cantones_shp <- raster::getData('GADM',country='ECU',level=2)
#cantones_INEC <- readOGR("Data/Cantones2014_INEC", "nxcantones")
cantones_shp <- readOGR("Data/CantonesGADM_editados", "cantones_GADMedit")


centroidCantINEC <- gCentroid(cantones_INEC, byid = TRUE)


SpDF_centroidINEC <- SpatialPointsDataFrame(centroidCantINEC, cantones_INEC@data)
SpDF_centroidINEC 



CantCentr_dfINEC <- as.data.frame(SpDF_centroidINEC)



#plot(cantones_shp)
centroidCant <- gCentroid(cantones_shp, byid = TRUE)


SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 



CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_GADMext <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, NAME_1, x, y) 

CantCentr_GADM <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, x, y) 

#edf2009_2019$cant_res[!(edf2009_2019$cant_res %in% CantCentr_GADM$CC_2)]
#edf2009_2019$cant_res[!(edf2009_2019$cant_res %in% CantCentr_dfext$DPA_CANTON)]

#save(CantCentr_df, file = "Data/EcuadorCantCent.RData")

class(CantCentr_df$x)
class(CantCentr_df$y)


#prueba <- CantCentr_dfext %>% left_join(y = CantCentr_GADMext, by = c("DPA_CANTON" = "CC_2")) 
#prueba <- prueba %>% dplyr::select(DPA_CANTON, DPA_DESCAN, NAME_2)

n_occur <- data.frame(table(prueba$DPA_CANTON))

n_occur <- n_occur %>% 
  filter(Freq==2)

edf2009_2019 <- edf2009_2019  %>% left_join(y = CantCentr_GADMext, by = c("cant_res" = "CC_2"))%>% 
  rename(Cantlong = x, Cantlat = y, Nom_CantRes = NAME_2, Nom_ProvRes = NAME_1)




cantones_shp2 <- raster::getData('GADM',country='ECU',level=2)
plot(cantones_shp2)
writeOGR(cantones_shp2, dsn = "Results", layer = "cantonesGADM_shp", driver = "ESRI Shapefile")
centroidCant2 <- gCentroid(cantones_shp2, byid = TRUE)


SpDF_centroid2 <- SpatialPointsDataFrame(centroidCant2, cantones_shp2@data)
SpDF_centroid2 


df1 <- data.frame(col1 = LETTERS[1:4],
                  col2 = 1:4)
df2 <- data.frame(col1 = rep(LETTERS[1:2], 2),
                  col3 = 4:1)

df3<- left_join(df1, df2) 


#plot(cantones_shp)
#plot(SpDF_centroid, add = TRUE)

###INTENTO PARR


cantones_shp <- readOGR("Data/CantonesGADM_editados", "cantones_GADMedit")

centroidCant <- gCentroid(cantones_shp, byid = TRUE)


SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 



CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_GADMext <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, NAME_1, x, y) 

CantCentr_GADM <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, x, y) 

edf2009_2019 <- edf2009_2019  %>% left_join(y = CantCentr_GADMext, by = c("cant_res" = "CC_2"))%>% 
  rename(Cantlong = x, Cantlat = y, Nom_CantRes = NAME_2, Nom_ProvRes = NAME_1)




parroquias_shp <- readOGR("Data/ParroquiasGADM", "parroquias_conGol")
#plot(parroquias_shp)
#writeOGR(parroquias_shp, dsn = "Results", layer = "parroquias_shp", driver = "ESRI Shapefile")

centroidParr <- gCentroid(parroquias_shp, byid = TRUE)

SpDF_centroidParr <- SpatialPointsDataFrame(centroidParr, parroquias_shp@data)
SpDF_centroidParr

ParrCentr <- as.data.frame(SpDF_centroidParr)

ParrCentr <- ParrCentr %>% 
  dplyr::select(CC_3, NAME_3, x,y) %>% 
  rename(ParrLong = x, ParrLat = y)

n_occur <- data.frame(table(ParrCentr$CC_3))

n_occur <- n_occur %>% 
  filter(Freq==2)

 
edf2009_2019 <- edf2009_2019  %>% left_join(y = ParrCentr, by = c("parr_res" = "CC_3"))%>% 
  rename( Nom_ParrRes = NAME_3)

ExplorEnvd2009_2019 <- edf2009_2019 %>% 
  dplyr::select(cant_res, parr_res, Nom_CantRes, Nom_ProvRes, Cantlong, Cantlat, ParrLong, ParrLat)


ExplorEnvd2009_2019 <- ExplorEnvd2009_2019 %>% 
  filter(is.na(ParrLong))

###HERE MARCH $.... many missing parroquias


########################
#### Definitivo CAntones
#######################

cantones_shp <- readOGR("Data/CantonesGADM_editados", "cantones_GADMedit")


centroidCant <- gCentroid(cantones_shp, byid = TRUE)


SpDF_centroid <- SpatialPointsDataFrame(centroidCant, cantones_shp@data)
SpDF_centroid 



CantCentr_df <- as.data.frame(SpDF_centroid)

CantCentr_GADMext <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, NAME_1, x, y) 

CantCentr_GADM <- CantCentr_df %>% dplyr::select(CC_2, NAME_2, x, y) 

edf2009_2019 <- edf2009_2019  %>% left_join(y = CantCentr_GADMext, by = c("cant_res" = "CC_2"))%>% 
  rename(Cantlong = x, Cantlat = y, Nom_CantRes = NAME_2, Nom_ProvRes = NAME_1)

#####################################
#### Definitivo Parroquias
####################################


