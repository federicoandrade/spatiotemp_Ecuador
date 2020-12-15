library(haven)
library(tidyverse)
library(remotes)
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

edg2019 <- read_sav("Data/EDG_2019.sav")
edf2018 <- read_sav("Data/EDF_2018.sav")
edf2017 <- read_sav("Data/EDF_2017.sav")
edf2016 <- read_sav("Data/EDF_2016.sav")
edf2015 <- read_sav("Data/EDF_2015.sav")
edf2014 <- read_sav("Data/EDF_2014.sav")
edf2013 <- read_sav("Data/EDF_2013.sav")
edf2012 <- read_sav("Data/EDF_2012.sav")
edf2011 <- read_sav("Data/EDF_2011.sav")



=



