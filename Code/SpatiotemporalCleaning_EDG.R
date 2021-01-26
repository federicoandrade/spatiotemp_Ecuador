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
#Spatiotemporal - Defunciones generales 2011-2019 analysis of diseases associated 
#with pesticides use in Ecuador
###############################################################################

edg2019 <- read_sav("Data/EDG_2019.sav")
edg2018 <- read_sav("Data/EDG_2018.sav")
edg2017 <- read_sav("Data/EDG_2017.sav")
edg2016 <- read_sav("Data/EDG_2016.sav")
edg2015 <- read_sav("Data/EDG_2015.sav")
edg2014 <- read_sav("Data/EDG_2014.sav")
edg2013 <- read_sav("Data/EDG_2013.sav")
edg2012 <- read_sav("Data/EDG_2012.sav")
edg2011 <- read_sav("Data/EDG_2011.sav")
edg2010 <- read_sav("Data/EDG_2010.sav")

#############
#Class

t(t(sapply(edg2019,class)))
t(t(sapply(edg2018, class)))
t(t(sapply(edg2017, class)))
t(t(sapply(edg2016, class)))
t(t(sapply(edg2015, class)))

## Lista variables para comparar en Excel

capture.output(names(edg2019), file = "Results/EDGvariables2019.txt")
capture.output(names(edg2018), file = "Results/EDGvariables2018.txt")
capture.output(names(edg2017), file = "Results/EDGvariables2017.txt")
capture.output(names(edg2016), file = "Results/EDGvariables2016.txt")
capture.output(names(edg2015), file = "Results/EDGvariables2015.txt")
capture.output(names(edg2014), file = "Results/EDGvariables2014.txt")
capture.output(names(edg2013), file = "Results/EDGvariables2013.txt")
capture.output(names(edg2012), file = "Results/EDGvariables2012.txt")
capture.output(names(edg2011), file = "Results/EDGvariables2011.txt")

