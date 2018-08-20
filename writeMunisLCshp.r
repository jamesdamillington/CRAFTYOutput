#script to output a shp of municipalities with model prediction data for a given year

rm(list=ls())

#set year here (year must be in the input, cDat, data file)
yr<-2005

library(tidyverse)
library(sf)

#this directory should exist and contain the CRAFTYmunisServCap.csv
scenario <- "Testing_2018-08-16c"
runID <- "0-0"

#input/putput variables
cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv"))

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

#join
cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
cDat_map <- cDat_map %>% mutate(muniID = CD_GEOCMUn)

#write
st_write(cDat_map, paste0("Data/",scenario,"/",runID,"/",scenario,"_",yr,"_muniDat.shp"))