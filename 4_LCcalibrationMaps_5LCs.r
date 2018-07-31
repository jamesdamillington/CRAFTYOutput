#script to create maps for comparison of modelled vs observed land cover 
#reads file created by CRAFTYsummary_5LCs.r


rm(list=ls())

#script assumes there are three calibration years (2005, 2010, 2015); edit next line if that is not correct
calib_yrs <- c(2005, 2010, 2015)

#maps can be plotted to pdf by setting following variable appropriately (TRUE/FALSE)
pdfplot <- TRUE


library(tidyverse)
library(sf)
library(RColorBrewer)  #for plotting

#this directory should exist and contain the CRAFTYmunisServCap.csv
scenario <- "Testing_2018-07-31"
runID <- "0-0"

#input/putput variables
output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_LCcomparisonMaps.pdf")

cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv"),
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

if(pdfplot) {
  pdf(file = output_name)
}

## Maps
#loop through years, printing maps
for(yr in calib_yrs){

  cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 

  #create land cover palette
  map_pal <- c("darkgreen", "darkcyan", "green", "grey", "khaki")

  #plot observed vs modelled modal muni land cover
  plot(cDat_map["ObsMode"], pal = map_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Observed Mode LC"), key.pos = NULL)
  legend("bottomright", cex = 1.3, c("Nature", "Other Agri", "Agriculture", "Other", "Pasture"), fill = map_pal)
  
  plot(cDat_map["ModMode"], pal = map_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LC"), key.pos = NULL)
  legend("bottomright", cex = 1.3, c("Nature", "Other Agri", "Agriculture", "Other", "Pasture"), fill = map_pal)

  
  #map of muni mode correct/incorrect   
  plot(cDat_map["diffcMode"], pal = c("darkgreen","red"), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Model vs Obs Mode Comparison"), key.pos = NULL)  
  legend("bottomright", cex = 1.3, c("Correct", "Incorrect"), fill = c("darkgreen","red"))
  
  
  #get max value for colour breaks below
  errorMax <- max(filter(cDat, Year == yr)$cellDiffcCount)

  #for cell accuracy maps
  cell_pal <- brewer.pal(8, "Reds")

  #total count of cells incorrect
  plot(cDat_map["cellDiffcCount"], pal = cell_pal, breaks = seq(0,errorMax, length.out = length(cell_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Count Incorrect Cells"))

  #proportion of cells incorrect 
  plot(cDat_map["cellDiffcProp"], pal = cell_pal, breaks = seq(0,1, length.out = length(cell_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Prop Incorrect Cells"))

  
  #for LC proportion accuracy maps
  prop_pal <- brewer.pal(11, "RdYlGn")
  
  #difference in proportion predictions (negative is under-prediction by model, positive over-prediction)
  plot(cDat_map["diffcProp1"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Nature Prop Diffc"))
  plot(cDat_map["diffcProp2"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Other Agri Prop Diffc"))
  plot(cDat_map["diffcProp3"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Agriculture Prop Diffc"))
  plot(cDat_map["diffcProp5"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Pasture Prop Diffc"))
  plot(cDat_map["diffcProp4"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Other Prop Diffc"))
  par(mfrow=c(1,1))  #needed to ensure plotting plays nicely with key.pos = NULL above
  
}

if(pdfplot) {
  dev.off()
}

