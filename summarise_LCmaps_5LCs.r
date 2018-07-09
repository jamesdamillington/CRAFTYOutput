#Create SummaryTables (needed before other CRAFTYOutput scripts)
#Takes mapbiomas maps and converts to tables of summary data by municipality
#Assumes 5 land covers. Outputs data to SummaryTables directory

rm(list=ls())

library(raster)
library(tidyverse)
library(mapview)

######
#FUNCTIONS
#raster to xyz  (with help from https://stackoverflow.com/a/19847419)
#sepcify input raster, whether nodata cells should be output, whether a unique cell ID should be added
#return is a matrix. note format is row (Y) then col (X)
extractXYZ <- function(raster, nodata = FALSE, addCellID = TRUE){
  
  vals <- raster::extract(raster, 1:ncell(raster))   #specify raster otherwise dplyr used
  xys <- rowColFromCell(raster,1:ncell(raster))
  combine <- cbind(xys,vals)
  
  if(addCellID){
    combine <- cbind(1:length(combine[,1]), combine)
  }
  
  if(!nodata){
    combine <- combine[!rowSums(!is.finite(combine)),]  #from https://stackoverflow.com/a/15773560
  }
  
  return(combine)
}


getLCs <- function(data)
{
  #calculates proportion of each LC in the muni (ignoring NAs, help from https://stackoverflow.com/a/44290753)
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(LC1 = round(sum(lc2000 == 1, na.rm = T) / sum(!is.na(lc2000)), 3),
                     LC2 = round(sum(lc2000 == 2, na.rm = T) / sum(!is.na(lc2000)), 3),
                     LC3 = round(sum(lc2000 == 3, na.rm = T) / sum(!is.na(lc2000)), 3),
                     LC4 = round(sum(lc2000 == 4, na.rm = T) / sum(!is.na(lc2000)), 3),
                     LC5 = round(sum(lc2000 == 5, na.rm = T) / sum(!is.na(lc2000)), 3),
                     NonNAs = sum(!is.na(lc2000)),
                     NAs = sum(is.na(lc2000))
    ) -> LCs

  return(LCs)
}
######


#####
#Only needed once, hence commented out
#initially needed to create raster of munis on same projection as mapbiomas data
#munis.r <- raster("Raster/sim10_BRmunis_latlon_5km.asc")
#plot(munis.r)

#lc2000 <- raster("Raster/brazillc_2000_int_reclass_5km_txt.txt")

#need to resample to get raster to align?!
#munis.r.new = resample(munis.r, lc2000, "ngb")
  
#writeRaster(munis.r.new, "Raster/sim10_BRmunis_latlon_5km_2018-03-21", format = 'ascii')
# ######

#load the rasters
munis.r <- raster("Raster/sim10_BRmunis_latlon_5km_2018-04-27.asc")

calib_yrs <- seq(2000, 2016, 1)
#calib_yrs <- c(2000, 2005)

for(yr in calib_yrs){

  inname <- paste0("Raster/brazillc_",yr,"_int_reclass_5km_txt_pasture.txt")
  outname <- paste0("SummaryTables/LCs",yr,".csv")
  
  lc <- raster(inname)

  #extract cell values to table format
  munis.t <- extractXYZ(munis.r, addCellID = F)
  lc.t <- extractXYZ(lc, addCellID = F)
  
  munis.t <- as.data.frame(munis.t)
  munis.t <- plyr::rename(munis.t, c("vals" = "muniID"))
  
  lc.t <- as.data.frame(lc.t)
  lc.t <- plyr::rename(lc.t, c("vals" = "lc"))
  
  #set NA in both rasters
  lc[is.na(munis.r)] <- NA
  munis.r[is.na(lc)] <- NA
  
  lc_munis <- left_join(as.data.frame(munis.t), as.data.frame(lc.t), by = c("row" = "row", "col" = "col"))
  
  #now summarise by muniID
  lcs <- getLCs(lc_munis)
  
  head(lcs)
  summary(lcs)
  
  write.csv(lcs, outname, row.names = F)

}

#plot municipalities with NAs
#library(sf)
#BRmunis <- st_read(paste(CRAFTY_testing, "/Vector/BRmunis_sim10_simple2.shp", sep = ""))
  
#cDat_map <- left_join(BRmunis, lcs_2000, by = c("CD_GEOCMUn" ="muniID")) 
 
#plot(cDat_map["NAs"], breaks = c(0,1,100), pal = c("darkgreen","red"), graticule = st_crs(cDat_map), axes = TRUE)  


