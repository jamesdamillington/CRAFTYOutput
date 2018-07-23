#this is a one-time only script to make image files (of observed LC) needed by outputMovies.R script

rm(list=ls())

library(raster)
library(gridExtra)

sim_yrs <- seq(2000, 2005, 1)   #movie made for all these years

cl <- "PastureB" 

#Maps
#For 2000 for the three different classifications (for quick visual comparison)

for(i in seq_along(sim_yrs)){
  
  LU <- raster(paste0("Data/ObservedLCmaps/brazillc_",sim_yrs[i],"_",cl,".asc"))
  maskmap <- raster(paste0("Data/ObservedLCmaps/sim10_BRmunis_latlon_5km_2018-04-27.asc"))
  
  LU <- mask(LU, maskmap)
  LU <- trim(LU)
  
  #add categories for later plotting etc. (see https://stackoverflow.com/a/37214431)
  LU <- ratify(LU)     #tell R that the map raster is categorical 
  rat <- levels(LU)[[1]]    #apply the levels (i.e. categories) 

  uLU <- unique(LU) 

  LUcols <- c()
  LUlabs <- c()
  
  if(1 %in% uLU) { 
    LUcols <- c(LUcols, 'forestgreen') 
    LUlabs <- c(LUlabs, 'Nature')  }
  if(2 %in% uLU) { 
    LUcols <- c(LUcols, 'darkcyan') 
    LUlabs <- c(LUlabs, 'Other Agri') }
  if(3 %in% uLU) { 
    LUcols <- c(LUcols, 'wheat2') 
    LUlabs <- c(LUlabs, 'Agriculture') }
  if(4 %in% uLU) { 
    LUcols <- c(LUcols, 'black') 
    LUlabs <- c(LUlabs, 'Other') }
  if(5 %in% uLU) { 
    LUcols <- c(LUcols, 'orange2') 
    LUlabs <- c(LUlabs, 'Pasture') }
  
  rat$LandUse <- LUlabs  
  levels(LU) <- rat 
  
  
  
  p <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0("Observed Land Use ",sim_yrs[i]))  
  
  ggsave(paste0("Data/ObservedLCmaps/Raster_ObsLUMap",sim_yrs[i],".png"), plot = arrangeGrob(p), width=15, height=15, units="cm", dpi = 300)
  
}


