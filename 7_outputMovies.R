
#Six raster maps for all cells, Six vector maps for munis:

#1. Observed LC - only for vector (mode for muni) 
#2. Modelled LC (mode for muni)
#3. Agri Capital (mean for muni)
#4. Nature Capital (mean for muni)
#5. Infrastructure Capital (mean for muni)
#6. Other Agri Capital (mean for muni)
#7 Acessibility - only for raster

#raster data from cell output data
#muni cata from CRAFTYmunisLC.csv and CRAFTYmunisServCap.csv


rm(list=ls())

#this directory should exist and contain the output files and region file from the run being examined
run_name <- "test"

library(raster)
library(tidyverse)
library(rasterVis)
library(gridExtra)
library(RColorBrewer)
  
#Load model output data
output2005 <- read_csv(paste0("Data/",run_name,"/Brazil-0-0-Unknown-Cell-2005.csv"))



outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
  
}


LU <- outputRaster(output2005, "LandUseIndex")
Agri <- outputRaster(output2005, "Capital:Agriculture")
Nat <- outputRaster(output2005, "Capital:Nature")
Infra <- outputRaster(output2005, "Capital:Infrastructure")
OAg <- outputRaster(output2005, "Capital:Other Agriculture")
Aces <- outputRaster(output2005, "Capital:Acessibility")


pl <- list()

#add categories for later plotting etc. (see https://stackoverflow.com/a/37214431)
LU <- ratify(LU)     #tell R that the map raster is categorical 
rat <- levels(LU)[[1]]    #apply the levels (i.e. categories) 
labs <- c("Soy", "Maize", "Double Cropping", "Second N", "Pristine N", "OAgri", "Other", "Pasture")
rat$LandUse <- labs  
levels(LU) <- rat 

LUcols <- c('wheat1', 'wheat2', 'wheat3', 'darkgreen', 'forestgreen', 'gray', 'black', 'orange2')
  
pl[[1]] <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = "Land Use")  


pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)

rl <- list(Agri, Nat, Infra, OAg, Aces)
rl_names <- c("Agriculture C", "Nature C", "Infrastructure C", "Other Agri C", "Accessibility C") 

for(i in seq_along(rl)){

  p <- levelplot(rl[[i]],
  col.regions=pal, 
  contour=F, 
  margin=F,
  main = (rl_names[i]))

  pl[[i+1]] <- p
}

mp <- marrangeGrob(pl, nrow = 3, ncol = 2, top = "2005")


ggsave("plot.jpg", plot = mp, width=25, height=25, units="cm", dpi = 200)





