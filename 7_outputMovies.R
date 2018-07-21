
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
library(animation)

outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
  
}


plot_yrs <- seq(2000, 2002, 1)
scenario <- "Testing-0-0"

mps <- list()
lus <- list()

for(i in seq_along(plot_yrs)){
  
  #Load model output data
  output <- read_csv(paste0("Data/",run_name,"/",scenario,"-Cell-",plot_yrs[i],".csv"))
  
  LU <- outputRaster(output, "LandUseIndex")
  Agri <- outputRaster(output, "Capital:Agriculture")
  Nat <- outputRaster(output, "Capital:Nature")
  Infra <- outputRaster(output, "Capital:Infrastructure")
  OAg <- outputRaster(output, "Capital:Other Agriculture")
  Aces <- outputRaster(output, "Capital:Acessibility")
  
  pl <- list()  #this will hold the plots for ths year
  
  #add categories for later plotting etc. (see https://stackoverflow.com/a/37214431)
  LU <- ratify(LU)     #tell R that the map raster is categorical 
  rat <- levels(LU)[[1]]    #apply the levels (i.e. categories) 

  #not all LUs may be present so need to create labels and colours dynamically
  uLU <- unique(LU)  
  labs <- c()
  LUcols <- c()
  
  if(0 %in% uLU) { 
    labs <- c(labs, "Soy") 
    LUcols <- c(LUcols, 'wheat1')}
  if(1 %in% uLU) { 
    labs <- c(labs, "Maize") 
    LUcols <- c(LUcols, 'wheat2')}
  if(2 %in% uLU) { 
    labs <- c(labs, "Double Cropping") 
    LUcols <- c(LUcols, 'wheat3')}
  if(3 %in% uLU) { 
    labs <- c(labs, "Second N") 
    LUcols <- c(LUcols, 'darkgreen')}
  if(4 %in% uLU) { 
    labs <- c(labs, "Pristine N") 
    LUcols <- c(LUcols, 'forestgreen')}
  if(5 %in% uLU) { 
    labs <- c(labs, "OAgri") 
    LUcols <- c(LUcols, 'gray')}
  if(6 %in% uLU) { 
    labs <- c(labs, "Other") 
    LUcols <- c(LUcols, 'black')}
  if(7 %in% uLU) { 
    labs <- c(labs, "Pasture") 
    LUcols <- c(LUcols, 'orange2')}

  rat$LandUse <- labs  
  levels(LU) <- rat 

  #create the LU plot and add to the list
  pl[[1]] <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = "Land Use")  

  
  #now create Capital maps (all with same palette)
  pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)
  
  rl <- list(Agri, Nat, Infra, OAg, Aces)
  rl_names <- c("Agriculture C", "Nature C", "Infrastructure C", "Other Agri C", "Accessibility C") 
  
  for(j in seq_along(rl)){
    
    #create the plot
    p <- levelplot(rl[[j]],
    col.regions=pal, 
    contour=F, 
    margin=F,
    main = (rl_names[j]))
  
    #add it to the list
    pl[[j+1]] <- p    #+1 because LU is in furst slot
  }
  
  mps[[i]] <- marrangeGrob(pl, nrow = 3, ncol = 2, top = paste0(plot_yrs[i]))
  lus[[i]] <- pl[[1]]
  
  ggsave(paste0("allplot",plot_yrs[i],".jpg"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)

  ggsave(paste0("LUplot",plot_yrs[i],".jpg"), plot = arrangeGrob(pl[[1]]), width=15, height=15, units="cm", dpi = 300)

    
}


saveVideo(
  for(i in seq_along(lus)){
    lus[[i]]
  },
  video.name = "test_png.mp4")
  



