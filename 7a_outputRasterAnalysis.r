#CRAFTYOutput analysis - creates vector maps

#Outputs can be a combination of:
#1. figures showing raster maps of land use and Captials (.png files); years indicated by fig_yrs
#2. movies of 1. (.mp4); if video_output is TRUE
#3. comparison matrices for pairs of maps (.pdf); if comp_matrices is TRUE

#Nine raster maps for all cells 
#1. Modelled LC (mode for muni)
#2. Agri Capital (mean for muni)
#3. Nature Capital (mean for muni)
#4. Port Access Capital (mean for muni)
#5. Other Agri Capital (mean for muni)
#6. Acessibility - only for raster
#7. LandPrice 
#8. LandProtection 
#9. GrowingSeason

#raster data from cell output data


rm(list=ls())

library(raster)
library(diffeR)
library(tidyverse)
library(rasterVis)
library(gridExtra)
library(animation)
library(sf)
library(viridisLite)


sim_yrs <- seq(2001, 2015, 1)   #movie made for all these years
fig_yrs <- c(2001, 2005, 2010, 2015) #figures output for only these years 

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2018-09-27e"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map
maskpath <- "Data/ObservedLCmaps/sim10_BRmunis_latlon_5km.asc"

#outputs to create
video_output <- FALSE
comp_matrices <- TRUE



##FUNCTIONS
#####
#converts data in CRAFTY output file for a single variable and creates a raster
outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
  
}

#LU is a raster object
makeModLUmap <- function(LU, year) {
  
  #add categories for later plotting etc. (see https://stackoverflow.com/a/37214431)
  LU <- ratify(LU)     #tell R that the map raster is categorical 
  rat <- levels(LU)[[1]]    #apply the levels (i.e. categories) 

  #not all LUs may be present so need to create labels and colours dynamically
  uLU <- unique(LU)  
  labs <- c()
  LUcols <- c()
  
  if(-1 %in% uLU) { 
    labs <- c(labs, "LazyFR") 
    LUcols <- c(LUcols, 'deeppink')}
  if(0 %in% uLU) { 
    labs <- c(labs, "Soy") 
    LUcols <- c(LUcols, 'wheat1')}
  if(1 %in% uLU) { 
    labs <- c(labs, "Mze") 
    LUcols <- c(LUcols, 'wheat2')}
  if(2 %in% uLU) { 
    labs <- c(labs, "DblC") 
    LUcols <- c(LUcols, 'wheat3')}
  if(3 %in% uLU) { 
    labs <- c(labs, "SNat") 
    LUcols <- c(LUcols, 'darkgreen')}
  if(4 %in% uLU) { 
    labs <- c(labs, "PNat") 
    LUcols <- c(LUcols, 'forestgreen')}
  if(5 %in% uLU) { 
    labs <- c(labs, "OAg") 
    LUcols <- c(LUcols, 'darkcyan')}
  if(6 %in% uLU) { 
    labs <- c(labs, "O") 
    LUcols <- c(LUcols, 'black')}
  if(7 %in% uLU) { 
    labs <- c(labs, "Pas") 
    LUcols <- c(LUcols, 'orange2')}

  rat$LandUse <- labs  
  levels(LU) <- rat 

  LUmap <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0("Mod LU ",year))  

  return(LUmap)
  
}

makeObsLUmap <- function(LU, year, maskfile) {
  
  maskmap <- raster(maskfile)
  
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
    LUlabs <- c(LUlabs, 'Nat')  }
  if(2 %in% uLU) { 
    LUcols <- c(LUcols, 'darkcyan') 
    LUlabs <- c(LUlabs, 'OAg') }
  if(3 %in% uLU) { 
    LUcols <- c(LUcols, 'wheat2') 
    LUlabs <- c(LUlabs, 'Ag') }
  if(4 %in% uLU) { 
    LUcols <- c(LUcols, 'black') 
    LUlabs <- c(LUlabs, 'Oth') }
  if(5 %in% uLU) { 
    LUcols <- c(LUcols, 'orange2') 
    LUlabs <- c(LUlabs, 'Pas') }
  
  rat$LandUse <- LUlabs  
  levels(LU) <- rat 
  
  p <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0("Obs LU ",year))  

  return(p)
}
##END FUNCTIONS
#####


#First, Raster maps
mps <- list()
lus <- list()
s <- stack()

for(i in seq_along(sim_yrs)){
  
  #i = 1
  
  print(paste0("raster: ", sim_yrs[i]))

  #Load model output data
  output <- read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))

  LU <- outputRaster(output, "LandUseIndex")
  Agri <- outputRaster(output, "Capital:Agriculture")
  Nat <- outputRaster(output, "Capital:Nature")
  Infra <- outputRaster(output, "Capital:Port Access")
  OAg <- outputRaster(output, "Capital:Other Agriculture")
  Aces <- outputRaster(output, "Capital:Nature Access")
  Lprice <- outputRaster(output, "Capital:Land Price")
  Spro <- outputRaster(output, "Capital:Soy Protection")
  GrowS <- outputRaster(output, "Capital:Growing Season")

  #create stack of LU for comparison matrices
  LU[LU == -1] <- NA #remove LazyFR if present
  if(i == 1) { s <- stack(LU) }
  else { s <- stack(s, LU) }
  
  pl <- list()  #this will hold the plots for the all map for this year
  lul <- list()  #this will hold the plots for the LU map for this year
  
  #create the Modelled LU plot and add to the list
  ModLUmap <- makeModLUmap(LU, sim_yrs[i])
  pl[[1]] <- ModLUmap
  
  
  ObsLU <- raster(paste0("Data/ObservedLCmaps/PlantedArea_brazillc_",cl,"_",sim_yrs[i],".asc"))

  ObsLUmap <- makeObsLUmap(ObsLU, sim_yrs[i], maskpath)
  lul[[1]] <- ObsLUmap
  lul[[2]] <- ModLUmap
  
  #ras_pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)
  ras_pal <- viridis(100)
  
  rl <- list(Agri, Nat, Infra, OAg, Aces, Lprice, Spro, GrowS)
  rl_names <- c("Agriculture C", "Nature C", "Port Access C", "Other Agri C", "Accessibility C", "Land Price", "Soy Protection", "Growing Season") 
  
  for(j in seq_along(rl)){
    
    #create the plot
    p <- levelplot(rl[[j]],
    col.regions=ras_pal, 
    at=seq(from=0,to=1,by=0.01), 
    contour=F, 
    margin=F,
    main = (rl_names[j]))
  
    #add it to the list
    pl[[j+1]] <- p    #+1 because LU is in first slot
  }
  
  mps[[i]] <- marrangeGrob(pl, nrow = 3, ncol = 3, top = paste0(sim_yrs[i]))
  lus[[i]] <- marrangeGrob(lul, nrow = 1, ncol = 2, top = paste0(sim_yrs[i]))
  
  #lus[[i]] <- LUmap
  
  #if we want this year saved as an image 
  if(sim_yrs[i] %in% fig_yrs) {
  ggsave(paste0("Data/",scenario,"/",runID,"/",scenario,"_RasterOutput_AllMaps",sim_yrs[i],".png"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)

  ggsave(paste0("Data/",scenario,"/",runID,"/",scenario,"_RasterOutput_LUMap",sim_yrs[i],".png"), plot = lus[[i]], width=20, height=12.5, units="cm", dpi = 300)
  }
    
}


if(comp_matrices)
{
  
  mat_yrs <- head(sim_yrs, -1) #drop last element of the list
  
  
  output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_MapTransitions.pdf")

  pdf(file = output_name)
  
  #next create the comparison matrices for each pair of maps
  for(i in seq_along(mat_yrs)){
    
    #set labels for error matrix (No SNat in yr 2000)  
    if(mat_yrs[i] == sim_yrs[1]) {  LCnames <- c("Soy", "Mze", "Dblc", "PNat", "OAgri","Oth","Pas","SNat")}  
    else { LCnames <- c("Soy", "Mze", "Dblc", "SNat", "PNat", "OAgri","Oth","Pas") }

    xtab <- crosstabm(s[[i]], s[[i+1]])
    colnames(xtab) <- LCnames
    rownames(xtab) <- LCnames
  
    grid.arrange(top = paste0("Transitions ",sim_yrs[i],"-",sim_yrs[i+1]), tableGrob(xtab))  
  }
  
  #close pdf device
  dev.off()
  
}



#make videos here by looping through list
if(video_output)
{
  saveVideo(
    for(i in seq_along(lus)){
      print(lus[[i]])
    },
    video.name = paste0("Data/",scenario,"/",runID,"/",scenario,"_RasterOutput_LandUse_",scenario,".mp4"))
    
  saveVideo(
    for(i in seq_along(mps)){
      print(mps[[i]])
    },
    video.name = paste0("Data/",scenario,"/",runID,"/",scenario,"_RasterOutput_Capitals_",scenario,".mp4"))
}


