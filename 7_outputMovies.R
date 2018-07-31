
#Nine raster maps for all cells 
#1. Modelled LC (mode for muni)
#2. Agri Capital (mean for muni)
#3. Nature Capital (mean for muni)
#4. Infrastructure Capital (mean for muni)
#5. Other Agri Capital (mean for muni)
#6. Acessibility - only for raster
#7. LandPrice 
#8. LandProtection 
#9. GrowingSeason

#8 vector maps for munis (in two different plots:
#First plot
#1. Observed LC - only for vector (mode for muni) 
#2. Modelled LC (mode for muni)
#Second plot
#3. Agri Capital (mean for muni)
#4. Nature Capital (mean for muni)
#5. Infrastructure Capital (mean for muni)
#6. LandPrice 
#7. LandProtection 
#8. GrowingSeason


#raster data from cell output data
#muni cata from CRAFTYmunisLC.csv and CRAFTYmunisServCap.csv


rm(list=ls())


library(raster)
library(tidyverse)
library(rasterVis)
library(gridExtra)
library(animation)
library(sf)
library(viridisLite)

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

makeObsLUmap <- function(LU, year) {
  
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

sim_yrs <- seq(2000, 2015, 1)   #movie made for all these years
fig_yrs <- c(2000, 2005, 2010, 2015) #figures output for only these years 

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2018-07-31"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map

#First, Raster maps
mps <- list()
lus <- list()

for(i in seq_along(sim_yrs)){
  
  print(paste0("raster: ", sim_yrs[i]))

  #Load model output data
  output <- read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))

  LU <- outputRaster(output, "LandUseIndex")
  Agri <- outputRaster(output, "Capital:Agriculture")
  Nat <- outputRaster(output, "Capital:Nature")
  Infra <- outputRaster(output, "Capital:Infrastructure")
  OAg <- outputRaster(output, "Capital:Other Agriculture")
  Aces <- outputRaster(output, "Capital:Acessibility")
  Lprice <- outputRaster(output, "Capital:Land Price")
  Lpro <- outputRaster(output, "Capital:Land Protection")
  GrowS <- outputRaster(output, "Capital:Growing Season")
  

  pl <- list()  #this will hold the plots for the all map for this year
  lul <- list()  #this will hold the plots for the LU map for this year
  
  #create the Modelled LU plot and add to the list
  ModLUmap <- makeModLUmap(LU, sim_yrs[i])
  pl[[1]] <- ModLUmap
  
  
  ObsLU <- raster(paste0("Data/ObservedLCmaps/brazillc_",sim_yrs[i],"_",cl,".asc"))

  ObsLUmap <- makeObsLUmap(ObsLU, sim_yrs[i])
  lul[[1]] <- ObsLUmap
  lul[[2]] <- ModLUmap
  
  #now create Capital maps (all with same palette)
  #ras_pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)
  ras_pal <- viridis(100)
  
  rl <- list(Agri, Nat, Infra, OAg, Aces, Lprice, Lpro, GrowS)
  rl_names <- c("Agriculture C", "Nature C", "Infrastructure C", "Other Agri C", "Accessibility C", "Land Price", "Land Protection", "Growing Season") 
  
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

#make videos here by looping through list
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



#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv"))
scDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisServCap.csv"))

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

#yr <- 2005   #for testing

#create land cover palette
lc_pal <- c("forestgreen", "darkcyan", "wheat2", "black", "orange2")
lc_labs <- c("Nature", "Other Agri", "Agriculture", "Other", "Pasture")

#create capitals palette
cap_pal <- viridis(100)
brks <- seq(from=0,to=1,by=0.01)  #101 values

#create figures
for(yr in sim_yrs){
  
  print(paste0("vector: ", yr))

  #if we want this year saved as an image 
  if(yr %in% fig_yrs) {
    
    #land cover map first
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
    
    png(paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_LandUse_",yr,".png"), width=1000, height=1000, res=100)
    
    m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
    layout(mat = m,heights = c(0.8,0.2))

    plot(cDat_map["ObsMode"], pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
    plot(cDat_map["ModMode"], pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
    
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0, lc_labs, fill = lc_pal, title=paste0(yr), horiz = TRUE)
    
    dev.off()

    #now create capital maps 
    scDat_map <- left_join(BRmunis, filter(scDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
    ps <- scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanLandPriceC,meanLandProteC,meanGSeasonC)
    
    png(paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_Capitals_",yr,".png"), width=1200, height=1200, res=100)
    
    m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
    layout(mat = m,heights = c(0.45,0.45,0.1))

    for(psi in 1:6)
    {
      par(mar = c(2,2,1,1))
      plot(ps[psi], pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = T, lty = 0, key.pos=NULL, reset=F)
    }

    #the legend is its own plot https://stackoverflow.com/a/10391001
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0,
      legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr), horiz = TRUE)

    dev.off()
  }
}

#Now create LC video
saveVideo(
  for(yr in sim_yrs){
 
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 

    m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
    layout(mat = m,heights = c(0.8,0.2))

    plot(cDat_map["ObsMode"], pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
    plot(cDat_map["ModMode"], pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
    
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0, lc_labs, fill = lc_pal, title=paste0(yr), horiz = TRUE)

  },
  video.name = paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_LandUse_",scenario,".mp4"))
 


#capitals video
saveVideo(
  for(yr in sim_yrs){
 
    #create capital maps 
    scDat_map <- left_join(BRmunis, filter(scDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
    ps <- scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanLandPriceC,meanLandProteC,meanGSeasonC)
 
    m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
    layout(mat = m,heights = c(0.45,0.45,0.1))

    for(psi in 1:6)
    {
      par(mar = c(2,2,1,1))
      plot(ps[psi], pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = T, lty = 0, key.pos=NULL, reset=F)
    }

    #the legend is its own plot
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0,
      legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr), horiz = TRUE)
 
  },
  video.name = paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_Capitals_",scenario,".mp4"))


#following is how we would do it with ggplot but rendering is tool slow (keep for publications plotting later)
#
# library(ggpubr)
# 
# #lc maps
# om <- ggplot(cDat_map) +
#   geom_sf(aes(fill=as.factor(ObsMode)), color = NA) +
#   scale_fill_manual(values=lc_pal, labels=lc_labs, name="Land Cover") +
#   theme(legend.position = c(0.825, 0.2)) + 
#   ggtitle(paste0(yr," Observed Mode LC"))
#   
# mm <- ggplot(cDat_map) +
#   geom_sf(aes(fill=as.factor(ModMode)), color = NA) +
#   scale_fill_manual(values=lc_pal, labels=lc_labs, name="Land Cover") +
#   theme(legend.position = c(0.825, 0.2)) + 
#   ggtitle(paste0(yr," Modelled Mode LC"))  
# 
# ggarrange(om, mm, nrow = 1, ncol = 2, common.legend=T, legend="bottom")
# 
# ggarrange(om, mm, nrow = 1, ncol = 2, common.legend=T, legend="bottom") %>% 
#   ggsave(filename="tesing2.png")
# 
# 
# #capitals maps
# acm <- ggplot(scDat_map) +
#   geom_sf(aes(fill=meanAgriC), color = NA) +
#   scale_fill_distiller(palette = "YlOrBr", direction = 1, name="Capital") +
#   theme(legend.position = "none") + 
#   ggtitle(paste0(yr," Agriculture C"))
# 
# ncm <- ggplot(scDat_map) +
#   geom_sf(aes(fill=meanNatureC), color = NA) +
#   scale_fill_distiller(palette = "YlOrBr", direction = 1, name="Capital") +
#   theme(legend.position = "none") + 
#   ggtitle(paste0(yr," Nature C"))
# 
# icm <- ggplot(scDat_map) +
#   geom_sf(aes(fill=meanInfraC), color = NA) +
#   scale_fill_distiller(palette = "YlOrBr", direction = 1, name="Capital") +
#   theme(legend.position = "none") + 
#   ggtitle(paste0(yr," Infrastructure C"))
# 
# oacm <- ggplot(scDat_map) +
#   geom_sf(aes(fill=meanOtherAgriC), color = NA) +
#   scale_fill_distiller(palette = "YlOrBr", direction = 1, name="Capital") +
#   theme(legend.position = "none") + 
#   ggtitle(paste0(yr," Other Agri C"))
# 
# system.time(ggarrange(acm, ncm, icm, oacm, nrow = 2, ncol = 3))




