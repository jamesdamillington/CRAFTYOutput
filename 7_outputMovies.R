
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



library(raster)
library(tidyverse)
library(rasterVis)
library(gridExtra)
library(animation)
library(sf)

library(viridisLite)


outputRaster <- function(data, variable){
  
  out <- data %>%
    dplyr::select(X, Y, !!variable)
  
  ras <- rasterFromXYZ(out)
  
  return(ras)
  
}


sim_yrs <- seq(2000, 2015, 1)   #movie made for all these years
fig_yrs <- c(2000, 2005, 2010, 2015) #figures output for only these years 

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing"
runID <- "0-0"


#First, Raster maps
mps <- list()
lus <- list()

for(i in seq_along(sim_yrs)){
  
  #Load model output data
  output <- read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
  
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
    LUcols <- c(LUcols, 'darkcyan')}
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
  LUmap <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0("Land Use ",sim_yrs[i]))  
  
  #now create Capital maps (all with same palette)
  #ras_pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)
  ras_pal <- viridis(100)
  
  rl <- list(Agri, Nat, Infra, OAg, Aces)
  rl_names <- c("Agriculture C", "Nature C", "Infrastructure C", "Other Agri C", "Accessibility C") 
  
  for(j in seq_along(rl)){
    
    #create the plot
    p <- levelplot(rl[[j]],
    col.regions=ras_pal, 
    contour=F, 
    margin=F,
    main = (rl_names[j]))
  
    #add it to the list
    pl[[j+1]] <- p    #+1 because LU is in furst slot
  }
  
  mps[[i]] <- marrangeGrob(pl, nrow = 3, ncol = 2, top = paste0(sim_yrs[i]))
  lus[[i]] <- LUmap
  
  #if we want this year saved as an image 
  if(sim_yrs[i] %in% fig_yrs) {
  ggsave(paste0("Data/",scenario,"/",runID,"/RasterOutput_AllMaps",sim_yrs[i],".jpg"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)

  ggsave(paste0("Data/",scenario,"/",runID,"/RasterOutput_LUMap",sim_yrs[i],".jpg"), plot = arrangeGrob(lus[[i]]), width=15, height=15, units="cm", dpi = 300)
  }
    
}

#make videos here by looping through list
saveVideo(
  for(i in seq_along(lus)){
    print(lus[[i]])
  },
  video.name = paste0("Data/",scenario,"/",runID,"/RasterOutput_LandUse_",scenario,".mp4"))
  
saveVideo(
  for(i in seq_along(mps)){
    print(mps[[i]])
  },
  video.name = paste0("Data/",scenario,"/",runID,"/RasterOutput_Capitals_",scenario,".mp4"))



#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/CRAFTYmunisLC.csv"))
scDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/CRAFTYmunisServCap.csv"))

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

#yr <- 2005   #for testing

#create figures
for(yr in sim_yrs){

  #if we want this year saved as an image 
  if(yr %in% fig_yrs) {
    
    #land cover map first
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
    #create land cover palette
    lc_pal <- c("forestgreen", "darkcyan", "wheat2", "black", "orange2")
    lc_labs <- c("Nature", "Other Agri", "Agriculture", "Other", "Pasture")
  
    png(paste0("Data/",scenario,"/",runID,"/MuniOutput_LandUse_",yr,".png"), width=1000, height=1000, res=100)
    plot(cDat_map %>% select(ObsMode, ModMode), pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, reset=F)
    legend("bottomright", cex = 1.3, lc_labs, fill = lc_pal, title=paste0(yr))
    dev.off()
    
    #now create capital maps 
    scDat_map <- left_join(BRmunis, filter(scDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
    cap_pal <- viridis(100)
    brks <- seq(from=0,to=1,by=0.01)  #101 values
    
    png(paste0("Data/",scenario,"/",runID,"/MuniOutput_Capitals_",yr,".png"), width=1200, height=1200, res=100)
    plot(scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanOtherAgriC), pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, reset = T)
    legend("right", legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr))
    dev.off()
  }
}

#Now create LC video
saveVideo(
  for(yr in sim_yrs){
 
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 

    #create land cover palette
    lc_pal <- c("forestgreen", "darkcyan", "wheat2", "black", "orange2")
    lc_labs <- c("Nature", "Other Agri", "Agriculture", "Other", "Pasture")
  
    plot(cDat_map %>% select(ObsMode, ModMode), pal = lc_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, reset=F)
    legend("bottomright", cex = 1.3, lc_labs, fill = lc_pal)
    
  },
  video.name = paste0("Data/",scenario,"/",runID,"/MuniOutput_LandUse_",scenario,".mp4"))
 
pr <- par()

#capitals video
saveVideo(
  for(yr in sim_yrs){
 
    #create capital maps 
    scDat_map <- left_join(BRmunis, filter(scDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
    cap_pal <- viridis(100)
    brks <- seq(from=0,to=1,by=0.01)  #101 values
    
    plot(scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanOtherAgriC), pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, reset = T)
    legend("bottom", legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr))

  },
  video.name = paste0("Data/",scenario,"/",runID,"/MuniOutput_Capitals_",scenario,".mp4"))



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




