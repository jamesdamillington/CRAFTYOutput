#CRAFTYOutput analysis - creates vector maps

#Outputs can be a combination of:
#1. figures vector maps of land use and Captials (.png files); years indicated by fig_yrs
#2. movies of 1. (.mp4); if video_output is TRUE

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

#muni cata from CRAFTYmunisLC.csv and CRAFTYmunisServCap.csv

rm(list=ls())

library(tidyverse)
library(gridExtra)
library(animation)
library(sf)
library(viridisLite)
library(RColorBrewer)  #for plotting

sim_yrs <- seq(2001, 2015, 1)   #movie made for all these years
fig_yrs <- c(2005, 2010, 2015) #figures output for only these years 

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2019-02-21"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map

#specify states to plot (for all states provide empty list)
#states <- c()  #all states
states <- c(51) #MT

#outputs to create
video_output <- FALSE


#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv"),
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)

scDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisServCap.csv"),
  col_types = cols(meanAgriC = col_double(), meanNatureC = col_double(), meanInfraC = col_double(),meanLandPriceC = col_double(),meanSoyProteC = col_double(),meanGSeasonC = col_double()))  #needed to ensure correct import 


#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

#yr <- 2005   #for testing

#create default land cover palette
lc_pal <- c("forestgreen", "darkcyan", "wheat2", "black", "orange2")
lc_labs <- c("Nature", "Other Agri", "Agriculture", "Other", "Pasture")

#funciton to get correct LC pallete colours (e.g. if a given LC is missing) 
lc_pal_function <- function(dat){
  
  pal <- c()
  
  if(is.element(1,unlist(dat))) pal <- c(pal, "forestgreen")
  if(is.element(2,unlist(dat))) pal <- c(pal, "darkcyan")
  if(is.element(3,unlist(dat))) pal <- c(pal, "wheat2")
  if(is.element(4,unlist(dat))) pal <- c(pal, "black")
  if(is.element(5,unlist(dat))) pal <- c(pal, "orange2")

  return(pal)
}

#create capitals palette
cap_pal <- viridis(100)
brks <- seq(from=0,to=1,by=0.01)  #101 values


#create proportions palette
cell_pal <- brewer.pal(6, "YlGn")

#create figures
for(yr in sim_yrs){
  
  print(paste0("vector: ", yr))

  #if we want this year saved as an image 
  if(yr %in% fig_yrs) {
    
    #land cover map first
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
    
    #filter to specified states (if needed)
    if(!is.null(states)){
      cDat_map <- filter(cDat_map, State %in% states) 
    }
    
    #open png device
    png(paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_LandUse_",yr,".png"), width=1000, height=1000, res=100)
    
    #create layout (including legend at bottom)
    m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
    layout(mat = m,heights = c(0.8,0.2))

    #add plots
    for(nm in c("ObsMode", "ModMode")){
      lc_pal_temp <- lc_pal_function(cDat_map[nm])
      plot(cDat_map[nm], pal = lc_pal_temp, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
    }

    #add legend
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0, lc_labs, fill = lc_pal, title=paste0(yr), horiz = TRUE)
    
    #close png device
    dev.off()

    
    
    #######
    #now create capital maps 
    scDat_map <- left_join(BRmunis, filter(scDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
    
    #filter to specified states (if needed)
    if(!is.null(states)){
      scDat_map <- filter(scDat_map, State %in% states) 
    }
    
    #select only Capitals we want
    ps <- scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanLandPriceC,meanSoyProteC,meanGSeasonC)
    
    #open png device
    png(paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_Capitals_",yr,".png"), width=1200, height=1200, res=100)
    
    #create layout
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

    
    
    #####
    #comparison maps of obs and mod proportions
    png(paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_LUprops_",yr,".png"), width=1000, height=1000, res=100)

    #create layout
    m <- matrix(c(1,2,3,4,5,6,7,7),nrow = 4,ncol = 2,byrow = TRUE)
    layout(mat = m,heights = c(0.3,0.3,0.3,0.1))
  
    #names columns to map
    propmaps <- c("Obs1","Mod1","Obs3","Mod3","Obs5","Mod5")
    #labels to use on map titles
    proptitles <- c("Obs Nature","Mod Nature","Obs Agri","Mod Agri","Obs Pasture","Mod Pasture")
    
    #plot maps
    for(m in 1:length(propmaps)){
      plot(cDat_map[propmaps[m]], pal = cell_pal, breaks = seq(0,1,0.2), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F, main=proptitles[m])
    }
      
    #plot legend
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0, legend=seq(0,1,0.2), fill=cell_pal, title=paste0(yr), horiz = TRUE)
    
    dev.off()

  }

}

#Now create LC video
if(video_output)
{
  saveVideo(
    for(yr in sim_yrs){
   
      cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
      #filter to specified states (if needed)
      if(!is.null(states)){
        cDat_map <- filter(cDat_map, State %in% states) 
      }
   
      #create layout (including legend at bottom)
      m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
      layout(mat = m,heights = c(0.8,0.2))
  
      #add plots
      for(nm in c("ObsMode", "ModMode")){
        lc_pal_temp <- lc_pal_function(cDat_map[nm])
        plot(cDat_map[nm], pal = lc_pal_temp, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, key.pos=NULL, reset=F)
      }
  
      #add legend
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
      
      #filter to specified states (if needed)
      if(!is.null(states)){
        scDat_map <- filter(scDat_map, State %in% states) 
      }

      
      ps <- scDat_map %>% dplyr::select(meanAgriC, meanNatureC, meanInfraC,meanLandPriceC,meanSoyProteC,meanGSeasonC)
   
      #create layout
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
    
    },
    video.name = paste0("Data/",scenario,"/",runID,"/",scenario,"_MuniOutput_Capitals_",scenario,".mp4"))
}

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


