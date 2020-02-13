#creates a facet plot of model error (vs observation) for 'Arable', 'Pasture', 'Nature' across years

rm(list=ls())

#required packages
library(tidyverse)
library(caret)   #for confusionMatrix
library(diffeR)  #for map comparison
library(gridExtra)  #for printing tables to pdf
library(grid)
library(sf)
library(RColorBrewer)  #for plotting
library(raster)
library(rasterVis)
library(animation)
library(viridisLite)
library(tidyselect)
library(viridis)

scenario <- "testing_demand_smoother3_2020-02-10c"
runID <- "0-0"
class4 <- F


cl <- "PastureB"  #classification for observed LU map
data_dir <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"
#data_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/CRAFTY_testing/CRAFTYOutput/Data/"
states <- c()  #all states
#states <- c(51) #MT

if(length(states) > 0){
  state_label = paste(c("_States", states), collapse = "-")
} else{ state_label = "_States-All" }


fig_yrs <- c(2005, 2010, 2015) #figures output for only these years 

if(class4){
  SC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisServCap_4class.csv")  #output file name for services and capitals
  LC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisLC_4class.csv")  #output file name for land cover data 
} else {
  SC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisServCap.csv")  #output file name for services and capitals
  LC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisLC.csv")  #output file name for land cover data 
} 


  
#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)
scDat <- readr::read_csv(SC_name,
  col_types = cols(meanMoisC = col_double(), meanNatureC = col_double(), meanInfraC = col_double(),meanLandPriceC = col_double(),meanSoyProteC = col_double(),meanGSeasonC = col_double()))  #needed to ensure correct import )

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read(paste0(data_dir,"Vector/BRmunis_sim10_simple2.shp"))

#join simulation data to shapefile
cDat_map <- left_join(BRmunis,cDat, by = c("CD_GEOCMUn" ="muniID")) 
    
#filter to specified states (if needed)
if(!is.null(states)){
  cDat_map <- filter(cDat_map, State %in% states) 
}


#create viridis (divergent) palette: ://stackoverflow.com/questions/37482977/what-is-a-good-palette-for-divergent-colors-in-r-or-can-viridis-and-magma-b
viridis <- viridis::viridis(11)
viridis_hcl <- colorspace::sequential_hcl(11,
  h = c(300, 75), c = c(35, 95), l = c(15, 90), power = c(0.8, 1.2))
  




yr_cDat <- cDat_map %>%
  filter(Year %in% fig_yrs) %>%
  dplyr::select(.,one_of(c("Year","diffcProp3","diffcProp5","diffcProp1"))) %>%
  rename("Arable" = "diffcProp3", "Pasture" = "diffcProp5", "Nature" = "diffcProp1") %>%
  gather(key = "LC", value = "prop", -geometry, -Year) %>%
  mutate(LC = factor(LC, levels=c("Arable", "Pasture", "Nature"))) 
  
muni_prop <- yr_cDat %>%
  ggplot(aes(fill = prop, color = prop)) +
  geom_sf() +
  theme(axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)) +
  scale_fill_viridis() + 
  scale_color_viridis() +
  labs(fill="Proportion", color="Proportion") + 
  facet_grid(LC ~ Year) 

if(class4){  
  ggsave(paste0(data_dir,scenario,"/",runID,"/CalibrationMaps_miniprop_4class.png"), muni_prop, dpi="print")
} else {
  ggsave(paste0(data_dir,scenario,"/",runID,"/CalibrationMaps_miniprop.png"), muni_prop, dpi="print")
}

  

