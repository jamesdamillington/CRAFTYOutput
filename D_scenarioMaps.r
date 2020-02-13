#creates a plot (map) of municipality land cover from model output faceted across years

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

#function to get correct LC pallete colours (e.g. if a given LC is missing) 
lc_pal_function <- function(dat){
  
  pal <- c()
  
  if(is.element(1,unlist(dat))) pal <- c(pal, "coral2")
  if(is.element(2,unlist(dat))) pal <- c(pal, "dodgerblue2")
  if(is.element(3,unlist(dat))) pal <- c(pal, "darkorchid2")
  if(is.element(6,unlist(dat))) pal <- c(pal, "wheat2")
  if(is.element(7,unlist(dat))) pal <- c(pal, "black")
  if(is.element(8,unlist(dat))) pal <- c(pal, "orange2")
  if(is.element(45,unlist(dat))) pal <- c(pal, "forestgreen")

  return(pal)
}


scenario <- "scenario_observed_2018-2035_2020-02-13"
runID <- "0-0"

data_dir <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"
#data_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/CRAFTY_testing/CRAFTYOutput/Data/"
states <- c()  #all states
#states <- c(51) #MT

if(length(states) > 0){
  state_label = paste(c("_States", states), collapse = "-")
} else{ state_label = "_States-All" }


fig_yrs <- c(2023, 2028, 2033) #figures output for only these years [MUST ONLY BE 3 FOR PLOT COD BELOW TO WORK]


SC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisServCap.csv")  #output file name for services and capitals
LC_name <- paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"_CRAFTYmunisLC.csv")  #output file name for land cover data 
  
  
#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), 
    muniID = col_integer(), 
    ModMode = col_integer(),
    cellCount = col_integer(),
    .default = col_double()))  
scDat <- readr::read_csv(SC_name,
  col_types = cols(meanAgriC = col_double(), meanNatureC = col_double(), meanInfraC = col_double(),meanLandPriceC = col_double(),meanSoyProteC = col_double(),meanGSeasonC = col_double()))  #needed to ensure correct import )


#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read(paste0(data_dir,"Vector/BRmunis_sim10_simple2.shp"))

#join simulation data to shapefile
cDat_map <- left_join(BRmunis,cDat, by = c("CD_GEOCMUn" ="muniID")) 
    
#filter to specified states (if needed)
if(!is.null(states)){
  cDat_map <- filter(cDat_map, State %in% states) 
}


#create viridis (divergent) palette: ://stackoverflow.com/questions/37482977/what-is-a-good-palette-for-divergent-colors-in-r-or-can-viridis-and-magma-b
#viridis <- viridis::viridis(11)
#viridis_hcl <- colorspace::sequential_hcl(11,
#  h = c(300, 75), c = c(35, 95), l = c(15, 90), power = c(0.8, 1.2))
  

yr_cDat_map <- cDat_map %>%
  filter(Year %in% fig_yrs)

#create palette before factoring 
temp_pal <- lc_pal_function(yr_cDat_map["ModMode"])

yr_cDat_map <- yr_cDat_map %>%
  mutate(ModMode = if_else(ModMode == 1, "Soy", 
        if_else(ModMode == 2, "Maize",
        if_else(ModMode == 3, "DoubleC",
        if_else(ModMode == 45, "Nature",
        if_else(ModMode == 6, "OtherAgri",
        if_else(ModMode == 7, "Other","Pasture" 
        ))))))) %>%
    mutate(ModMode = factor(ModMode, levels = c("Soy","Maize","DoubleC","OtherAgri","Other","Pasture","Nature"))) 
  

  
#viridis
muni_mode_vir <- yr_cDat_map %>%
  filter(Year %in% fig_yrs) %>%
  ggplot(aes(fill = ModMode, color = ModMode)) +
  geom_sf() +
  theme(axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)) +
  scale_fill_viridis_d() + 
  scale_color_viridis_d() +
  labs(fill="FR", color="FR") + 
  facet_grid(. ~ Year) 

plot(muni_mode_vir)


muni_mode <- yr_cDat_map %>%
  filter(Year %in% fig_yrs) %>%
  ggplot(aes(fill = ModMode, color = ModMode)) +
  geom_sf() +
  theme(axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)) +
  scale_fill_manual(values = temp_pal) + 
  scale_color_manual(values = temp_pal) +
  labs(fill="FR", color="FR") + 
  facet_grid(. ~ Year) 

plot(muni_mode)
  
ggsave(paste0(data_dir,scenario,"/",runID,"/",scenario,state_label,"ScenarionMaps_muniMode.png"), muni_mode, dpi="print")


  

