#Script modified from A_allOutputAnalysis.r (for testing) to be applied to future scenario
#difference is e.g. future scenarios do not need map comparison

#LCcomparisonAnalysis.pdf
#CRAFTYmunisLC.csv
#CRAFTYmunisServCap.csv
#LCcomparisonMaps.pdf
#images of LU maps and Capital maps
#videos of LU maps and Capital maps

#script assumes the entire CRAFTY scenario directory (output data) is present in Data directory (with name format scenario/runID)
#also assumes the input region.csv has been copied to the same directory as output data 



rm(list=ls())

scenario <- "scenario_observed_2001-2035_2020-02-13"
scenario_short <- "scenario constant"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map

path <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"
#path<- "Data/" #where output data have been saved


data_dir <- paste0(path,scenario,"/",runID,"/")  #where output data have been saved
region_filename <- "region2001_2020-02-10b.csv"

#specify states to plot (for all states provide empty list)
states <- c()  #all states
#states <- c(51) #MT

if(length(states) > 0){
  state_label = paste(c("_States", states), collapse = "-")
} else{ state_label = "_States-All" }

yrs <- seq(2001, 2035, 1)       #all years of analysis
sim_yrs <- c(2010, 2018, 2023, 2028, 2033)   #movie made for all these years (will usually be identical to yrs above)
fig_yrs <- c(2010, 2018, 2023, 2028, 2033)  #figures output for only these years 
#fig_yrs <- c(2015, 2016, 2017)  #figures output for only these years 

#calibration analysis output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- T
ras_video_output <- F
muni_video_output <- F
comp_matrices <- F
production <- F

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


#required functions 
#####
getServices <- function(data)
{
  #calculates services in each muni
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(
      sumNatureS = sum(Service.Nature),
      sumSoyS = sum(Service.Soy),
      sumMaizeS = sum(Service.Maize),
      sumAgriS = sum(Service.Soy) + sum(Service.Maize),
      sumPastureS = sum(Service.Pasture),
      sumOtherS = sum(Service.Other),
      sumOtherAgriS = sum(Service.Other.Agriculture),
      meanNatureS = round(mean(Service.Nature),3),
      meanSoyS = round(mean(Service.Soy),3),
      meanMaizeS = round(mean(Service.Maize),3),
      meanAgriS = round((sum(Service.Soy) + sum(Service.Maize)) / (sum(Service.Soy != 0) + sum(Service.Maize != 0)),3),
      meanPastureS = round(mean(Service.Pasture),3),
      meanOtherS = round(mean(Service.Other),3),
      meanOtherAgriS = round(mean(Service.Other.Agriculture),3)
    ) -> services
  
  return(services)
}

getCapitals <- function(data)
{
  #calculates captials in each muni
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(
      sumAgriC = sum(Capital.Moisture),
      sumNatureC = sum(Capital.Nature),
      sumHumanC = sum(Capital.Human),
      sumDevC = sum(Capital.Development),
      sumInfraC = sum(Capital.Port.Access),
      sumEconC = sum(Capital.Economic),
      sumAcessC = sum(Capital.Nature.Access),
      sumGSeasonC = sum(Capital.Growing.Season),
      sumOtherAgriC = sum(Capital.Other.Agriculture),
      sumOtherC = sum(Capital.Other),
      sumSoyProteC = sum(Capital.Soy.Protection),
      sumMaizeProteC = sum(Capital.Maize.Protection),
      sumPasProteC = sum(Capital.Pasture.Protection),
      sumOAgriProteC = sum(Capital.OAgri.Protection),
      meanAgriC = round(mean(Capital.Moisture),3),
      meanNatureC = round(mean(Capital.Nature),3),
      meanHumanC = round(mean(Capital.Human),3),
      meanDevC = round(mean(Capital.Development),3),
      meanInfraC = round(mean(Capital.Port.Access),3),
      meanEconC = round(mean(Capital.Economic),3),
      meanAcessC = round(mean(Capital.Nature.Access),3),
      meanGSeasonC = round(mean(Capital.Growing.Season),3),
      meanOtherAgriC = round(mean(Capital.Other.Agriculture),3),
      meanOtherC = round(mean(Capital.Other),3),
      meanSoyProteC = round(mean(Capital.Soy.Protection),3),
      meanMaizeProteC = round(mean(Capital.Maize.Protection),3),
      meanPasProteC = round(mean(Capital.Pasture.Protection),3),
      meanOAgriProteC = round(mean(Capital.OAgri.Protection),3),
      meanLandPriceC = round(mean(Capital.Land.Price),3)
    ) -> services
  
  return(services)
}

getFRs <- function(data)
{
  LU <- dplyr::select(data, LandUse, muniID)
  
  #calculates proportion of each FR in the muni
  LU %>%
    group_by(muniID) %>%
    dplyr::summarise(FR1 = round(sum(LandUse == 'FR1') / n(), 3),
                     FR2 = round(sum(LandUse == 'FR2') / n(), 3),
                     FR3 = round(sum(LandUse == 'FR3') / n(), 3),
                     FR123 = round(sum(LandUse == 'FR1' | LandUse == 'FR2' | LandUse == 'FR3') / n(), 3),
                     FR4 = round(sum(LandUse == 'FR4') / n(), 3),
                     FR5 = round(sum(LandUse == 'FR5') / n(), 3),
                     FR45 = round(sum(LandUse == 'FR4' | LandUse == 'FR5' | LandUse == 'Lazy FR') / n(), 3),
                     FR6 = round(sum(LandUse == 'FR6') / n(), 3),
                     FR7 = round(sum(LandUse == 'FR7') / n(), 3),
                     FR8 = round(sum(LandUse == 'FR8') / n(), 3)
    ) -> FRs

  return(FRs)
}

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
    LUcols <- c(LUcols, 'coral2')}
  if(1 %in% uLU) { 
    labs <- c(labs, "Mze") 
    LUcols <- c(LUcols, 'dodgerblue2')}
  if(2 %in% uLU) { 
    labs <- c(labs, "DblC") 
    LUcols <- c(LUcols, 'darkorchid2')}
  if(3 %in% uLU) { 
    labs <- c(labs, "Nat") 
    LUcols <- c(LUcols, 'forestgreen')}
  if(4 %in% uLU) { 
    labs <- c(labs, "Nat") 
    LUcols <- c(LUcols, 'forestgreen')}
  if(5 %in% uLU) { 
    labs <- c(labs, "OAg") 
    LUcols <- c(LUcols, 'wheat2')}
  if(6 %in% uLU) { 
    labs <- c(labs, "O") 
    LUcols <- c(LUcols, 'black')}
  if(7 %in% uLU) { 
    labs <- c(labs, "Pas") 
    LUcols <- c(LUcols, 'orange2')}

  rat$LandUse <- labs  
  levels(LU) <- rat 

  LUmap <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0(scenario_short, " - ",year))  

  return(LUmap)
  
}


clipStates <- function(inRaster, statemap, states_ls)
{
    #substitute values so that only specified states have data
    #first create the data frame
    mdf <- data.frame()
    for(m in seq_along(states_ls)) {
      if(m == 1) { 
        mdf <- data.frame(a=states_ls[m], b=1) 
      }  else  {  
        mdf <- rbind(mdf, c(states_ls[m], 1))  
      }
    }
    #now subs in the statemap 
    statemap <- subs(x=statemap, y=mdf, by=1, which=2, subsWithNA=T)
    
    #trim so that statemap is extent of masked data
    smaskmap <- trim(statemap)
    
    #crop pl stack to extent of crop map 
    inRaster <- crop(inRaster, smaskmap)
    
    #mask to extent of desired states
    inRaster <- mask(inRaster, smaskmap)

    return(inRaster)
    
}


read_cDat <- function() {
  
  cDat <- readr::read_csv(LC_name, col_types = cols(
    muniID = col_integer(),	
    FR1	= col_double(),
    FR2	= col_double(),	
    FR3	= col_double(),	
    FR45	= col_double(),
    FR6	= col_double(),	
    FR7	= col_double(),	
    FR8	= col_double(),	
    cellCount = col_integer(),	
    Year = col_integer(),	
    ModMode = col_integer()))
  
  return(cDat)
}


read_scDat <- function(){
  
  scDat <- readr::read_csv(SC_name, col_types = cols(
    meanAgriC = col_double(), 
    meanNatureC = col_double(), 
    meanInfraC = col_double(),
    meanLandPriceC = col_double(),
    meanSoyProteC = col_double(),
    meanGSeasonC = col_double()))  

  return(scDat)
}
#####



#start of 2_CRAFTYsummary_5LCs.r
#####
#output variables
SC_name <- paste0(data_dir,scenario,state_label,"_CRAFTYmunisServCap.csv")  #output file name for services and capitals
LC_name <- paste0(data_dir,scenario,state_label,"_CRAFTYmunisLC.csv")  #output file name for land cover data 

#load the region file (used to match each cell to a municipality)
region <- read.csv(paste0(data_dir,region_filename))


for(i in seq_along(yrs)){
  
  #Load model output data
  output <- read.csv(paste0(data_dir,scenario,"-",runID,"-Cell-",yrs[i],".csv"))
  
  #load municipality cell counts
  lc <- read.csv(paste0(path,"MuniCellCounts.csv"), header = T)

  #create df containing only cell and muni data (rename columns) 
  munis<-data.frame(region$X, region$Y, region$muniID)
  munis <- rename_all(munis, .funs = funs(substring(., 8)))  #string "region." prefix using substring function to retain only chars after 8 position (1 index) 
  #munis <- rename_all(munis, .funs = funs(sub("^region.", "",.)))  #or same as previos line using sub with regex

  #create new column to indicate state (from first two digits of muniID)
  munis <- mutate(munis, State = substring(muniID, 1, 2))
  
  #join to add muniID to the CRAFTY output data
  output <- inner_join(output, munis, by = c("X", "Y"))
  
  ##subset to state if necessary
  if(!is.null(states)){
    output <- filter(output, State %in% states) 
  }
  
  #***Services and Capitals
  #aggregate services and capitals to municipality 
  #(from each row is cell to each row is municipality)
  services <- getServices(output)
  capitals <- getCapitals(output)
  
  #join mapbiomas (lc) and CRAFTY output (mapFRs) summary tables together
  sc <- left_join(lc, services, by = "muniID") %>%
    left_join(., capitals, by = "muniID")
  
  #add year column
  sc<- mutate(sc, Year = yrs[i])
  
  if(i == 1) { scDat <- sc }
  else {  scDat <- bind_rows(scDat, sc) }
  
  
  #***Land Cover
  #aggregate functional roles (indicative of lcs) to municipality 
  #(from each row is cell to each row is municipality)
  FRs <- getFRs(output)
  
  #legend for mapbiomas 
  #1. Nature (FRs 4 and 5, so use FR45)
  #2. Other Agri (FR6)
  #3. Arable FRs 1, 2 and 3, so use FR123)
  #4. Other (FR7)
  #5. PAsture (FR8)

  #subset to get only the FR combos that indicate a specific land cover for municipalities
  selectedFRs <- c("muniID","FR1", "FR2", "FR3", "FR45", "FR6", "FR7", "FR8") #JM! check this works
  mapFRs <- dplyr::select(FRs, selectedFRs)
  
  #join mapbiomas (lc) and CRAFTY output (mapFRs) summary tables together
  lcs <- left_join(mapFRs, lc, by = "muniID")
  
  #add year column
  lcs <- mutate(lcs, Year = yrs[i])
  
  if(i == 1) { lcDat <- lcs }
  else {  lcDat <- bind_rows(lcDat, lcs) }
}

#rename columns using legend above (plus others
 lcDat <- plyr::rename(lcDat, c(
#   "LC1" = "Obs1",
#   "LC2" = "Obs2",
#   "LC3" = "Obs3",
#   "LC4" = "Obs4",
#   "LC5" = "Obs5",
#   "FR45" = "Mod1", 
#   "FR6" = "Mod2", 
#   "FR123" = "Mod3", 
#   "FR7" = "Mod4",
#   "FR8" = "Mod5",
   "NonNAs" = "cellCount"))

#code to add modal cell lc for each muncipality for modelled and observed LUC
#mMode and oMode are characters  (the names of the column with greatest proportion)
lcDat <- mutate(lcDat, mM = names(lcDat)[max.col(lcDat[2:8])+1L])  #from https://stackoverflow.com/a/37197584
#lcDat <- mutate(lcDat, oM = names(lcDat)[max.col(lcDat[7:11])+6L])  #edit 6L to get to right columns

#remove letters from start of mM and oM (returning as integer)
lcDat <- mutate(lcDat, ModMode = as.integer(substring(mM, 3)))
#lcDat <- mutate(lcDat, ObsMode = as.integer(substring(oM, 4)))

#drop column that had letters in
lcDat <- lcDat %>%
  dplyr::select(-c(mM))


#ensure Year is written as integer (see https://github.com/tidyverse/readr/issues/645)
lcDat <- lcDat %>%
  mutate(Year = as.integer(Year))

scDat <- scDat %>%
  mutate(Year = as.integer(Year))

#write data to file
readr::write_csv(scDat, path = SC_name)
readr::write_csv(lcDat, path = LC_name)
##### 

#start of 3_LCcalibrationAnalysis_5LCs.r
#####
if(pdfprint) {
  pdf(file = paste0(data_dir,scenario,state_label,"_LCtimeline.pdf"))
}

cDat <- read_cDat()   #function so col types can be modified once 

#create colours for plot
myCols <- c("coral2","dodgerblue2","darkorchid2","forestgreen","wheat2","black","orange2")
names(myCols) <- c("Soy", "Maize", "DoubleC",'Nature',"OtherAgri","Other","Pasture")


#add state ID
cDat <- cDat %>%
  mutate(state = (muniID %/% 100000)) %>%
  mutate(state = if_else(state == 17, "TO", 
      if_else(state == 29, "BA",
      if_else(state == 31, "MG",
      if_else(state == 35, "SP",
      if_else(state == 41, "PR",
      if_else(state == 42, "SC",
      if_else(state == 43, "RS", 
      if_else(state == 50, "MS",
      if_else(state == 51, "MT",
      if_else(state == 52, "GO", "NA"
      ))))))))))
    )

#timeseries plots
cDat <- cDat %>%
  mutate(Soy.Mod = round(FR1 * cellCount * 25,0)) %>%
  mutate(Mze.Mod = round(FR2 * cellCount * 25,0)) %>%
  mutate(DC.Mod = round(FR3 * cellCount * 25,0)) %>%
  mutate(Nat.Mod = round(FR45 * cellCount * 25,0)) %>%
  mutate(OAgri.Mod = round(FR6 * cellCount * 25,0)) %>%
  mutate(Other.Mod = round(FR7 * cellCount * 25,0)) %>%
  mutate(Pas.Mod = round(FR8 * cellCount * 25,0)) #%>%


#long version
cDat_long_mod <- cDat %>%
  dplyr::select(Year, state:Pas.Mod) %>%
  gather(key = LUC, value = sqkm, -Year, -state) %>% 
  group_by(Year,LUC) %>%
  summarise_at(vars(matches("sqkm")),sum, na.rm=TRUE) %>%
  mutate(Source = "Mod")


cDat_long_mod <- cDat_long_mod %>%
  mutate(LUC = 
      if_else(LUC == "Soy.Mod", "Soy",
        if_else(LUC == "Mze.Mod", "Maize",
          if_else(LUC == "DC.Mod", "DoubleC",
            if_else(LUC == "Nat.Mod", "Nature",
              if_else(LUC == "OAgri.Mod", "OtherAgri",
                if_else(LUC == "Other.Mod", "Other",
                  "Pasture"
    ))))))) %>%
  mutate(LUC = factor(LUC, c(levels = c("Soy","Maize","DoubleC","OtherAgri","Other","Pasture","Nature"))))

c <- cDat_long_mod %>% 
  ggplot(aes(x = Year, y = sqkm,color = LUC, linetype = Source)) + 
  geom_line(size = 1) +
  scale_colour_manual(name = "LUC",values = myCols) +
  scale_y_continuous(name = "Area (sq km)", labels = scales::comma, limits=c(0,2250000)) 
  #ggtitle(scenario_short)
print(c)


if(pdfprint) {
  dev.off()
}
#####

#start of 4_LCcalibrationMaps_5LCs.r
#####

#function to get correct LUC pallete colours (e.g. if a given LUC is missing) 
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

#create default land cover palette
lc_pal <- c("coral2","dodgerblue2","darkorchid2","forestgreen", "wheat2", "black", "orange2")
lc_labs <- c("Soy","Maize","DoubleC","Nature", "Other Agri", "Other", "Pasture")


cDat <- read_cDat()   #function so col types can be modified once 

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read(paste0(path,"Vector/BRmunis_sim10_simple2.shp"))


if(pdfprint) {
  pdf(file = paste0(data_dir,scenario,state_label,"_LCcomparisonMaps.pdf"))
}

## Maps
#loop through years, printing maps
for(yr in fig_yrs){

  cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 

  #filter to specified states (if needed)
  if(!is.null(states)){
    cDat_map <- filter(cDat_map, State %in% states) 
  }
  
  #create layout (including legend at bottom)
  m <- matrix(c(1,2),nrow = 2,ncol = 1,byrow = TRUE)
  layout(mat = m,heights = c(0.9,0.1))
    

  #plot modal modal muni land cover
  temp_pal <- lc_pal_function(cDat_map["ModMode"])  #create land cover palette
  plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LUC"), key.pos = NULL, reset=F)

  #add legend
  par(mar=c(0,0,0,0))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  legend(x = "center",inset = 0, lc_labs, fill = lc_pal, horiz = TRUE)

}

if(pdfprint) {
  dev.off()
}
#####

#start of 7a_outputRasterAnalysis.r
#####
#First, Raster maps
mps <- list()
lus <- list()
lum <- list() #this will hold the plot for the LU map for this year
s <- stack()

for(i in seq_along(sim_yrs)){
  
  print(paste0("raster maps: ", sim_yrs[i]))

  #create state raster map from region file data (in case needed for state output below)
  muniRas <- outputRaster(region, "muniID")
  stateRas <- muniRas %/% 100000   #truncate muniID
  
  #Load model output data
  output <- read_csv(paste0(data_dir,scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
  
  LU <- outputRaster(output, "LandUseIndex")
  Agri <- outputRaster(output, "Capital:Moisture")
  Nat <- outputRaster(output, "Capital:Nature")
  Infra <- outputRaster(output, "Capital:Port Access")
  OAg <- outputRaster(output, "Capital:Other Agriculture")
  Aces <- outputRaster(output, "Capital:Nature Access")
  Lprice <- outputRaster(output, "Capital:Land Price")
  Spro <- outputRaster(output, "Capital:Soy Protection")
  GrowS <- outputRaster(output, "Capital:Growing Season")

  print("readLU")
  #create stack of LU for comparison matrices
  LU[LU == -1] <- 3 #set LazyFR to SNat
  if(i == 1) { s <- stack(LU) }
  else { s <- stack(s, LU) }

  pl <- list()  #this will hold the plots for the all map for this year
  lul <- list()  #this will hold the plots for the LU-Agri map for this year
  
  #mask if specific states are desired
  if(!is.null(states)){
    LU<- clipStates(LU, stateRas, states) }
    
  #create the Modelled LU plot and add to the list
  ModLUmap <- makeModLUmap(LU, sim_yrs[i])
  pl[[1]] <- ModLUmap
  
  #print("readObsLU")
  #ObsLU <- raster(paste0("Data/ObservedLCmaps/PlantedArea_brazillc_",cl,"_",sim_yrs[i],".asc"))

  #ObsLUmap <- makeObsLUmap(ObsLU, sim_yrs[i])
  #lul[[1]] <- ObsLUmap
  #lul[[2]] <- ModLUmap
  
  #now create Capital maps (all with same palette)
  #ras_pal <- colorRampPalette(brewer.pal(9,"YlOrBr"))(100)
  ras_pal <- viridis(100)
  
  rl <- list(Agri, Nat, Infra, OAg, Aces, Lprice, Spro, GrowS)
  rl_names <- c("Moisture C", "Nature C", "Port Access C", "Other Agri C", "Accessibility C", "Land Price", "Soy Protection", "Growing Season") 
  
  #mask the maps if specific states are desired
  if(!is.null(states)){
    for(r in seq_along(rl)) {
      rl[[r]] <- clipStates(rl[[r]], stateRas, states)
    }
  }
  
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
  
  print("marrange")
  
  mps[[i]] <- marrangeGrob(pl, nrow = 3, ncol = 3, top = paste0(sim_yrs[i]))

  #now create LU map with AgriCap map
  lul[[1]] <- ModLUmap
  lul[[2]] <- pl[[2]] #this should be the AgriCap map

  lus[[i]] <- marrangeGrob(lul, nrow = 1, ncol = 2, top = paste0(sim_yrs[i]))
  
  #and the LU map
  lum[[i]] <- ModLUmap
  
  #if we want this year saved as an image 
  if(sim_yrs[i] %in% fig_yrs) {
  
    ggsave(paste0(data_dir,scenario,state_label,"_Raster_AllMaps",sim_yrs[i],".png"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)
    ggsave(paste0(data_dir,scenario,state_label,"_Raster_LU-Agri-Map",sim_yrs[i],".png"), plot = lus[[i]], width=20, height=12.5, units="cm", dpi = 300)
    #png(paste0(data_dir,scenario,state_label,"_Raster_LandUse-Map",sim_yrs[i],".png"), width=1200, height=1200, res=100)
  }
    
}


if(comp_matrices)
{
  
  mat_yrs <- head(sim_yrs, -1) #drop last element of the list
  
  
  output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,state_label,"_MapTransitions.pdf")

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


if(ras_video_output)
{
  saveVideo(
    for(i in seq_along(mps)){
      print(mps[[i]])
    },
    video.name = paste0(data_dir,scenario,state_label,"_Raster_Capitals.mp4"))
  
  saveVideo(
    for(i in seq_along(lus)){
      print(lus[[i]])
    },
    video.name = paste0(data_dir,scenario,state_label,"_Raster_LU-Agri.mp4"))

    saveGIF(
    for(i in seq_along(lum)){
      print(lum[[i]])
    },
    movie.name = paste0(data_dir,scenario,state_label,"_Raster_LandUse.gif"),
      interval=2,
      loop=T)
  
}


#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- read_cDat()   #function so col types can be modified once 

scDat <- read_scDat() #function so col types can be modified once 

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")

#create proportions palette
cell_pal <- brewer.pal(6, "YlGn")

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
    
    #filter to specified states (if needed)
    if(!is.null(states)){
      cDat_map <- filter(cDat_map, State %in% states) 
    }
    
    #open png device
    png(paste0("Data/",scenario,"/",runID,"/",scenario,state_label,"_MuniOutput_LandUse_",yr,".png"), width=1000, height=1000, res=100)
    
    #create layout (including legend at bottom)
    m <- matrix(c(1,2),nrow = 2,ncol = 1,byrow = TRUE)
    layout(mat = m,heights = c(0.9,0.1))
    
    
    #plot modal modal muni land cover
    temp_pal <- lc_pal_function(cDat_map["ModMode"])  #create land cover palette
    plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LUC"), key.pos = NULL, reset=F)

    #add legend
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    legend(x = "center",inset = 0, lc_labs, fill = lc_pal, horiz = TRUE)

    
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
    png(paste0("Data/",scenario,"/",runID,"/",scenario,state_label,"_MuniOutput_Capitals_",yr,".png"), width=1200, height=1200, res=100)
    
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
    png(paste0("Data/",scenario,"/",runID,"/",scenario,state_label,"_MuniOutput_LUprops_",yr,".png"), width=1000, height=1000, res=100)

    #create layout
    m <- matrix(c(1,2,3,4,5,6,7,7,7),nrow = 3,ncol = 3,byrow = TRUE)
    layout(mat = m,heights = c(0.45,0.45,0.1))
    #layout(mat = m,heights = c(lcm(10),lcm(10),lcm(2)))
  
    #names columns to map
    propmaps <- c("FR1","FR2","FR3","FR45","FR6","FR8")
    #labels to use on map titles
    proptitles <- c("Soy","Maize","DoubleC","Nature","O Agri"," Pasture")
    
    #plot maps
    for(m in 1:length(propmaps)){
      #plot(cDat_map[propmaps[m]], pal = cell_pal, breaks = seq(0,1,0.2), graticule = st_crs(cDat_map), axes = TRUE, lty=0, key.pos=NULL, reset=F, main=proptitles[m])
      plot(cDat_map[propmaps[m]], pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = TRUE, lty=0, key.pos=NULL, reset=F, main=proptitles[m])
    }
      
    #plot legend
    par(mar=c(0,0,0,0))
    plot(1, type = "n", axes=FALSE, xlab="", ylab="")
    #legend(x = "center",inset = 0, legend=seq(0,1,0.2), fill=cell_pal, title=paste0(yr), horiz = TRUE)
    legend(x = "center",inset = 0,
      legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr), horiz = TRUE)
    
    dev.off()

  }
}


if(muni_video_output)
{
  #Now create LUC video
  saveVideo(
    for(yr in sim_yrs){
   
     cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
      #filter to specified states (if needed)
      if(!is.null(states)){
        cDat_map <- filter(cDat_map, State %in% states) 
      }
   
      #create layout (including legend at bottom)
      m <- matrix(c(1,2),nrow = 2,ncol = 1,byrow = TRUE)
      layout(mat = m,heights = c(0.9,0.1))
      
      #plot modal modal muni land cover
      temp_pal <- lc_pal_function(cDat_map["ModMode"])  #create land cover palette
      plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LUC"), key.pos = NULL, reset=F)
  
      #add legend
      par(mar=c(0,0,0,0))
      plot(1, type = "n", axes=FALSE, xlab="", ylab="")
      legend(x = "center",inset = 0, lc_labs, fill = lc_pal, horiz = TRUE)
       
    },
    video.name = paste0(data_dir,scenario,state_label,"_MuniOutput_LandUse.mp4"))
   

  
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
    video.name = paste0(data_dir,scenario,state_label,"_MuniOutput_Capitals.mp4"))

    #proportions video
    saveVideo(
      for(yr in sim_yrs){
   
       cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
    
        #filter to specified states (if needed)
        if(!is.null(states)){
          cDat_map <- filter(cDat_map, State %in% states) 
        }
   
        #create layout
        m <- matrix(c(1,2,3,4,4,4),nrow = 2,ncol = 3,byrow = TRUE)
        layout(mat = m,heights = c(0.8,0.2))
        #layout(mat = m,heights = c(lcm(15),lcm(2)))
      
        #names columns to map
        propmaps <- c("Mod1","Mod3","Mod5")
        #labels to use on map titles
        proptitles <- c("Mod Nature","Mod Agri","Mod Pasture")
        
        #plot maps
        for(m in 1:length(propmaps)){
          #plot(cDat_map[propmaps[m]], pal = cell_pal, breaks = seq(0,1,0.2), graticule = st_crs(cDat_map), axes = TRUE, lty=0, key.pos=NULL, reset=F, main=proptitles[m])
          plot(cDat_map[propmaps[m]], pal = cap_pal, breaks = brks, graticule = st_crs(cDat_map), axes = TRUE, lty=0, key.pos=NULL, reset=F, main=proptitles[m])
        }
      
        #plot legend
        par(mar=c(0,0,0,0))
        plot(1, type = "n", axes=FALSE, xlab="", ylab="")
        #legend(x = "center",inset = 0, legend=seq(0,1,0.2), fill=cell_pal, title=paste0(yr), horiz = TRUE)
        legend(x = "center",inset = 0,
          legend=seq(1,0,-0.1), fill=rev(viridis(11)), title=paste0(yr), horiz = TRUE)
  
    },
    video.name = paste0(data_dir,scenario,state_label,"_MuniOutput_LUprops.mp4"))

}


if(production)
{

#start of 8_analyseProduction.r
#####

#don't do this analysis if only creating output for individual states
if(length(states) == 0)
{
  output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_ProductionAnalysis_allBrazil.pdf")
  outputcsv_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_ProductionAnalysis_allBrazil.csv")
  
  #for reading InternalDemand, from https://stackoverflow.com/a/17289991
  read.tcsv = function(file, header=TRUE, sep=",", ...) {
  
    n = max(count.fields(file, sep=sep), na.rm=TRUE)
    x = readLines(file)
  
    .splitvar = function(x, sep, n) {
      var = unlist(strsplit(x, split=sep))
      length(var) = n
      return(var)
    }
  
    x = do.call(cbind, lapply(x, .splitvar, sep=sep, n=n))
    x = apply(x, 1, paste, collapse=sep) 
    out = read.csv(text=x, sep=sep, header=header, ...)
    return(out)
  
  }
  
  odata <- read_csv("Data/Production_Export_allBrazil.csv")
  
  obs_data <- odata %>%
    filter(Year <= 2015) %>%
    dplyr::select(-ends_with("export_China_gg")) %>%
    rename(year = Year)
  
  Dairy_long <- obs_data %>%
    dplyr::select(year, starts_with("Dairy")) %>%
    gather(key = measure, value = value_gg, -year) %>%
    mutate(commodity = "Dairy") %>%
    mutate(measure =
        if_else(grepl("Production", measure), "Production",
          if_else(grepl("export", measure), "Export", "Internal")
          )
      )
  
  Maize_long <- obs_data %>%
    dplyr::select(year, starts_with("Maize")) %>%
    gather(key = measure, value = value_gg, -year) %>%
    mutate(commodity = "Maize") %>%
    mutate(measure = 
        if_else(grepl("Production", measure), "Production", 
          if_else(grepl("export", measure), "Export", "Internal")
          ) 
      )
  
  Meat_long <- obs_data %>%
    dplyr::select(year, starts_with("Meat")) %>%
    gather(key = measure, value = value_gg, -year) %>%
    mutate(commodity = "Meat") %>%
    mutate(measure = 
        if_else(grepl("Production", measure), "Production", 
          if_else(grepl("export", measure), "Export", "Internal")
          ) 
      )
  
  Soy_long <- obs_data %>%
    dplyr::select(year, starts_with("Soy")) %>%
    gather(key = measure, value = value_gg, -year) %>%
    mutate(commodity = "Soy") %>%
    mutate(measure = 
        if_else(grepl("Production", measure), "Production", 
          if_else(grepl("export", measure), "Export", "Internal")
          ) 
      )
  
  obs_long <- bind_rows(Dairy_long, Maize_long, Soy_long, Meat_long) %>%
   mutate(Source = "Obs")
  
  
  #empty table to populate from files below
  mod_dat <- data.frame(
      commodity = character(),
      measure = character(),
      year = integer(),
      value_gg = numeric()
      
    )
  tbl_df(mod_dat)
  
  #loop through all files 
  for(i in seq_along(yrs)){
   
    filen <- paste0("0_FromMaestro",yrs[i],"_",scenario,".csv")
    
    dat <- read_csv(paste0("Data/",scenario,"/StellaData/",filen),col_names=F)
  
    mod_dat <- mod_dat %>% 
      add_row(commodity = "Soy", measure = "Production", year = yrs[i], value_gg = as.numeric(dat[1,2])) %>%
      add_row(commodity = "Soy", measure = "Storage", year = yrs[i], value_gg = as.numeric(dat[4,2])) %>%
      add_row(commodity = "Soy", measure = "Export", year = yrs[i], value_gg = as.numeric(dat[3,2])) %>%
      add_row(commodity = "Maize", measure = "Production", year = yrs[i], value_gg = as.numeric(dat[6,2])) %>%
      add_row(commodity = "Maize", measure = "Storage", year = yrs[i], value_gg = as.numeric(dat[9,2])) %>%
      add_row(commodity = "Maize", measure = "Export", year = yrs[i], value_gg = as.numeric(dat[8,2])) %>%
      add_row(commodity = "Meat", measure = "Production", year = yrs[i], value_gg = as.numeric(dat[11,2])) %>%
      add_row(commodity = "Meat", measure = "Export", year = yrs[i], value_gg = as.numeric(dat[14,2])) %>%
      add_row(commodity = "Dairy", measure = "Production", year = yrs[i], value_gg = as.numeric(dat[12,2])) %>%
      add_row(commodity = "Dairy", measure = "Export", year = yrs[i], value_gg = as.numeric(dat[16,2]))
  
  }
  
  #needed to prevent bind_rows error below
  mod_dat <- mod_dat %>%
    mutate(measure = as.character(measure), commodity = as.character(commodity))
  
  
  #get internal demand data
  internal <- read.tcsv(paste0("Data/",scenario,"/StellaData/InternalCRAFTY_2001_2019-08-14_to2030.csv"))
  internal <- internal %>%
    rename(year = 1, Soy = 2, Maize = 3, Meat = 4, Dairy = 5)
  
  internal <- internal %>%
    gather(key = commodity, value = value_gg, -year) %>%
    mutate(measure = "IntDemand") %>%
    dplyr::select(commodity, measure, year, value_gg)
  
  
  #get external demand data
  external <- read.tcsv(paste0("Data/",scenario,"/StellaData/ToCRAFTY.csv"))
  external <- external %>%
    rename(year = 1, Soy = 2, Maize = 3, Meat = 4, Dairy = 5)
  
  external <- external %>%
    gather(key = commodity, value = value_gg, -year) %>%
    mutate(measure = "ExtDemand") %>%
    dplyr::select(commodity, measure, year, value_gg)
  
  
  #combine
  mod_dat <- bind_rows(mod_dat, internal, external)  %>%
    mutate(Source = "Modelled")
  
  mod_dat <- mod_dat %>%
    dplyr::select(year, commodity, measure, Source, value_gg) 
  
  obs_long <- obs_long %>%
    dplyr::select(year, commodity, measure, Source, value_gg)
  
  
  all_dat <- bind_rows(mod_dat, obs_long) %>%
    mutate(source = factor(Source), measure = factor(measure), commodity = factor(commodity))
    
  #write all_dat to csv
  write_csv(all_dat, outputcsv_name)
  
  
  #CRAFTY demand - add code here
  #empty table to populate from files below
  crafty_dat <- data.frame(
      commodity = character(),
      measure = character(),
      year = integer(),
      value_cells = numeric()
      
    )
  tbl_df(crafty_dat)
  
  for(i in seq_along(yrs)){
   
    filen <- paste0("0_FromMaestro",yrs[i],"_",scenario,".csv")
    
    dat <- read_csv(paste0("Data/",scenario,"/StellaData/",filen),col_names=F)
  
    crafty_dat <- crafty_dat %>% 
      add_row(commodity = "Soy", measure = "Demand", year = yrs[i], value_cells = as.numeric(dat[16,2])) %>%
      add_row(commodity = "Maize", measure = "Demand", year = yrs[i], value_cells = as.numeric(dat[17,2])) %>%
      add_row(commodity = "Meat", measure = "Demand", year = yrs[i], value_cells = as.numeric(dat[18,2])) %>%
      add_row(commodity = "Nature", measure = "Demand", year = yrs[i], value_cells = as.numeric(dat[20,2])) %>%
      add_row(commodity = "OAgri", measure = "Demand", year = yrs[i], value_cells = as.numeric(dat[21,2]))
  }
  
  
  
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")
  
  
  if(pdfprint) {
    pdf(file = output_name)
  }
  
  #now plot
  #timelines of production, storage, export by commodity
  a <- all_dat %>% 
    filter(commodity == "Soy") %>%
    ggplot(aes(x=year, y=value_gg, color=measure, linetype=Source)) +
    geom_line() +
    scale_colour_manual(values=cbPalette) +
    scale_y_continuous(labels = scales::comma, limits=c(0,150000)) +
    ylab("Value (gg)") +
    xlab("Year") +
    ggtitle("Soy") 
  print(a)
  
  
  a <- all_dat %>% 
    filter(commodity == "Maize") %>%
    ggplot(aes(x=year, y=value_gg, color=measure, linetype=Source)) +
    geom_line() +
    scale_colour_manual(values=cbPalette) +
    scale_y_continuous(labels = scales::comma, limits=c(0,150000)) +
    ylab("Value (gg)") +
    xlab("Year") +
    ggtitle("Maize") 
  print(a)
  
  a <- all_dat %>% 
    filter(commodity == "Meat") %>%
    ggplot(aes(x=year, y=value_gg, color=measure, linetype=Source)) +
    geom_line() +
    scale_colour_manual(values=cbPalette) +
    scale_y_continuous(labels = scales::comma, limits=c(0,15000)) +
    ylab("Value (gg)") +
    xlab("Year") +
    ggtitle("Meat") 
  print(a)
  
  # a <- all_dat %>%
  #   filter(commodity == "Dairy") %>%
  #   ggplot(aes(x=year, y=value_gg, color=measure, linetype=Source)) +
  #   geom_line() +
  #   scale_colour_manual(values=cbPalette) +
  #   ylab("Value (gg)") +
  #   xlab("Year") +
  #   ggtitle("Dairy")
  # print(a)
  
  c <- crafty_dat %>% 
    ggplot(aes(x = year, y = value_cells, fill = commodity)) + 
    geom_bar(position = "fill",stat = "identity", colour="white") +
    scale_y_continuous(name = "Proportion of Total", labels = scales::percent_format()) +
    ggtitle("CRAFTY Demand")
  print(c)
  
  c <- crafty_dat %>% 
    ggplot(aes(x = year, y = value_cells, fill = commodity)) + 
    geom_bar(stat = "identity", colour="white") +
    scale_y_continuous(name = "Cells", labels = scales::comma) +
    ggtitle("CRAFTY Demand")
  print(c)
  
  c <- crafty_dat %>% 
    ggplot(aes(x = year, y = value_cells, colour = commodity)) + 
    geom_line() +
    scale_y_continuous(name = "Cells", labels = scales::comma) +
    ggtitle("CRAFTY Demand")
  print(c)
  
  
  if(pdfprint) {
    dev.off()
  }
  
}
  
} #end Production
#####

dev.off() #reset par https://stackoverflow.com/a/31909011
