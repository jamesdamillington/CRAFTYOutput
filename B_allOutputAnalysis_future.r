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

scenario <- "ClimateRCP26_DC_DemandConst2015"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map

data_dir <- paste0("Data/",scenario,"/",runID,"/")  #where output data have been saved
region_filename <- "region2015.csv"

#specify states to plot (for all states provide empty list)
states <- c()  #all states
#states <- c(51) #MT

if(length(states) > 0){
  state_label = paste(c("_States", states), collapse = "-")
} else{ state_label = "_States-All" }

yrs <- seq(2015, 2017, 1)       #all years of analysis
sim_yrs <- seq(2015, 2017, 1)   #movie made for all these years (will usually be identical to yrs above)
#fig_yrs <- c(2020, 2025, 2030)  #figures output for only these years 
fig_yrs <- c(2015, 2016, 2017)  #figures output for only these years 

#calibration analysis output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE
ras_video_output <- TRUE
muni_video_output <- FALSE
comp_matrices <- TRUE


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
      sumAgriC = sum(Capital.Agriculture),
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
      meanAgriC = round(mean(Capital.Agriculture),3),
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
                     FR45 = round(sum(LandUse == 'FR4' | LandUse == 'FR5') / n(), 3),
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
    labs <- c(labs, "SNat") 
    LUcols <- c(LUcols, 'darkgreen')}
  if(4 %in% uLU) { 
    labs <- c(labs, "PNat") 
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

  LUmap <- levelplot(LU, att = "LandUse", col.regions=LUcols, main = paste0("Mod LU ",year))  

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
  lc <- read.csv(paste0("Data/MuniCellCounts.csv"), header = T)

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
   "NonNAs" = "cellCount",
   "NAs" = "NAcellCount"))

#code to add modal cell lc for each muncipality for modelled and observed LC
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

cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer())) #needed to ensure correct import (many zeros in diffcProp3 at top of file)

## Mode analysis

#3. maps of modes , modelled 



## Proportions


#create colours for plot
myCols <- c("coral2","dodgerblue2","darkorchid2","forestgreen","wheat2","orange2")
names(myCols) <- c("Soy", "Maize", "DoubleC",'Nature',"OtherAgri","Pasture")


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
  mutate(Soy.Mod = round(FR1 * cellCount,0)) %>%
  mutate(Mze.Mod = round(FR2 * cellCount,0)) %>%
  mutate(DC.Mod = round(FR3 * cellCount,0)) %>%
  mutate(Nat.Mod = round(FR45 * cellCount,0)) %>%
  mutate(OAgri.Mod = round(FR6 * cellCount,0)) %>%
  mutate(Other.Mod = round(FR7 * cellCount,0)) %>%
  mutate(Pas.Mod = round(FR8 * cellCount,0)) #%>%


#long version
cDat_long_mod <- cDat %>%
  dplyr::select(Year, state:Pas.Mod) %>%
  gather(key = LC, value = cells, -Year, -state) %>% 
  group_by(Year,LC) %>%
  summarise_at(vars(matches("cells")),sum) %>%
  mutate(source = "Mod")


cDat_long_mod <- cDat_long_mod %>%
  mutate(LC = 
      if_else(LC == "Soy.Mod", "Soy",
        if_else(LC == "Mze.Mod", "Maize",
          if_else(LC == "DC.Mod", "DoubleC",
            if_else(LC == "Nat.Mod", "Nature",
              if_else(LC == "OAgri.Mod", "OtherAgri",
                if_else(LC == "Other.Mod", "Other",
                  "Pasture"
                )
              )
            )
          )
        )
      )
    )
    

c <- cDat_long_mod %>% 
  ggplot(aes(x = Year, y = cells,color = LC, linetype = source)) + 
  geom_line(size = 1) +
  scale_colour_manual(name = "LC",values = myCols) +
  scale_y_continuous(name = "Cells", labels = scales::comma) +
  ggtitle("CRAFTY Output")
print(c)


if(pdfprint) {
  dev.off()
}
#####

#start of 4_LCcalibrationMaps_5LCs.r
#####

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

#create default land cover palette
lc_pal <- c("coral2","dodgerblue2","darkorchid2","forestgreen", "wheat2", "black", "orange2")
lc_labs <- c("Soy","Maize","DoubleC","Nature", "Other Agri", "Other", "Pasture")


cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer())) #needed to ensure correct import (many zeros in diffcProp3 at top of file)

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")


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
  plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LC"), key.pos = NULL, reset=F)

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
s <- stack()

for(i in seq_along(sim_yrs)){
  
  print(paste0("raster maps: ", sim_yrs[i]))

  #create state raster map from region file data (in case needed for state output below)
  muniRas <- outputRaster(region, "muniID")
  stateRas <- muniRas %/% 100000   #truncate muniID
  
  #Load model output data
  output <- read_csv(paste0(data_dir,scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
  
  LU <- outputRaster(output, "LandUseIndex")
  Agri <- outputRaster(output, "Capital:Agriculture")
  Nat <- outputRaster(output, "Capital:Nature")
  Infra <- outputRaster(output, "Capital:Port Access")
  OAg <- outputRaster(output, "Capital:Other Agriculture")
  Aces <- outputRaster(output, "Capital:Nature Access")
  Lprice <- outputRaster(output, "Capital:Land Price")
  Spro <- outputRaster(output, "Capital:Soy Protection")
  GrowS <- outputRaster(output, "Capital:Growing Season")

  print("readLU")
  #create stack of LU for comparison matrices
  LU[LU == -1] <- NA #remove LazyFR if present
  if(i == 1) { s <- stack(LU) }
  else { s <- stack(s, LU) }

  pl <- list()  #this will hold the plots for the all map for this year
  lul <- list()  #this will hold the plots for the LU map for this year
  
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
  rl_names <- c("Agriculture C", "Nature C", "Port Access C", "Other Agri C", "Accessibility C", "Land Price", "Soy Protection", "Growing Season") 
  
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
  
  
  #if we want this year saved as an image 
  if(sim_yrs[i] %in% fig_yrs) {
  
    ggsave(paste0(data_dir,scenario,state_label,"_Raster_AllMaps",sim_yrs[i],".png"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)
    ggsave(paste0(data_dir,scenario,state_label,"_Raster_LUMap",sim_yrs[i],".png"), plot = lus[[i]], width=20, height=12.5, units="cm", dpi = 300)
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
    video.name = paste0(data_dir,scenario,state_label,"_Raster_LandUse.mp4"))
  
}


#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)
scDat <- readr::read_csv(SC_name,
  col_types = cols(meanAgriC = col_double(), meanNatureC = col_double(), meanInfraC = col_double(),meanLandPriceC = col_double(),meanSoyProteC = col_double(),meanGSeasonC = col_double()))  #needed to ensure correct import )

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
    plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LC"), key.pos = NULL, reset=F)

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
    m <- matrix(c(1,2,3,4,4,4),nrow = 2,ncol = 3,byrow = TRUE)
    #layout(mat = m,heights = c(0.9,0.1))
    layout(mat = m,heights = c(lcm(15),lcm(2)))
  
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
    
    dev.off()

  }
}


if(muni_video_output)
{
  #Now create LC video
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
      plot(cDat_map["ModMode"], pal = temp_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LC"), key.pos = NULL, reset=F)
  
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



#start of 8_analyseProduction.r
#####

#don't do this analysis if only creating output for individual states
if(length(states) == 0)
{
  output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_ProductionAnalysis_NoSTELLA.pdf")

  #empty table to populate from files below
  mod_dat <- data.frame(
      service = character(),
      measure = character(),
      year = integer(),
      value = numeric()

    )
  tbl_df(mod_dat)

  for(i in seq_along(sim_yrs)){
    
    #i <- 2
    #Load model output data
    output <- read.csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
    
    Soy <- output %>%
      filter(`Service.Soy` > 0) %>%
      summarise(mn = mean(`Service.Soy`), sm = sum(`Service.Soy`), mm = min(`Service.Soy`), mx = max(`Service.Soy`), sd = sd(`Service.Soy`))
  
    Maize <- output %>%
      filter(`Service.Maize` > 0) %>%
      summarise(mn = mean(`Service.Maize`), sm = sum(`Service.Maize`), mm = min(`Service.Maize`), mx = max(`Service.Maize`), sd = sd(`Service.Maize`))
  
    Nature <- output %>%
      filter(`Service.Nature` > 0) %>%
      summarise(mn = mean(`Service.Nature`), sm = sum(`Service.Nature`), mm = min(`Service.Nature`), mx = max(`Service.Nature`), sd = sd(`Service.Nature`))
  
    OAgri <- output %>%
      filter(`Service.Other.Agriculture` > 0) %>%
      summarise(mn = mean(`Service.Other.Agriculture`), sm = sum(`Service.Other.Agriculture`), mm = min(`Service.Other.Agriculture`), mx = max(`Service.Other.Agriculture`), sd = sd(`Service.Other.Agriculture`))
  
    Other <- output %>%
      filter(`Service.Other` > 0) %>%
      summarise(mn = mean(`Service.Other`), sm = sum(`Service.Other`), mm = min(`Service.Other`), mx = max(`Service.Other`), sd = sd(`Service.Other`))
  
    Pasture <- output %>%
      filter(`Service.Pasture` > 0) %>%
      summarise(mn = mean(`Service.Pasture`), sm = sum(`Service.Pasture`), mm = min(`Service.Pasture`), mx = max(`Service.Pasture`), sd = sd(`Service.Pasture`))
  
    
    lserv <- c("Soy", "Maize", "Pasture")
    
    for(j in lserv) {
  
        
    mod_dat <- mod_dat %>%
      add_row(year = sim_yrs[i], service = j, measure = "Mean", value = round(UQ(as.name(j))$mn,3)) %>%
      add_row(year = sim_yrs[i], service = j, measure = "Sum", value = round(UQ(as.name(j))$sm,3)) %>%
      add_row(year = sim_yrs[i], service = j, measure = "Min", value = round(UQ(as.name(j))$mm,3)) %>%
      add_row(year = sim_yrs[i], service = j, measure = "Max", value = round(UQ(as.name(j))$mx,3)) %>%
      add_row(year = sim_yrs[i], service = j, measure = "SD", value = round(UQ(as.name(j))$sd,3)) 
    
    }
  }

  mod_serv <- mod_dat %>%
  filter(measure == "Sum") %>%
  #filter(service != "Other", service != "OAgri") %>%
  rename(commodity = service) %>%
  mutate(measure = "Production") %>%
  mutate(value_gg = if_else(commodity == "Soy", value * 30,
    if_else(commodity == "Maize", value * 20, value * 1.9)  #this value of 1.9 comes from standard mean proportionmilk of all states then use to weight average of milk and meat intensities (2.5*0.74)+(0.275*0.26)
  )) %>%
  mutate(value_gg = round(value_gg,1)) %>%
  dplyr::select(-value) %>%
  mutate(source = "Mod") %>%
  dplyr::select(year, measure, value_gg, commodity, source)

 
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")


  if(pdfprint) {
    pdf(file = output_name)
  }

  #now plot
  c <- mod_dat %>%
    filter(measure == "Sum") %>%
    ggplot(aes(x = year, y = value, color = service)) + 
        geom_line() +
        scale_y_continuous(name = "CRAFTY units", labels = scales::comma) +
        ggtitle("Sum Service")
  print(c)


  a <- mod_serv %>%
    #filter(commodity == "Maize") %>%
    ggplot(aes(x=year, y=value_gg, color=commodity)) +
    geom_line() +
    scale_colour_manual(values=cbPalette) +
    ylab("Production (gg)") +
    xlab("Year") 
  print(a)

  if(pdfprint) {
    dev.off()
  }

}
#####

dev.off() #reset par https://stackoverflow.com/a/31909011

