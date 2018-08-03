#Script combined code from multiple other individual output scripts to produce:
#LCcomparisonAnalysis.pdf
#CRAFTYmunisLC.csv
#CRAFTYmunisServCap.csv
#LCcomparisonMaps.pdf
#images of LU maps and Capital maps
#videos of LU maps and Capital maps

#script assumes the entire CRAFTY scenario directory (output data) is present in Data directory (with name format scenario/runID)
#also assumes the input region.csv has been copied to the same directory as output data 


rm(list=ls())

scenario <- "Testing_2018-08-03"
runID <- "0-0"
cl <- "PastureB"  #classification for observed LU map

data_dir <- paste0("Data/",scenario,"/",runID,"/")  #where output data have been saved

yrs <- seq(2000, 2015, 1)        #all years of analysis
calib_yrs <- c(2005, 2010, 2015) #years for calibration analysis

sim_yrs <- seq(2000, 2015, 1)   #movie made for all these years (will usually be identical to yrs above)
fig_yrs <- c(2000, 2005, 2010, 2015) #figures output for only these years 

#calibration analysis output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE

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
      sumInfraC = sum(Capital.Infrastructure),
      sumEconC = sum(Capital.Economic),
      sumAcessC = sum(Capital.Acessibility),
      sumGSeasonC = sum(Capital.Growing.Season),
      sumOtherAgriC = sum(Capital.Other.Agriculture),
      sumOtherC = sum(Capital.Other),
      sumLandProteC = sum(Capital.Land.Protection),
      sumLandPriceC = sum(Capital.Land.Price),
      meanAgriC = round(mean(Capital.Agriculture),3),
      meanNatureC = round(mean(Capital.Nature),3),
      meanHumanC = round(mean(Capital.Human),3),
      meanDevC = round(mean(Capital.Development),3),
      meanInfraC = round(mean(Capital.Infrastructure),3),
      meanEconC = round(mean(Capital.Economic),3),
      meanAcessC = round(mean(Capital.Acessibility),3),
      meanGSeasonC = round(mean(Capital.Growing.Season),3),
      meanOtherAgriC = round(mean(Capital.Other.Agriculture),3),
      meanOtherC = round(mean(Capital.Other),3),
      meanLandProteC = round(mean(Capital.Land.Protection),3),
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
#####



#start of 2_CRAFTYsummary_5LCs.r
#####
#output variables
SC_name <- paste0(data_dir,scenario,"_CRAFTYmunisServCap.csv")  #output file name for services and capitals
LC_name <- paste0(data_dir,scenario,"_CRAFTYmunisLC.csv")  #output file name for land cover data 

#load the region file (used to match each cell to a municipality)
region <- read.csv(paste0(data_dir,"region.csv"))


for(i in seq_along(yrs)){
  
  #Load model output data
  output <- read.csv(paste0(data_dir,scenario,"-",runID,"-Cell-",yrs[i],".csv"))
  
  #load empirical map summary data (created using summarise_LCmaps.r)
  lc <- read.csv(paste0("Data/SummaryTables/LCs",yrs[i],"_PastureB.csv"), header = T)

  #create df containing only cell and muni data (rename columns) 
  munis<-data.frame(region$X, region$Y, region$muniID)
  munis <- rename_all(munis, .funs = funs(substring(., 8)))  #string "region." prefix using substring function to retain only chars after 8 position (1 index) 
  #munis <- rename_all(munis, .funs = funs(sub("^region.", "",.)))  #or same as previos line using sub with regex

  #join to add muniID to the CRAFTY output data
  output <- inner_join(output, munis, by = c("X", "Y"))
  
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
  selectedFRs <- c("muniID","FR123", "FR45", "FR6", "FR7", "FR8") #JM! check this works
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
    "LC1" = "Obs1",
    "LC2" = "Obs2",
    "LC3" = "Obs3",
    "LC4" = "Obs4",
    "LC5" = "Obs5",
    "FR45" = "Mod1", 
    "FR6" = "Mod2", 
    "FR123" = "Mod3", 
    "FR7" = "Mod4",
    "FR8" = "Mod5",
    "NonNAs" = "cellCount",
    "NAs" = "NAcellCount"))
  
  #code to add modal cell lc for each muncipality for modelled and predicted LC
  #mMode and oMode are characters  (the names of the column with greatest proportion)
  lcDat <- mutate(lcDat, mM = names(lcDat)[max.col(lcDat[2:6])+1L])  #from https://stackoverflow.com/a/37197584
  lcDat <- mutate(lcDat, oM = names(lcDat)[max.col(lcDat[7:11])+6L])  #edit 6L to get to right columns
  
  #remove letters from start of mM and oM (returning as integer)
  lcDat <- mutate(lcDat, ModMode = as.integer(substring(mM, 4)))
  lcDat <- mutate(lcDat, ObsMode = as.integer(substring(oM, 4)))
  
  #drop column that had letters in
  lcDat <- lcDat %>%
    dplyr::select(-c(mM,oM))
  
  #comparison of modelled mode vs observed mode (TRUE/FALSE)
  lcDat <- lcDat[!is.na(lcDat$ObsMode),]
  lcDat$diffcMode <- lcDat$ModMode != lcDat$ObsMode
  
  #calc total prop incorrectly predicted cells (in cells; need to chack ths against Pontius papers)
  #cannot use sum as that sums entire variable
  lcDat <- lcDat %>%
    mutate(cellDiffcCount = round(cellCount*(abs(Mod1-Obs1) + abs(Mod2-Obs2) + abs(Mod3-Obs3) + abs(Mod4-Obs4) + abs(Mod5-Obs5)))/2) %>%
    mutate(cellDiffcProp = round(cellDiffcCount/cellCount,3))
  
  #calc difference in proportion for each LC between Modelled and Observed
  lcDat <- lcDat %>%
    mutate(diffcProp1 = round(Mod1 - Obs1, digits = 3)) %>%
    mutate(diffcProp2 = round(Mod2 - Obs2, digits = 3)) %>%
    mutate(diffcProp3 = round(Mod3 - Obs3, digits = 3)) %>%
    mutate(diffcProp4 = round(Mod4 - Obs4, digits = 3)) %>%
    mutate(diffcProp5 = round(Mod5 - Obs5, digits = 3))

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
  pdf(file = paste0(data_dir,scenario,"_LCcomparisonAnalysis.pdf"))
}

cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), diffcProp3 = col_double())) #needed to ensure correct import (many zeros in diffcProp3 at top of file)

## Mode analysis

#1. count munis with different modes
#2. contingency table of performance
#3. maps of modes (observed, modelled, diffc) 


#total number of munis that have different modes
cDat %>%
  group_by(Year) %>%
  dplyr::summarise(count = sum(diffcMode)) -> CDiffc

#proportional difference (of all municipalities)
cDat %>%
  group_by(Year) %>%
  dplyr::summarise(prop = round(sum(diffcMode)/length(diffcMode),3)) -> PDiffc

diffc <- inner_join(CDiffc, PDiffc)

print(diffc)

if(pdfprint) {
  grid.arrange(tableGrob(as.data.frame(diffc)), top=" Municipality Error")
}


#confusionMatrix needs factors
cDat <- cDat %>%
  mutate(ModMode = factor(ModMode, c(1,2,3,4,5))) %>%
  mutate(ObsMode = factor(ObsMode, c(1,2,3,4,5)))


#could create a table subset here of munis that do not match?

summary(cDat)
LCnames <- c("Nat", "OtherAgri", "Agri", "Other", "Pasture")  #used to label error matrix in loop below

#yr <- 2005
#loop through years, priting error matrices
for(yr in calib_yrs){

  #yr<-2005  #for testing
  cfm <- confusionMatrix(filter(cDat, Year == yr)$ModMode,filter(cDat, Year == yr)$ObsMode)

  xmat <- as.matrix(cfm, what = "xtabs")
  xtab <- as.table(cfm, what = "xtabs")

  colnames(xtab) <- LCnames
  rownames(xtab) <- LCnames

  print(cfm$overall)


  #From Pontius and Santacruz 2014 http://dx.doi.org/10.1080/2150704X.2014.969814
  #"Quantity difference is the amount of difference between the two maps that 
  #derives from a less than perfect match in the amount of the categories."p.7543
  
  #"Quantity difference is zero if and only if the right column of marginal totals at time t
  #matches the bottom row of marginal totals at time t + 1." p.7544
  
  #"Exchange exists for a pair of pixels when one pixel is classified as category A in the
  #first map and as category B in the second map, while simultaneously the paired pixel is
  #classified as category B in the first map and as category A in the second map." (abstract)
  
  #"If there are more than two categories, then it is possible to have a component called shift,
  #which is allocation difference that is not exchange."(abstract)
  
  #Also see Ponitus Millones 2011 https://doi.org/10.1080/01431161.2011.552923
  #Sum of Exchange and Shift is equal to Exchange in this earlier paper 
  
  print(diffTablej(xtab))     
  print(exchangeDij(xtab))     #disaggregates Overall exchange (value from diffTablej or overallExchangeD)
  print(overallDiffCatj(xtab))  #sum of rows in diffTablej
  
  if(pdfprint) {
    g = list(tableGrob(xtab),
      tableGrob(round(as.data.frame(cfm$overall),3)) )
    grid.arrange(grobs=g, top=paste0(yr," Cross Tab and Error Metrics"))
    
    g = list( tableGrob(diffTablej(xtab)),
      tableGrob(exchangeDij(xtab)))
    grid.arrange(grobs=g, top=paste0(yr," Pontius diffTablej and its disaggregration"))
  }
  
}
  

## Proportions

#Plots are modelled proportions vs observed proportions by municiapilty. Municipality IDs of five largest outliers are shown. Red lines is a linear regression (coefficients shown in upper left). 
#linear models used in plots below

#dummy to df to expand
ddf <- data_frame(
  Year = rep(calib_yrs,length.out=5),
  LC = c(1,2,3,4,5) 
)

#get all combos of years and LCs (long, tidy format)
mods <- ddf %>% expand(Year, LC)

#add empty variables for coeffs of regression models 
mods <- mods %>% 
  mutate(a = -99) %>%
  mutate(b = -99) %>%
  mutate(r2 = -99)


#loop through years, fit models for each LC and populate mods table
for(yr in calib_yrs){

  #fit models for this year
  LC1_mod <- lm(Mod1 ~ Obs1, data = filter(cDat, Year == yr))
  LC2_mod <- lm(Mod2 ~ Obs2, data = filter(cDat, Year == yr))
  LC3_mod <- lm(Mod3 ~ Obs3, data = filter(cDat, Year == yr))
  LC4_mod <- lm(Mod4 ~ Obs4, data = filter(cDat, Year == yr))
  LC5_mod <- lm(Mod5 ~ Obs5, data = filter(cDat, Year == yr))

  #populate table with coeffs 
  mods <- 
    mods %>%
    mutate(a = ifelse(Year == yr & LC == 1, round(as.numeric(LC1_mod$coefficients[2]),3), a)) %>%
    mutate(b = ifelse(Year == yr & LC == 1, round(as.numeric(LC1_mod$coefficients[1]),3), b)) %>%
    mutate(r2 = ifelse(Year == yr & LC == 1, round(summary(LC1_mod)$r.squared,3), r2)) %>%
    mutate(a = ifelse(Year == yr & LC == 2, round(as.numeric(LC2_mod$coefficients[2]),3), a)) %>%
    mutate(b = ifelse(Year == yr & LC == 2, round(as.numeric(LC2_mod$coefficients[1]),3), b)) %>%
    mutate(r2 = ifelse(Year == yr & LC == 2, round(summary(LC2_mod)$r.squared,3), r2)) %>%
    mutate(a = ifelse(Year == yr & LC == 3, round(as.numeric(LC3_mod$coefficients[2]),3), a)) %>%
    mutate(b = ifelse(Year == yr & LC == 3, round(as.numeric(LC3_mod$coefficients[1]),3), b)) %>%
    mutate(r2 = ifelse(Year == yr & LC == 3, round(summary(LC3_mod)$r.squared,3), r2)) %>%    
    mutate(a = ifelse(Year == yr & LC == 4, round(as.numeric(LC4_mod$coefficients[2]),3), a)) %>%
    mutate(b = ifelse(Year == yr & LC == 4, round(as.numeric(LC4_mod$coefficients[1]),3), b)) %>%
    mutate(r2 = ifelse(Year == yr & LC == 4, round(summary(LC4_mod)$r.squared,3), r2)) %>%
    mutate(a = ifelse(Year == yr & LC == 5, round(as.numeric(LC5_mod$coefficients[2]),3), a)) %>%
    mutate(b = ifelse(Year == yr & LC == 5, round(as.numeric(LC5_mod$coefficients[1]),3), b)) %>%
    mutate(r2 = ifelse(Year == yr & LC == 5, round(summary(LC5_mod)$r.squared,3), r2))
}

#convert year and LCs to factors
mods <- mods %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(LC = factor(LC, labels = c("Nat", "OtherAgri", "Agri", "Other","Pasture"))) #JM! check this is right after testing

print(mods)
grid.arrange(tableGrob(mods), top="Model Parameters")

#barplot of r2 by year and LC
p <- ggplot(mods, aes(x = Year, y = r2, fill = LC)) +
  geom_bar(stat="identity", position='dodge') +
  ggtitle("Model Comparison") 
print(p)


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
      mutate(Nat.Mod = round(Mod1 * cellCount,0)) %>%
      mutate(OAgri.Mod = round(Mod2 * cellCount,0)) %>%
      mutate(Agri.Mod = round(Mod3 * cellCount,0)) %>%
      mutate(Other.Mod = round(Mod4 * cellCount,0)) %>%
      mutate(Pas.Mod = round(Mod5 * cellCount,0)) %>%
      mutate(Nat.Obs = round(Obs1 * cellCount,0)) %>%
      mutate(OAgri.Obs = round(Obs2 * cellCount,0)) %>%
      mutate(Agri.Obs = round(Obs3 * cellCount,0)) %>%
      mutate(Other.Obs = round(Obs4 * cellCount,0)) %>%
      mutate(Pas.Obs = round(Obs5 * cellCount,0))

#long version
cDat_long_mod <- cDat %>%
  dplyr::select(Year, state:Pas.Mod) %>%
  gather(key = LC, value = cells, -Year, -state) %>% 
  group_by(Year,LC) %>%
  summarise_at(vars(matches("cells")),sum) %>%
  mutate(source = "Mod")

cDat_long_obs<- cDat %>%
  dplyr::select(Year, state, Nat.Obs:Pas.Obs) %>%
  gather(key = LC, value = cells, -Year, -state) %>% 
  group_by(Year,LC) %>%
  summarise_at(vars(matches("cells")),sum) %>%
  mutate(source = "Obs")

cDat_long <- bind_rows(cDat_long_mod, cDat_long_obs)

cDat_long <- cDat_long %>%
  mutate(LC = 
    if_else(LC == "Nat.Mod" | LC == "Nat.Obs", "Nature",
    if_else(LC == "OAgri.Mod" | LC == "OAgri.Obs", "OAgri",
    if_else(LC == "Agri.Mod" | LC == "Agri.Obs", "Agri",
    if_else(LC == "Other.Mod" | LC == "Other.Obs", "Other",
      "Pasture")))))
    
    
#mutate(LC = as.factor(LC), source = as.factor(source))

c <- cDat_long %>% 
  ggplot(aes(x = Year, y = cells,color = LC, linetype = source)) + 
  geom_line(size = 1) +
  scale_y_continuous(name = "Cells", labels = scales::comma) +
  ggtitle("CRAFTY Output")
print(c)




#scatter plots of observed vs 'modelled' proportion of muni LC
theme_set(theme_gray(base_size = 18))


##need to filter cDat by calib_yrs
cDat <- filter(cDat, Year %in% calib_yrs)

#facet plot by year for LC1
p <- ggplot(cDat, aes(x=Obs1, y=Mod1)) +
  geom_point() +
  xlab("Observed") + 
  ylab("Modelled") +
  ggtitle("Nature")  +
  geom_abline(intercept = 0, slope = 1, color = "red") +   #add 45 degree line (perfect prediction)
  facet_grid(.~ Year)  
print(p)

#facet plot by year for LC2
p <- ggplot(cDat, aes(x=Obs2, y=Mod2)) +
  geom_point() +
  xlab("Observed") + 
  ylab("Modelled") +
  ggtitle("OtherAgri")  +
  geom_abline(intercept = 0, slope = 1, color = "red") +  #add 45 degree line (perfect prediction)
  facet_grid(.~ Year) 
print(p)

#facet plot by year for LC3
p <- ggplot(cDat, aes(x=Obs3, y=Mod3)) +
  geom_point() +
  xlab("Observed") + 
  ylab("Modelled") +
  ggtitle("Agriculture")  +
  geom_abline(intercept = 0, slope = 1, color = "red")+   #add 45 degree line (perfect prediction)
  facet_grid(.~ Year) 
print(p)

#facet plot by year for LC4
p <- ggplot(cDat, aes(x=Obs4, y=Mod4)) +
  geom_point() +
  xlab("Observed") + 
  ylab("Modelled") +
  ggtitle("Other")  +
  geom_abline(intercept = 0, slope = 1, color = "red") +   #add 45 degree line (perfect prediction)
  facet_grid(.~ Year) 
print(p)

#facet plot by year for LC5
p <- ggplot(cDat, aes(x=Obs5, y=Mod5)) +
  geom_point() +
  xlab("Observed") + 
  ylab("Modelled") +
  ggtitle("Pasture")  +
  geom_abline(intercept = 0, slope = 1, color = "red") +   #add 45 degree line (perfect prediction)
  facet_grid(.~ Year) 
print(p)


if(pdfprint) {
  dev.off()
}
#####

#start of 4_LCcalibrationMaps_5LCs.r
#####
cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), diffcProp3 = col_double())) #needed to ensure correct import (many zeros in diffcProp3 at top of file)

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read("Data/Vector/BRmunis_sim10_simple2.shp")


if(pdfprint) {
  pdf(file = paste0(data_dir,scenario,"_LCcomparisonMaps.pdf"))
}

## Maps
#loop through years, printing maps
for(yr in calib_yrs){

  cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 

  #create land cover palette
  map_pal <- c("darkgreen", "darkcyan", "green", "grey", "khaki")

  #plot observed vs modelled modal muni land cover
  plot(cDat_map["ObsMode"], pal = map_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Observed Mode LC"), key.pos = NULL)
  legend("bottomright", cex = 1.3, c("Nature", "Other Agri", "Agriculture", "Other", "Pasture"), fill = map_pal)
  
  plot(cDat_map["ModMode"], pal = map_pal, graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Modelled Mode LC"), key.pos = NULL)
  legend("bottomright", cex = 1.3, c("Nature", "Other Agri", "Agriculture", "Other", "Pasture"), fill = map_pal)

  
  #map of muni mode correct/incorrect   
  plot(cDat_map["diffcMode"], pal = c("darkgreen","red"), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Model vs Obs Mode Comparison"), key.pos = NULL)  
  legend("bottomright", cex = 1.3, c("Correct", "Incorrect"), fill = c("darkgreen","red"))
  
  
  #get max value for colour breaks below
  errorMax <- max(filter(cDat, Year == yr)$cellDiffcCount)

  #for cell accuracy maps
  cell_pal <- brewer.pal(8, "Reds")

  #total count of cells incorrect
  plot(cDat_map["cellDiffcCount"], pal = cell_pal, breaks = seq(0,errorMax, length.out = length(cell_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Count Incorrect Cells"))

  #proportion of cells incorrect 
  plot(cDat_map["cellDiffcProp"], pal = cell_pal, breaks = seq(0,1, length.out = length(cell_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Prop Incorrect Cells"))

  
  #for LC proportion accuracy maps
  prop_pal <- brewer.pal(11, "RdYlGn")
  
  #difference in proportion predictions (negative is under-prediction by model, positive over-prediction)
  plot(cDat_map["diffcProp1"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Nature Prop Diffc"))
  plot(cDat_map["diffcProp2"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Other Agri Prop Diffc"))
  plot(cDat_map["diffcProp3"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Agriculture Prop Diffc"))
  plot(cDat_map["diffcProp5"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Pasture Prop Diffc"))
  plot(cDat_map["diffcProp4"], pal = prop_pal, breaks = seq(-1,1, length.out = length(prop_pal)+1), graticule = st_crs(cDat_map), axes = TRUE, lty = 0, main = paste(yr,"Other Prop Diffc"))
  par(mfrow=c(1,1))  #needed to ensure plotting plays nicely with key.pos = NULL above
  
}

if(pdfprint) {
  dev.off()
}
#####

#start of 7_outputMovies.r
#####
#First, Raster maps
mps <- list()
lus <- list()

for(i in seq_along(sim_yrs)){
  
  print(paste0("raster maps: ", sim_yrs[i]))

  #Load model output data
  output <- read_csv(paste0(data_dir,scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
  
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
  ggsave(paste0(data_dir,scenario,"_RasterOutput_AllMaps",sim_yrs[i],".png"), plot = mps[[i]], width=25, height=25, units="cm", dpi = 200)

  ggsave(paste0(data_dir,scenario,"_RasterOutput_LUMap",sim_yrs[i],".png"), plot = lus[[i]], width=20, height=12.5, units="cm", dpi = 300)
  }
    
}

#make videos here by looping through list
saveVideo(
  for(i in seq_along(lus)){
    print(lus[[i]])
  },
  video.name = paste0(data_dir,scenario,"_RasterOutput_LandUse_",scenario,".mp4"))
  
saveVideo(
  for(i in seq_along(mps)){
    print(mps[[i]])
  },
  video.name = paste0(data_dir,scenario,"_RasterOutput_Capitals_",scenario,".mp4"))



#Next, vector maps
#different approach - create figures first, then videos (as cannot save sf plot objects to a list)
cDat <- readr::read_csv(LC_name,
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)
scDat <- readr::read_csv(SC_name)

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

  print(paste0("vector maps: ", yr))
  
  #if we want this year saved as an image 
  if(yr %in% fig_yrs) {
    
    #land cover map first
    cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
  
    png(paste0(data_dir,scenario,"_MuniOutput_LandUse_",yr,".png"), width=1000, height=1000, res=100)
   
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

    png(paste0(data_dir,scenario,"_MuniOutput_Capitals_",yr,".png"), width=1200, height=1200, res=100)
    
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
  video.name = paste0(data_dir,scenario,"_MuniOutput_LandUse_",scenario,".mp4"))
 
pr <- par()

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
  video.name = paste0(data_dir,scenario,"_MuniOutput_Capitals_",scenario,".mp4"))



#start of 8_analyseProduction.r
#####
output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_ProductionAnalysis.pdf")

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

odata <- read_csv("Data/Production_Export_Internal.csv")

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
  mutate(source = "Obs")
  


#empty table to populate from files below
mod_dat <- data.frame(
    commodity = character(),
    measure = character(),
    year = integer(),
    value_gg = numeric()
    
  )
tbl_df(mod_dat)

#loop through all files 
for(i in seq_along(sim_yrs)){
 
  filen <- paste0("FromMaestro",sim_yrs[i],".csv")
  
  dat <- read_csv(paste0("Data/",scenario,"/StellaData/",filen),col_names=F)

  mod_dat <- mod_dat %>% 
    add_row(commodity = "Soy", measure = "Production", year = sim_yrs[i], value_gg = as.numeric(dat[1,2])) %>%
    add_row(commodity = "Soy", measure = "Storage", year = sim_yrs[i], value_gg = as.numeric(dat[4,2])) %>%
    add_row(commodity = "Soy", measure = "Export", year = sim_yrs[i], value_gg = as.numeric(dat[3,2])) %>%
    add_row(commodity = "Maize", measure = "Production", year = sim_yrs[i], value_gg = as.numeric(dat[5,2])) %>%
    add_row(commodity = "Maize", measure = "Storage", year = sim_yrs[i], value_gg = as.numeric(dat[8,2])) %>%
    add_row(commodity = "Maize", measure = "Export", year = sim_yrs[i], value_gg = as.numeric(dat[7,2])) %>%
    add_row(commodity = "Meat", measure = "Production", year = sim_yrs[i], value_gg = as.numeric(dat[29,2])) %>%
    add_row(commodity = "Meat", measure = "Export", year = sim_yrs[i], value_gg = as.numeric(dat[33,2])) %>%
    add_row(commodity = "Dairy", measure = "Production", year = sim_yrs[i], value_gg = as.numeric(dat[30,2])) %>%
    add_row(commodity = "Dairy", measure = "Export", year = sim_yrs[i], value_gg = as.numeric(dat[34,2]))

}

#needed to prevent bind_rows error below
mod_dat <- mod_dat %>%
  mutate(measure = as.character(measure), commodity = as.character(commodity))


#get internal demand data
internal <- read.tcsv(paste0("Data/",scenario,"/StellaData/InternalCRAFTY.csv"))
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
  mutate(source = "Modelled")

mod_dat <- mod_dat %>%
  dplyr::select(year, commodity, measure, source, value_gg) 

obs_long <- obs_long %>%
  dplyr::select(year, commodity, measure, source, value_gg)


all_dat <- bind_rows(mod_dat, obs_long) %>%
  mutate(source = factor(source), measure = factor(measure), commodity = factor(commodity))
  


#CRAFTY demand - add code here
#empty table to populate from files below
crafty_dat <- data.frame(
    commodity = character(),
    measure = character(),
    year = integer(),
    value_cells = numeric()
    
  )
tbl_df(crafty_dat)

for(i in seq_along(sim_yrs)){
 
  filen <- paste0("FromMaestro",sim_yrs[i],".csv")
  
  dat <- read_csv(paste0("Data/",scenario,"/StellaData/",filen),col_names=F)

  crafty_dat <- crafty_dat %>% 
    add_row(commodity = "Soy", measure = "Demand", year = sim_yrs[i], value_cells = as.numeric(dat[35,2])) %>%
    add_row(commodity = "Maize", measure = "Demand", year = sim_yrs[i], value_cells = as.numeric(dat[36,2])) %>%
    add_row(commodity = "Meat", measure = "Demand", year = sim_yrs[i], value_cells = as.numeric(dat[37,2])) %>%
    add_row(commodity = "Dairy", measure = "Demand", year = sim_yrs[i], value_cells = as.numeric(dat[38,2])) 

}



cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")


if(pdfprint) {
  pdf(file = output_name)
}

#now plot
#timelines of production, storage, export by commodity
a <- all_dat %>% 
  filter(commodity == "Soy") %>%
  ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Soy") 
print(a)


a <- all_dat %>% 
  filter(commodity == "Maize") %>%
  ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Maize") 
print(a)

a <- all_dat %>% 
  filter(commodity == "Meat") %>%
  ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Meat") 
print(a)

a <- all_dat %>% 
  filter(commodity == "Dairy") %>%
  ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Dairy") 
print(a)

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
#####