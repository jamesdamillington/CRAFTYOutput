#Script takes CRAFTY output files and combines with regionfile to summarise data to municipality level 
#Two summary files are produced: 1) Services and Capitals, 2) Land Cover 
#The script reads empirical data for corresponding CRAFTY output years and calculates various metrics of land cover comparison 
#Initial code assumes years are 2005, 2010, 2015 but this could be edited 

#updated by JM 2018-05-12 to include Pature (5LCs)

#LC1 = Nature
#LC2 = Other Agri
#LC3 = Agri
#LC4 = Other
#LC5 = Pasture

rm(list=ls())

#required packages
library(tidyverse)

#required functions  
getServices <- function(data)
{
  #calculates services in each muni
  data %>%
    group_by(muniID) %>%
    dplyr::summarise(
      sumNatureS = sum(Service.Nature),
      sumSoyS = sum(Service.Soy),
      sumMaizeS = sum(Service.Maize),
      sumPastureS = sum(Service.Pasture),
      sumOtherS = sum(Service.Other),
      sumOtherAgriS = sum(Service.Other.Agriculture),
      meanNatureS = round(mean(Service.Nature),3),
      meanSoyS = round(mean(Service.Soy),3),
      meanMaizeS = round(mean(Service.Maize),3),
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
      sumGSeasonC = sum(Capital.Growing.Season),
      sumOtherAgriC = sum(Capital.Other.Agriculture),
      sumOtherC = sum(Capital.Other),
      sumLandProteC = sum(Capital.Land.Protection),
      meanAgriC = round(mean(Capital.Agriculture),3),
      meanNatureC = round(mean(Capital.Nature),3),
      meanHumanC = round(mean(Capital.Human),3),
      meanDevC = round(mean(Capital.Development),3),
      meanInfraC = round(mean(Capital.Infrastructure),3),
      meanEconC = round(mean(Capital.Economic),3),
      meanGSeasonC = round(mean(Capital.Growing.Season),3),
      meanOtherAgriC = round(mean(Capital.Other.Agriculture),3),
      meanOtherC = round(mean(Capital.Other),3),
      meanLandProteC = round(mean(Capital.Land.Protection),3)
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

#output variables
SC_name <- "Data/CRAFTYmunisServCap.csv"  #output file name for services and capitals
LC_name <- "Data/CRAFTYmunisLC.csv"  #output file name for land cover data 

#load the region file (used to match each cell to a municipality)
region <- read.csv("Data/region.csv")

#Load model output data
output2005 <- read.csv("Data/Brazil-0-0-Unknown-Cell-2005.csv")
output2010 <- read.csv("Data/Brazil-0-0-Unknown-Cell-2010.csv")
output2015 <- read.csv("Data/Brazil-0-0-Unknown-Cell-2015.csv")

#load empirical map summary data (created using summarise_LCmaps.r)
lc2005 <- read.csv("Data/SummaryTables/LCs2005_PastureB.csv", header = T)
lc2010 <- read.csv("Data/SummaryTables/LCs2010_PastureB.csv", header = T)
lc2015 <- read.csv("Data/SummaryTables/LCs2015_PastureB.csv", header = T)

#create df containing only cell and muni data (rename columns) 
munis<-data.frame(region$X, region$Y, region$muniID)
munis <- rename_all(munis, .funs = funs(substring(., 8)))  #string "region." prefix using substring function to retain only chars after 8 position (1 index) 
#munis <- rename_all(munis, .funs = funs(sub("^region.", "",.)))  #or same as previos line using sub with regex

#join to add muniID to the CRAFTY output data
output2005 <- inner_join(output2005, munis, by = c("X", "Y"))
output2010 <- inner_join(output2010, munis, by = c("X", "Y"))
output2015 <- inner_join(output2015, munis, by = c("X", "Y"))


#***Services and Capitals
#aggregate services and capitals to municipality 
#(from each row is cell to each row is municipality)
services2005 <- getServices(output2005)
services2010 <- getServices(output2010)
services2015 <- getServices(output2015)
capitals2005 <- getCapitals(output2005)
capitals2010 <- getCapitals(output2010)
capitals2015 <- getCapitals(output2015)

#join mapbiomas (lc2000) and CRAFTY output (mapFRs) summary tables together
sc2005 <- left_join(lc2005, services2005, by = "muniID") %>%
  left_join(., capitals2005, by = "muniID")
sc2010 <- left_join(lc2010, services2010, by = "muniID") %>%
  left_join(., capitals2010, by = "muniID")
sc2015 <- left_join(lc2015, services2015, by = "muniID") %>%
  left_join(., capitals2015, by = "muniID")

#add year column
sc2005 <- mutate(sc2005, Year = 2005)
sc2010 <- mutate(sc2010, Year = 2010)
sc2015 <- mutate(sc2015, Year = 2015)

scDat <- bind_rows(sc2005, sc2010, sc2015)

#write data to file
readr::write_csv(scDat, path = SC_name)



#***Land Cover
#aggregate functional roles (indicative of lcs) to municipality 
#(from each row is cell to each row is municipality)
FRs2005 <- getFRs(output2005)
FRs2010 <- getFRs(output2010)
FRs2015 <- getFRs(output2015)


#legend for mapbiomas 
#1. Nature (FRs 4 and 5, so use FR45)
#2. Other Agri (FR6)
#3. Arable FRs 1, 2 and 3, so use FR123)
#4. Other (FR7)
#5. PAsture (FR8)

#subset to get only the FR combos that indicate a specific land cover for municipalities
selectedFRs <- c("muniID","FR123", "FR45", "FR6", "FR7", "FR8") #JM! check this works
mapFRs2005 <- select(FRs2005, selectedFRs)
mapFRs2010 <- select(FRs2010, selectedFRs)
mapFRs2015 <- select(FRs2015, selectedFRs)

#join mapbiomas (lc2000) and CRAFTY output (mapFRs) summary tables together
lcDat2005 <- left_join(mapFRs2005, lc2005, by = "muniID")
lcDat2010 <- left_join(mapFRs2010, lc2010, by = "muniID")
lcDat2015 <- left_join(mapFRs2015, lc2015, by = "muniID")

#add year column
lcDat2005 <- mutate(lcDat2005, Year = 2005)
lcDat2010 <- mutate(lcDat2010, Year = 2010)
lcDat2015 <- mutate(lcDat2015, Year = 2015)

lcDat <- bind_rows(lcDat2005, lcDat2010, lcDat2015)

#legend for mapbiomas from Paul
#1. Nature (FRs 4 and 5, so use FR45)
#2. Other Agri (FR6)
#3. Arable FRs 1, 2 and 3, so use FR123)
#4. Other (FR7)
#5. PAsture (FR8)

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
  select(-c(mM,oM))

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

#write data to file
readr::write_csv(lcDat, path = LC_name)
