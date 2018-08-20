
#make a csv to identify cells by municipality

rm(list=ls())

#required packages
library(tidyverse)


scenario <- "Testing_2018-08-16"
runID <- "0-0"
yrs <- seq(2000, 2015, 1)


#output variables
SC_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisServCap.csv")  #output file name for services and capitals
LC_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv")  #output file name for land cover data 

#load the region file (used to match each cell to a municipality)
region <- read.csv(paste0("Data/",scenario,"/",runID,"/region.csv"))

i <- 1

#Load model output data
output <- read.csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",yrs[i],".csv"))

#load empirical map summary data (created using summarise_LCmaps.r)
lc <- read.csv(paste0("Data/SummaryTables/LCs",yrs[i],"_PastureB.csv"), header = T)

#create df containing only cell and muni data (rename columns) 
munis<-data.frame(region$X, region$Y, region$muniID)
munis <- rename_all(munis, .funs = funs(substring(., 8)))  #string "region." prefix using substring function to retain only chars after 8 position (1 index) 
#munis <- rename_all(munis, .funs = funs(sub("^region.", "",.)))  #or same as previos line using sub with regex

#join to add muniID to the CRAFTY output data
output <- inner_join(output, munis, by = c("X", "Y"))

write_csv(output, "Data/muniCells.csv")
  
  