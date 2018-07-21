#analysis as for LCcalibrationAnalysis but for a single municipality

#timeseries of land cover and captitals 
rm(list=ls())

#this directory should exist and contain the CRAFTYmunisServCap.csv
run_name <- "test"

target_muni <- 1702406

#output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE

library(tidyverse)
library(ggrepel)

#oputput variables
output_name <- paste0("Data/",run_name,"/muni",target_muni,"_calibrationPlots.pdf")


if(pdfprint) {
  pdf(file = output_name)
}

lcDat <- readr::read_csv(paste0("Data/",run_name,"/CRAFTYmunisLC.csv"))
scDat <- readr::read_csv(paste0("Data/",run_name,"/CRAFTYmunisServCap.csv"))

df_all <- left_join(lcDat, scDat, by = c("muniID", "Year"))


#capitals
muniCaps <- df_all %>% 
  filter(., muniID == target_muni) %>%
  dplyr::select(Year, meanNatureC:meanLandProteC) %>% 
  gather(Capital, meanCapital, meanNatureC:meanLandProteC) 

#rename to look better in plot
muniCaps$Capital <- recode(muniCaps$Capital, 
  meanNatureC = "Nature",
  meanHumanC = "Human",
  meanDevC = "Development",
  meanInfraC = "Infrastructure",
  meanEconC = "Economic",
  meanGSeasonC = "Growing Season",
  meanPastureC = "Pasture",
  meanOtherC = "Other",
  meanLandProteC = "Land Protection"
  )
  

p.cap <- muniCaps %>% 
  mutate(label = if_else(Year == max(Year), as.character(Capital), NA_character_)) %>%
  ggplot(aes(x = as.factor(Year), y = meanCapital, group = Capital, colour=Capital)) +
  geom_line() +
  scale_color_brewer(palette="Paired") + 
  geom_label_repel(aes(label = label), na.rm = TRUE) +
  theme(legend.position="none") + 
  xlab("Year") +
  ggtitle(paste0("Muni ",target_muni))
print(p.cap)




#land covers
muniLCs <- df_all %>% 
  filter(., muniID == target_muni) %>%
  dplyr::select(Year, Mod1:Obs5) %>% 
  gather(LC, Proportion, Mod1:Obs5) %>%
  mutate(Data = as.factor(substring(LC, 1, 3))) %>%          #get first three characters (used in plot for linetype)
  mutate(LandCover = as.integer(substring(LC, 4))) %>%       #get the integer from e.g. Mod1
  mutate(LandCover = case_when(
     LandCover == 1 ~ "Nature",
     LandCover == 2 ~ "Other Agri",
     LandCover == 3 ~ "Agriculture",
     LandCover == 4 ~ "Other",
     LandCover == 5 ~ "Pasture",
     TRUE ~ "NA"))                                           #rename to look better in plot

p.lc <- muniLCs %>%
  mutate(label = if_else(Year == max(Year), as.character(LandCover), NA_character_)) %>%
  ggplot(aes(x = as.factor(Year), y = Proportion, group = LC, colour=LandCover)) +
  geom_line(aes(linetype = Data)) +
  scale_color_brewer(palette = "Dark2", guide = FALSE) + 
  geom_label_repel(aes(label = label, fill = Data), na.rm = TRUE) +
  scale_fill_brewer(palette = "Greys") + 
  xlab("Year") +
  ggtitle(paste0("Muni ",target_muni))
print(p.lc)


if(pdfprint) {
  dev.off()
}
