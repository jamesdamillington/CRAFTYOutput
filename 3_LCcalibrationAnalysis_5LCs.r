#script to further analyse comparison of modelled vs observed land cover 
#reads a file created by CRAFTYsummary_5LCs.r

rm(list=ls())

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2019-02-21"
runID <- "0-0"

#script assumes there are three calibration years (2005, 2010, 2015); edit next line if that is not correct
calib_yrs <- c(2005, 2010, 2015)

#output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- T

library(tidyverse)
library(caret)   #for confusionMatrix
library(diffeR)  #for map comparison
library(gridExtra)  #for printing tables to pdf
library(grid)

#output variables)
output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_LCcomparisonAnalysis.pdf")


if(pdfprint) {
  pdf(file = output_name)
}


cDat <- readr::read_csv(paste0("Data/",scenario,"/",runID,"/",scenario,"_CRAFTYmunisLC.csv"),
  col_types = cols(Year = col_integer(), diffcProp3 = col_double()))  #needed to ensure correct import (many zeros in diffcProp3 at top of file)


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
  mutate(LC = factor(LC, labels = c("Nature", "OtherAgri", "Agri", "Other","Pasture"))) 

print(mods)
grid.arrange(tableGrob(mods), top="Model Parameters")


#create colours for plot
myCols <- c("forestgreen","darkcyan","wheat3","black","orange2")
names(myCols) <- c('Nature',"OtherAgri","Agri","Other","Pasture")


#barplot of r2 by year and LC
p <- ggplot(mods, aes(x = Year, y = r2, fill = LC)) +
  scale_fill_manual(name = "LC",values = myCols) +
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
    if_else(LC == "OAgri.Mod" | LC == "OAgri.Obs", "OtherAgri",
    if_else(LC == "Agri.Mod" | LC == "Agri.Obs", "Agri",
    if_else(LC == "Other.Mod" | LC == "Other.Obs", "Other",
      "Pasture")))))
    
    
#mutate(LC = as.factor(LC), source = as.factor(source))



c <- cDat_long %>% 
  ggplot(aes(x = Year, y = cells, color = LC, linetype = source)) + 
  geom_line(size = 1) +
  scale_colour_manual(name = "LC",values = myCols) +
  scale_y_continuous(name = "Cells", labels = scales::comma) +
  ggtitle("CRAFTY Output")
print(c)



#scatter plots of observed vs 'modelled' proportion of muni LC
theme_set(theme_gray(base_size = 18))

##filter cDat by calib_yrs
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


