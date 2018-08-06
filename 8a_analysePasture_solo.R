#version of analyseProduction script but using service calculated from cells output file
#used with data from run that do not use STELLA (no FromMaestro files created)


rm(list=ls())

library(tidyverse)
library(ggplot2)

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2018-08-06h"
runID <- "0-0"
sim_yrs <- seq(2000, 2015, 1)   #consolidate these years

#output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE
output_name <- paste0("Data/",scenario,"/",runID,"/",scenario,"_ProductionAnalysis_NoSTELLA.pdf")



#year, service, measure, value 
mod_dat <- data.frame(
    year = integer(),
    service = character(),
    measure = integer(),
    value = numeric()
    
  )
tbl_df(mod_dat)

for(i in seq_along(sim_yrs)){
  
  #i <- 1
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


#read Demand_Empirical.csv here and join to mod_serv...

# mod_demand_dat <- read_csv(paste0("Data/",scenario,"/Demand_Empirical.csv"))
# 
# mod_demand<- mod_demand_dat %>%
#   dplyr::select(Year, Soy, Maize, Pasture) %>%
#   gather(key = commodity, value = value, -Year) %>%
#   mutate(value_gg = if_else(commodity == "Soy", value * 30,
#     if_else(commodity == "Maize", value * 20, value * 1.9)
#   )) %>%
#   mutate(source = "Mod", measure = "Demand") %>%
#   rename(year = Year) %>%
#   dplyr::select(year, measure, value_gg, commodity, source)



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

all_dat <- bind_rows(mod_serv, obs_long) %>%
  mutate(source = factor(source), measure = factor(measure), commodity = factor(commodity))
  


summary(all_dat)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#F0E442")


if(pdfprint) {
  pdf(file = output_name)
}

c <- mod_dat %>%
  filter(measure == "Sum") %>%
  ggplot(aes(x = year, y = value, color = service)) + 
      geom_line() +
      scale_y_continuous(name = "CRAFTY units", labels = scales::comma) +
      ggtitle("Sum Service")
print(c)

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
  filter(commodity == "Pasture") %>%
  ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  scale_y_continuous(limits = c(0, 50000)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Pasture") 
print(a)
  
if(pdfprint) {
  dev.off()
}

