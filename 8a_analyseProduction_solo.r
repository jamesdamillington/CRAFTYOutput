#version of analyseProduction script but using service calculated from cells output file
#used with data from run that do not use STELLA (no FromMaestro files created)


rm(list=ls())

library(tidyverse)
library(ggplot2)
library(readxl)

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2019-11-07j"
demand <- "Demand_2019-11-05c"
runID <- "0-0"
sim_yrs <- seq(2001, 2018, 1)   #consolidate these years

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
  
  #i <- 11
  #Load model output data
  output <- read.csv(paste0("Data/",scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",sim_yrs[i],".csv"))
  
  Soy <- output %>%
    filter(`Service.Soy` > 0) %>%
    summarise(mn = mean(`Service.Soy`), sm = sum(`Service.Soy`), mm = min(`Service.Soy`), mx = max(`Service.Soy`), sd = sd(`Service.Soy`))

  Maize <- output %>%
    filter(`Service.Maize` > 0) %>%
    summarise(mn = mean(`Service.Maize`), sm = sum(`Service.Maize`), mm = min(`Service.Maize`), mx = max(`Service.Maize`), sd = sd(`Service.Maize`))

  Agri <- output %>%
    mutate(`Service.Agri` = `Service.Soy` + `Service.Maize`) %>%
    filter(`Service.Agri` > 0) %>%
    summarise(mn = mean(`Service.Agri`), sm = sum(`Service.Agri`), mm = min(`Service.Agri`), mx = max(`Service.Agri`), sd = sd(`Service.Agri`))
    
  Nature <- output %>%
    filter(`Service.Nature` > 0) %>%
    summarise(mn = mean(`Service.Nature`), sm = sum(`Service.Nature`), mm = min(`Service.Nature`), mx = max(`Service.Nature`), sd = sd(`Service.Nature`))

  OAgri <- output %>%
    filter(`Service.Other.Agriculture` > 0) %>%
    summarise(mn = mean(`Service.Other.Agriculture`), sm = sum(`Service.Other.Agriculture`), mm = min(`Service.Other.Agriculture`), mx = max(`Service.Other.Agriculture`), sd = sd(`Service.Other.Agriculture`))

  Other <- output %>%
    filter(`Service.Other` > 0) %>%
    summarise(mn = mean(`Service.Other`), sm = sum(`Service.Other`), mm = min(`Service.Other`), mx = max(`Service.Other`), sd = sd(`Service.Other`))

  Meat <- output %>%
    filter(`Service.Pasture` > 0) %>%
    summarise(mn = mean(`Service.Pasture`), sm = sum(`Service.Pasture`), mm = min(`Service.Pasture`), mx = max(`Service.Pasture`), sd = sd(`Service.Pasture`))

  
  #lserv <- c("Soy", "Maize", "Pasture", "Agri")
  lserv <- c("Soy", "Maize", "Meat")
  
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
  #mutate(measure = "Production") %>%
  mutate(value_gg = if_else(commodity == "Soy", value * 20,
    if_else(commodity == "Maize", value * 30, value * 0.275)  
  )) %>%
  mutate(value_gg = round(value_gg,1)) %>%
  dplyr::select(-value) %>%
  mutate(source = "Mod") %>%
  dplyr::select(year, value_gg, commodity, source)





meat_prod_Astates_Data <- read_excel("Data/Cattle_meat_production_Kg_2000_2018_all_states.xlsx", sheet = "Plan1", skip = 1)  #data for all states Astates
maize_prod_Amunis_Data <- read_excel("Data/maize_brazil.xlsx", sheet = "Production (tons)", skip = 1, na = c("", "-", "..."))
soy_prod_Amunis_Data <- read_excel("Data/soybean_brazil.xlsx", sheet = "Production (Tons)", skip = 1, na = c("", "-", "..."))


Fstate_vals <- c(17,    29, 31, 35, 41, 42, 43, 50, 51, 52)
Fstate_abbrev <- c("TO", "BA", "MG", "SP", "PR",  "SC", "RS", "MS", "MT", "GO")

Astate_codes <- meat_prod_Astates_Data %>%
  dplyr::select(NM_UF_SIGLA, CD_GCUF) %>%
  rename(state = NM_UF_SIGLA, stateid = CD_GCUF) %>%
  filter(!is.na(state))    #safer way to remove text line at bottom of state column
  

##MEAT
meat_prod_Astates <- meat_prod_Astates_Data %>%
  rename(state = NM_UF_SIGLA) %>%
  dplyr::select(-NM_UF, -CD_GCUF) %>%      #drop columns
  filter(!is.na(state)) %>%   #safer way to remove text line at bottom of state column
  mutate_at(vars("2001":"2018"), as.numeric) 

meat_prod_Astates_long <- meat_prod_Astates %>%
   gather(key = year, value = meat_kg, -state) %>%
   mutate_at(vars(year), as.integer) %>%
   mutate(Meat = meat_kg * 0.000001) %>%  #convert from kg to gg
   dplyr::select(-meat_kg)


#MAIZE
#has the same data strucutre (with some differences in unit conversions - could write function to cover both?) 
maize_prod_Amunis <- maize_prod_Amunis_Data %>%
  rename(muniID = `IBGE CODE`) %>%
  filter(!is.na(muniID)) %>%   #safer way to remove text line in muniID
  mutate(state = substr(muniID, 1, 2)) %>%     #extract the muniID
  mutate_at(vars("2001":"2018"), as.numeric) %>%  #convert values to numeric
  dplyr::select(-Municipality)   #drop unwanted columns

maize_prod_Astates <- maize_prod_Amunis %>%
  group_by(state) %>%
  summarise_all(sum, na.rm=T) %>%    #summarise munis to states
  mutate(state=replace(state, 1:length(Astate_codes$stateid), Astate_codes$state)) #re-label stated ids with state abbrevs

maize_prod_Astates_long <- maize_prod_Astates %>%
  gather(key = year, value = maize_kg, -state, -muniID) %>%
  mutate_at(vars(year), as.integer) %>%
  mutate(Maize = maize_kg * 0.001) %>%  #convert from tons to gg
  dplyr::select(-maize_kg, -muniID)

##SOY
soy_prod_Amunis <- soy_prod_Amunis_Data %>%
  rename(muniID = `IBGE CODE`) %>%
  filter(!is.na(muniID)) %>%   #safer way to remove text line in muniID
  mutate(state = substr(muniID, 1, 2)) %>%     #extract the muniID
  mutate_at(vars("2001":"2018"), as.numeric) %>%  #convert values to numeric
  dplyr::select(-Municipality)   #drop unwanted columns

soy_prod_Astates <- soy_prod_Amunis %>%
  group_by(state) %>%
  summarise_all(sum, na.rm=T) %>%    #summarise munis to states
  mutate(state=replace(state, 1:length(Astate_codes$stateid), Astate_codes$state)) #re-label stated ids with state abbrevs

soy_prod_Astates_long <- soy_prod_Astates %>%
  gather(key = year, value = soy_kg, -state, -muniID) %>%
  mutate_at(vars(year), as.integer) %>%
  mutate(Soy = soy_kg * 0.001) %>%  #convert from tons to gg
  dplyr::select(-soy_kg, -muniID)

prod_state_year <- left_join(meat_prod_Astates_long, maize_prod_Astates_long, by = c("year", "state"))

prod_state_year <- left_join(prod_state_year, soy_prod_Astates_long, by = c("year", "state"))

#add focal states indicator
prod_state_year <- prod_state_year %>%
  mutate(simulated = state %in% Fstate_abbrev) 

psy_long <- prod_state_year %>%
  gather(key = commodity, value = gg, -state, -year, -simulated)

psimy_long <- psy_long %>%
  group_by(simulated, year, commodity) %>%
  summarise(value_gg = sum(gg, na.rm=T)) %>%
  filter(simulated == TRUE) %>%
  mutate(source = "Obs") %>%
  ungroup() %>%
  select(-simulated)




######
#old observed
####### 


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
   filter(Year > 2000) %>%
   dplyr::select(-ends_with("export_China_gg")) %>%
   rename(year = Year)
# 
# Dairy_long <- obs_data %>%
#   dplyr::select(year, starts_with("Dairy")) %>%
#   gather(key = measure, value = value_gg, -year) %>%
#   mutate(commodity = "Dairy") %>%
#   mutate(measure = 
#       if_else(grepl("Production", measure), "Production", 
#         if_else(grepl("export", measure), "Export", "Internal")
#         ) 
#     )
# 
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
 
obs_long <- bind_rows(Maize_long, Soy_long, Meat_long) %>%
   mutate(source = "Obs")
 
all_dat <- bind_rows(mod_serv, psimy_long) %>%
   mutate(source = factor(source), measure = factor(measure), commodity = factor(commodity))
   

 
summary(all_dat)

######

all_dat <- bind_rows(mod_serv, psimy_long) %>%
   mutate(source = factor(source), commodity = factor(commodity))
   

 
summary(all_dat)



######
#output
######


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

# a <- all_dat %>%
#   filter(commodity == "Soy") %>%
#   ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
#   geom_line() +
#   scale_colour_manual(values=cbPalette) +
#   ylab("Value (gg)") +
#   xlab("Year") +
#   ggtitle("Soy")
# print(a)


agriplot <- all_dat %>%
  filter(commodity != "Meat") %>%
  ggplot(aes(x=year, y=value_gg, color=commodity, linetype=source)) +
  geom_line() +
  #scale_colour_manual(values=cbPalette) +
  ylab("Value (gg)") +
  xlab("Year") 
  #ggtitle("Soy")
print(agriplot)
 
 
# a <- all_dat %>%
#   filter(commodity == "Maize") %>%
#   ggplot(aes(x=year, y=value_gg, color=measure, linetype=source)) +
#   geom_line() +
#   scale_colour_manual(values=cbPalette) +
#   ylab("Value (gg)") +
#   xlab("Year") +
#   ggtitle("Maize")
# print(a)
   
meatplot <- all_dat %>% 
  filter(commodity == "Meat") %>%
  ggplot(aes(x=year, y=value_gg, linetype=source)) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  scale_y_continuous(limits = c(0, 10000)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Meat")
print(meatplot)
  
if(pdfprint) {
  dev.off()
}

