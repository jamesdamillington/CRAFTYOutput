#script to analyse production data output from CRAFTY-Brazil
#reads FromMaestro####.csv files from StellaData directory

rm(list=ls())

library(tidyverse)
library(ggplot2)

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2018-08-06d"
runID <- "0-0"
sim_yrs <- seq(2000, 2004, 1)   #consolidate these years

#output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE
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

summary(mod_dat)
summary(obs_long)


all_dat <- bind_rows(mod_dat, obs_long) %>%
  mutate(source = factor(source), measure = factor(measure), commodity = factor(commodity))
  


summary(all_dat)




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

c <- crafty_dat %>% 
  ggplot(aes(x = year, y = value_cells, fill = commodity)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_y_continuous(name = "Cells", labels = scales::comma) +
  ggtitle("CRAFTY Demand")
print(c)


if(pdfprint) {
  dev.off()
}

#also need to add empirical values for comparison...





