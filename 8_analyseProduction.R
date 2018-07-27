#script to analyse production data output from CRAFTY-Brazil
#reads FromMaestro####.csv files from StellaData directory

rm(list=ls())

library(tidyverse)
library(ggplot2)

#set for the run in CRAFTY (althrough runID difficult to control)
scenario <- "Testing_2018-07-24"
runID <- "0-0"
sim_yrs <- seq(2000, 2015, 1)   #consolidate these years

#output can be printed to pdf by setting following variable appropriately (TRUE/FALSE)
pdfprint <- TRUE
output_name <- paste0("Data/",scenario,"/",runID,"/ProductionAnalysis.pdf")


#empty table to populate from files below
all_dat <- data.frame(
    commodity = character(),
    measure = character(),
    year = integer(),
    value_gg = numeric()
    
  )
tbl_df(all_dat)

#loop through all files 
for(i in seq_along(sim_yrs)){
 
  filen <- paste0("FromMaestro",sim_yrs[i],".csv")
  
  dat <- read_csv(paste0("Data/",scenario,"/StellaData/",filen),col_names=F)

  all_dat <- all_dat %>% 
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

if(pdfprint) {
  pdf(file = output_name)
}

#now plot
#timelines of production, storage, export by commodity
all_dat %>% 
  filter(commodity == "Soy") %>%
  ggplot(aes(x=year, y=value_gg, group=measure)) +
  geom_line(aes(color=measure)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Soy") 

all_dat %>% 
  filter(commodity == "Maize") %>%
  ggplot(aes(x=year, y=value_gg, group=measure)) +
  geom_line(aes(color=measure)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Maize")

all_dat %>% 
  filter(commodity == "Meat") %>%
  ggplot(aes(x=year, y=value_gg, group=measure)) +
  geom_line(aes(color=measure)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Meat")

all_dat %>% 
  filter(commodity == "Dairy") %>%
  ggplot(aes(x=year, y=value_gg, group=measure)) +
  geom_line(aes(color=measure)) +
  ylab("Value (gg)") +
  xlab("Year") +
  ggtitle("Dairy")


if(pdfprint) {
  dev.off()
}

#how to get and plot demand from stella?
