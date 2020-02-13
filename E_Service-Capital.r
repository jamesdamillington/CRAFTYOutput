#creates plots of service/competitiveness against capital values (cell-by-cell) distinguishing AFTs by colour (facet by service and capital)
#data from CRAFTY output csv

rm(list=ls())

#required packages
library(tidyverse)

scenario <- "testing_demand_smoother3"
runID <- "0-0"
yr <- 2015

#data_dir <- "C:/Users/k1076631/craftyworkspace/CRAFTY_TemplateCoBRA/output/Brazil/Unknown/"
data_dir <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/CRAFTY_testing/CRAFTYOutput/Data/"


output <- read.csv(paste0(data_dir,scenario,"/",runID,"/",scenario,"-",runID,"-Cell-",yr,".csv"))

dat <- output %>%
  filter(Service.Soy > 0 | Service.Maize > 0 | Service.Other.Agriculture > 0 | Service.Pasture > 0) %>%
  select(-Tick, -RegionSet, -CellRegion, -Service.Nature, -Service.Other,
    -Capital.Human, -Capital.Development, -Capital.Economic, -Capital.Other,
    -Capital.Soy.Protection, -Capital.Maize.Protection, -Capital.Pasture.Protection,
    -Capital.OAgri.Protection, -LandUse, -LandUseIndex) %>%
  gather(key = Capital, value = CapValue, -X, -Y, -Service.Soy, -Service.Maize, -Service.Pasture, -Service.Other.Agriculture, -Agent, -Competitiveness) %>%
  gather(key = Service, value = ServiceValue, -X, -Y, -Capital, -CapValue, -Agent, -Competitiveness) %>%
  mutate(Capital = factor(Capital, levels=c("Capital.Moisture",	"Capital.Nature",	"Capital.Port.Access",	"Capital.Nature.Access",	"Capital.Growing.Season",	"Capital.Other.Agriculture",	"Capital.Land.Price",	"Capital.Agri.Infrastructure",	"Capital.OAgri.Infrastructure"))) %>%
  mutate(Service = factor(Service, levels=c("Service.Soy","Service.Maize","Service.Pasture","Service.Other.Agriculture")))

summary(dat)


#plot agent only (sample_frac to reduce time to create plot!)
dat %>%
  sample_frac(0.1) %>%
  filter(Service == "Service.Soy" | Service == "Service.Maize") %>%
  filter(Capital == "Capital.Growing.Season" | Capital == "Capital.Moisture") %>%
  #filter(Capital == "Capital.Growing.Season" | Capital == "Capital.Moisture" | Capital == "Capital.Other.Agriculture" | Capital == "Capital.Port.Access") %>%
  ggplot(aes(x=CapValue, y=ServiceValue,colour = Agent)) +
  geom_point() +
  facet_grid(Service ~ Capital)
  
#plot agent and competitiveness
dat %>%
  sample_frac(0.1) %>%
  filter(Service == "Service.Soy" | Service == "Service.Maize") %>%
  filter(Capital == "Capital.Growing.Season" | Capital == "Capital.Moisture") %>%
  ggplot(aes(x=CapValue, y=ServiceValue,colour = Competitiveness, shape=Agent)) +
  geom_point() +
  facet_grid(Service ~ Capital)
