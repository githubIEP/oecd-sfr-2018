# Example preprocessing script 2017
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/INFORM2018_TREND_2012_2018_v033_ALL_v4.xlsx"
inform <- read_excel(filename)
#names(inform) <- inform[1, ]
#no need: inform <- inform[-c(1:2), ]
inform <- inform %>% dplyr::rename(iso3c = ISO, value = IndicatorScore, variablename = IndicatorName, year = Year) %>% dplyr::select(-IndicatorId,-IndicatorType) #%>% gather(variablename, value, -iso3c)
# INFORM indicators are generally built from 2 years before raw data 
inform <- inform %>% dplyr::mutate(year = year-1) #%>% dplyr::select(iso3c, variablename, year, value)
indicators <- raw.data$log %>% dplyr::filter(source == "INFORM")
inform$variablename <- gsub("Human Hazard", "Human",inform$variablename)
inform$variablename <- gsub("Natural Hazard", "Natural",inform$variablename)
# change names in inform
# Violent Conflict Risk = Violent Conflict Probability & Natural Disaster Risk= Natural Hazard
inform <- inform %>% dplyr::filter(variablename %in% indicators$variablename)

inform$iso3c <- country.code.name(inform$iso3c)
inform$iso3c <- gsub("Korea DPR", "North Korea", inform$iso3c)
inform <- inform[,-(4)] 
raw.data$inform <- inform
rmExcept("raw.data") 
