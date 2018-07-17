# # Example preprocessing script.
source("./lib/funcs.R")
#TODO: delete old file
filename <- "./data/2017 sfr model data/Poly&ArmyTimeSeries.csv"
state.forces <- read.csv(filename)
#state.forces <- state.forces %>% dplyr::select(-iso3c) %>% dplyr::rename (iso3c = country) 
state.forces <- state.forces[, c("iso3c", "variablename", "year", "value")]
state.forces$iso3c <- country.code.name(state.forces$iso3c)
raw.data$state.forces <- state.forces
rmExcept("raw.data") 
