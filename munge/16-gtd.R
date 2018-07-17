# 2017 Example preprocessing script.
source("./lib/funcs.R")
#filename <- "./data/2017 sfr model data/GTI score and rank.csv"
filename <- "./data/2017 sfr model data/gti-banded-scores.csv"
gti <- read.csv(filename) #%>% dplyr::filter(year <= 2015) #to keep consistent with Roberto's results 
#                                                                                   -> I used the 2016 value
gti <- gti %>% dplyr::select(iso3c, variablename, year, value)
indicators <- raw.data$log %>% dplyr::filter(source == "IEP/START")
gti$variablename <- indicators$variablename
gti <- add.zeros.for.missing.countries(gti, raw.data)
gti$iso3c <- as.character(gti$iso3c)
gti$iso3c <- country.code.name(gti$iso3c)
#gti <- extend.time.series(gti, replace.with = 0)

raw.data$gti <- gti
rmExcept("raw.data")
