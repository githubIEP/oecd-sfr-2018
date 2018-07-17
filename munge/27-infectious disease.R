# Example preprocessing script.
source("./lib/funcs.R")

# infdis <- infdis %>% dplyr::filter(measure_name == "Deaths")
# infdis <- infdis %>% dplyr::filter(metric_name == "Number")
# write.csv(infdis, "./data/2017 sfr model data/Prevalence of infectious deseases.csv", row.names = F)

indicators <- raw.data$log %>% dplyr::filter(source == "GBD and CSIS")
filename <- c("./data/2017 sfr model data/Prevalence of infectious deseases.csv")
infdis <- read.csv(filename)
infdis <- infdis[, -c(1,3,5:11)]
infdis <- infdis %>% group_by(location_name, year) %>% dplyr::summarise(value = sum(val, na.rm = T))
infdis$variablename <- indicators$variablename[1]
infdis <- infdis %>% dplyr::rename(iso3c = location_name)
infdis <- infdis[, c("iso3c", "variablename", "year", "value")]
infdis <- infdis %>% dplyr::filter(!(iso3c %in% c("Sweden except Stockholm")))
infdis$iso3c <- country.code.name(infdis$iso3c)
infdis <- infdis %>% dplyr::filter(complete.cases(iso3c))

#Value needs to be numeric
infdis$value <- as.numeric(infdis$value)
#infdis <- most.recent(infdis)
infdis <- per.capita.calc(infdis)
infdis$iso3c <- country.code.name(infdis$iso3c)
infdis <- infdis %>% dplyr::filter(complete.cases(value))
raw.data$disease <- infdis
rmExcept("raw.data") 
