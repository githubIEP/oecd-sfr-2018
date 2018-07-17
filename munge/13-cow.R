# Example preprocessing script 2017. The only change to 2016 script is that I added Montenegro to NATO countries
filename <- "./data/2017 sfr model data/formal alliances.csv"
source("./lib/funcs.R")
cow <- read.csv(filename)
cow <- cow %>% dplyr::filter(is.na(all_end_year))
cow <- cow %>% group_by(state_name) %>% 
        dplyr::summarise(variablename = "Formal Security Alliances", year = 2012, 
        value = n()) %>% ungroup()
cow$iso3c <- cow$state_name
cow <- cow[, c("iso3c", "variablename", "year", "value")]
cow$iso3c <- gsub("Yemen Arab Republic", "Yemen", cow$iso3c)
cow$iso3c <- gsub("Yemen People's Republic", "Yemen", cow$iso3c)
cow$iso3c <- country.code.name(cow$iso3c)
cow <- cow %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = sum(value)) %>% ungroup()
cow <- add.zeros.for.missing.countries(cow, raw.data)

# add nato
nato <- read_excel("./data/2017 sfr model data/nato.xlsx")
nato$iso3c <- country.code.name(nato$Country)
nato$value <- nrow(nato)
nato <- data.frame(iso3c = nato$iso3c, variablename = cow$variablename[1], year = 2017, value = nato$value - 
                           1)
# temp = setdiff(cow$iso3c, nato$iso3c) commented out 30 August cow = cow %>% dplyr::filter(iso3c %in% temp)
# commented out 30 August
cow <- rbind(cow, nato)
cow <- cow %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = max(value))
# 
cow$iso3c <- country.code.name(cow$iso3c)
raw.data$cow <- cow
rmExcept("raw.data") 
