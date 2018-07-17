# Example preprocessing script.
source("./lib/funcs.R")
filename <- c("./data/2017 sfr model data/p4v2016.xls")
polity <- read_excel(filename)
polity <- polity %>% dplyr::select(country, year, democ, autoc, polity2, durable)
polity <- polity %>% dplyr::rename(iso3c = country, democracy = democ, autocracy = autoc, polity = polity2)
polity <- polity %>% gather(variablename, value, -c(iso3c, year))

# Regime persistance data 2016
filename <- c("./data/2017 sfr model data/p4v2016d.xls")
polity2 <- read_excel(filename)
polity2 <- polity2 %>% dplyr::select(country, eyear, persist)
polity2 <- polity2 %>% dplyr::filter(eyear == 9999)
polity2$eyear <- 2016
polity2 <- polity2 %>% dplyr::rename(iso3c = country, year = eyear, value = persist)
polity2$variablename <- "persistance"
# Guyana is included twice in this dataset. 
# Taking the minimum value for Guyana because the other one is French Guyana
polity2 <- polity2 %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = min(value))

# Regime persistance data 2015
filename <- c("./data/sfr model data/polity4d.xls")
polity3 <- read_excel(filename)
polity3 <- polity3 %>% dplyr::select(country, eyear, persist)
polity3 <- polity3 %>% dplyr::filter(eyear == 9999)
polity3$eyear <- 2015
polity3 <- polity3 %>% dplyr::rename(iso3c = country, year = eyear, value = persist)
polity3$variablename <- "persistance"
# Guyana is included twice in this dataset. 
# Taking the minimum value for Guyana because the other one is French Guyana
polity3 <- polity3 %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = min(value))


#Final merge
polity <- bind_rows(polity, polity2,polity3[, names(polity)])
polity <- polity %>% dplyr::filter(year > 2000, value >= 0)
polity$iso3c <- gsub("Korea North", "North Korea", polity$iso3c)
polity$iso3c <- gsub("Korea South", "South Korea", polity$iso3c)
polity <- polity %>% dplyr::filter(!(iso3c %in% c("Serbia and Montenegro")))


raw.data$polity <- polity
rmExcept("raw.data") 
