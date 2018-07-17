# Example preprocessing script.
filename <- "./data/2017 sfr model data/V-Dem.csv"
source("./lib/funcs.R")
vdem <- read.csv(filename, encoding = "UTF-8")
# vdem <- vdem %>% dplyr::filter(year > 2000)
# write.csv(vdem, "./data/2017 sfr model data/V-Dem.csv", row.names = F)

# drop older years, removes old countries Yemen's People Republic, USSR, Republic of Vietnam, Serbia
# and Montenegro.

vdem$iso3c <- gsub("Congo_Democratic Republic of", "Democratic Republic Congo", vdem$country_name)
vdem$iso3c <- gsub("Democratic Republic of Vietnam", "Vietnam", vdem$iso3c)
vdem$iso3c <- gsub("Congo_Republic of the", "Democratic Republic Congo", vdem$iso3c)
vdem$iso3c <- gsub("Korea_North", "North Korea", vdem$iso3c)
vdem$iso3c <- gsub("Korea_South", "South Korea", vdem$iso3c)
vdem$iso3c <- gsub("Burma_Myanmar", "Burma Myanmar", vdem$iso3c)
pos <- grep("Palestine", vdem$iso3c)
vdem$iso3c[pos] <- "Palestine"
pos <- grep("Somali", vdem$iso3c)
vdem$iso3c[pos] <- "Somalia"
#need to gather data before
vdem <- vdem %>% gather(variablename, value, -c(country_name, country_text_id, year, iso3c))
vdem <- vdem[, c("iso3c", "variablename", "year", "value")]

##############Including old data on control over territory
filename <- "./data/2017 sfr model data/v-dem_control.csv"
vdem_c <- read.csv(filename)
# drop older years, removes old countries Yemen's People Republic, USSR, Republic of Vietnam, Serbia
# and Montenegro.
vdem_c <- vdem_c %>% dplyr::filter(year > 2000)
vdem_c$iso3c <- gsub("Congo_Democratic Republic of", "Democratic Republic Congo", vdem_c$iso3c)
vdem_c$iso3c <- gsub("Vietnam_Democratic Republic of", "Vietnam", vdem_c$iso3c)
vdem_c$iso3c <- gsub("Congo_Republic of the", "Democratic Republic Congo", vdem_c$iso3c)
vdem_c$iso3c <- gsub("Korea_North", "North Korea", vdem_c$iso3c)
vdem_c$iso3c <- gsub("Korea_South", "South Korea", vdem_c$iso3c)
vdem_c$iso3c <- gsub("Burma_Myanmar", "Burma Myanmar", vdem_c$iso3c)
pos <- grep("Palestine", vdem_c$iso3c)
vdem_c$iso3c[pos] <- "Palestine"
pos <- grep("Somali", vdem_c$iso3c)
vdem_c$iso3c[pos] <- "Somalia"

vdem <- rbind(vdem, vdem_c)
############

# to account for Paletine being west bank and gaza strip and somalia and somaliland take the minimum
# number for either
vdem <- vdem %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = min(value))
# vdem$iso3c = country.code.name(vdem$iso3c)

#For consistency reasons with 2016, I would need to drop gini before 2012
vdem <- vdem %>% dplyr::filter(year >= 2012)

raw.data$vdem <- vdem
rmExcept("raw.data")
 
