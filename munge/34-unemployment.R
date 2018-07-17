# # Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/Unemployment_rate_WB.xlsx"
unemp <- read_excel(filename)
names(unemp) <- unemp[4, ]
unemp <- unemp[-c(1:4), ]
unemp <- unemp[, -c(2:4)]
names(unemp)[1] <- "iso3c"
unemp <- unemp %>% gather(year, value, -iso3c)
unemp$value <- as.numeric(unemp$value)
unemp$year <- as.numeric(as.character(unemp$year))
unemp <- unemp %>% dplyr::filter(complete.cases(.))
unemp$variablename <- "unemp"
unemp <- unemp[, c("iso3c", "variablename", "year", "value")]
unemp$iso3c <- country.code.name(unemp$iso3c)
unemp$iso3c <- oecd.country.name(unemp$iso3c)
unemp <- unemp %>% dplyr::filter(complete.cases(.))
unemp <- unemp %>% group_by(iso3c, variablename, year) %>% dplyr::summarise(value = max(value)) %>% ungroup()
raw.data$unemp <- unemp
rmExcept("raw.data") 
