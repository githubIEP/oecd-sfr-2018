# Example preprocessing script 2018
filename <- "./data/2017 sfr model data/institutional profiles database.xlsx"
ipd <- read_excel(filename, "Indicators")
indicators <- raw.data$log %>% dplyr::filter(source == "IPD")
pos <- which(names(ipd) %in% trim(strsplit(paste(indicators$variablename, collapse = ","), ",")[[1]]))
ipd <- ipd[, c(1:3, pos)]
ipd[2, -c(1:3)] <- names(ipd)[-c(1:3)]
names(ipd) <- ipd[2, ]
ipd <- ipd[-c(1:2), -1]
ipd <- ipd %>% dplyr::rename(year = Year, iso3c = Country) %>% dplyr::filter(complete.cases(year))
ipd <- ipd %>% gather(variablename, value, -c(iso3c, year))
ipd <- ipd[, c("iso3c", "variablename", "year", "value")]
raw.data$ipd <- ipd
rmExcept("raw.data") 
