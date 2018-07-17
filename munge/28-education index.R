# Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "HDI")
filename <- c("./data/2017 sfr model data/Education Index.xlsx")
edu <- read_excel(filename)
names(edu) <- edu[1, ]
edu <- edu[-c(1), -1]
#edu <- edu[1:which(edu$Country == "Zimbabwe"), ]
edu <- edu %>% gather(year, value, -Country)
edu$year <- as.numeric(as.character(edu$year))
edu$value <- as.numeric(edu$value)
edu <- edu %>% dplyr::filter(!is.na(value))
edu$variablename <- indicators$variablename[1]
edu <- edu %>% dplyr::rename(iso3c = Country)
edu <- edu[, c("iso3c", "variablename", "year", "value")]
raw.data$edu <- edu
rmExcept("raw.data") 
