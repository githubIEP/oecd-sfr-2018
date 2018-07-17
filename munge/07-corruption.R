# Example preprocessing script 2017
filename <- "./data/2017 sfr model data/corruption perceptions index.xlsx"
ti <- read_excel(filename, "CPI 2015-2016")
#ti <- ti[, 1:2]
#ti$year <- 2016
#2016 wrong: names(ti) <- c("value", "iso3c", "year")

ti <- ti[-c(2,5:8)]
names(ti) <- c("iso3c","2016", "2015")
ti <- ti %>% gather (year, value, -iso3c)


# no need ti <- ti[-1, ]
indicators <- raw.data$log %>% dplyr::filter(source == "TI")
ti$variablename <- unique(indicators$variablename)
ti$iso3c <- gsub("Korea \\(North\\)", "North Korea", ti$iso3c)
ti$iso3c <- gsub("Korea \\(South\\)", "South Korea", ti$iso3c)
#need to move columns 
#ti <- ti(iso3c=c(1,2), variablename=c(2,3), year=c(3,4), value=c(4,5))
ti <- ti[, c("iso3c", "variablename", "year", "value")]
raw.data$ti <- ti
rmExcept("raw.data") 

