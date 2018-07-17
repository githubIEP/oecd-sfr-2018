# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "OECD")
filename <- "./data/2017 sfr model data/sigi.xlsx"
x <- read_excel(filename)
x <- x %>% dplyr::select(Country, `Restricted physical integrity Value`)
x$year <- 2014
x$variablename <- "sigi"
x <- x %>% dplyr::rename(iso3c = Country, value = `Restricted physical integrity Value`)
x <- x[, c("iso3c", "variablename", "year", "value")]
x$value = as.numeric(x$value)
x <- x %>% dplyr::filter(!is.na(value))
x$iso3c <- country.code.name(x$iso3c)
oecd.countries <- read_excel("./data/additional data/OECD countries.xlsx")
oecd.countries$iso3c <- country.code.name(oecd.countries$Country)
oecd.countries.out <- setdiff(oecd.countries$iso3c, x$iso3c)
oecd.countries.in <- intersect(oecd.countries$iso3c, x$iso3c)
pos <- which(x$iso3c %in% oecd.countries.in)
average <- mean(as.numeric(x$value[pos]), na.rm = T)
oecd.countries.out <- data.frame(iso3c = oecd.countries.out, variablename = "sigi", year = 2014, value = average)
x <- rbind(x, oecd.countries.out)
x$iso3c <- country.code.name(x$iso3c)
raw.data$sigi <- x
rmExcept("raw.data") 
