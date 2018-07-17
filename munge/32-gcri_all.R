# Example preprocessing script.
source("./lib/funcs.R")
filename <- "./data/2017 sfr model data/gcri.csv"
gcri <- read.csv(filename)
gcri <- gcri[, -c(1:2)]
gcri$average.gcri <- rowMeans(gcri[, 3:4])
gcri <- gcri %>% dplyr::rename(iso3c = COUNTRY, year = YEAR) %>% gather(variablename, value, -c(iso3c, year))
indicators <- raw.data$log %>% dplyr::filter(source == "GCRI")
gcri <- gcri %>% dplyr::filter(variablename %in% indicators$variablename)
gcri <- gcri[, c("iso3c", "variablename", "year", "value")]
gcri$iso3c <- country.code.name(gcri$iso3c)
oecd.countries <- read_excel("./data/additional data/OECD countries.xlsx")
oecd.countries$iso3c <- country.code.name(oecd.countries$Country)
oecd.countries.out <- setdiff(oecd.countries$iso3c, gcri$iso3c)
oecd.countries.in <- intersect(oecd.countries$iso3c, gcri$iso3c)
pos <- which(gcri$iso3c %in% oecd.countries.in)
average <- mean(gcri$value[pos])
oecd.countries.out <- data.frame(iso3c = oecd.countries.out, variablename = "average.gcri", year = 2014, 
    value = average)
gcri <- rbind(gcri, oecd.countries.out)
gcri$iso3c <- country.code.name(gcri$iso3c)

raw.data$gcri.all <- gcri
rmExcept("raw.data") 
