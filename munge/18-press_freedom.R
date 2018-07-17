# Example preprocessing script.
filename <- "./data/sfr model data/world-press-freedom-index.csv"
press <- read.csv(filename)
press <- press %>% dplyr::select(EN_country, Overall.Score.2016)
press$Overall.Score.2016 <- as.numeric(gsub(",", ".", press$Overall.Score.2016))
press$year <- 2016
press$variablename <- "Freedom of the Press"
names(press) <- c("iso3c", "value", "year", "variablename")
press <- press[, c("iso3c", "variablename", "year", "value")]
press <- press %>% dplyr::filter(iso3c != "Cyprus North")
raw.data$press <- press
rmExcept("raw.data")
 
