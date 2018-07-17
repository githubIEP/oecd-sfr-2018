# Example preprocessing script.
filename <- "./data/2017 sfr model data/gcri_data_v5.0.1.xlsx"
gcri <- read_excel(filename)
gcri <- gcri[, -1]

gcri <- gcri %>% dplyr::rename(iso3c = COUNTRY, year = YEAR) %>% dplyr::select(-ISO) %>% gather(variablename, value, 
    -c(iso3c, year))
indicators <- raw.data$log %>% dplyr::filter(source == "GCRI")
gcri <- gcri %>% dplyr::filter(variablename %in% indicators$variablename)
gcri <- gcri[, c("iso3c", "variablename", "year", "value")]
raw.data$gcri <- gcri
rmExcept("raw.data") 
