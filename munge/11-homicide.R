# Example preprocessing script 2017.
indicators <- raw.data$log %>% dplyr::filter(source == "UNODC")
filename <- "./data/2017 sfr model data/homicide_rate.xlsx"
homi <- read_excel(filename, "Intentional homicide count rate")
homi <- homi[-c(1:8), -c(1, 2, 4:17)]
names(homi) <- tolower(homi[1, ])
homi <- homi[-c(1, which(is.na(homi$`country/territory`))),]
homi$`country/territory` = gsub("\\*\\*\\*\\*", "", homi$`country/territory`)
homi$`country/territory` = gsub("\\*", "", homi$`country/territory`)
#homi <- homi %>% dplyr::filter(indicator == "Rate")
homi <- homi %>% dplyr::rename(iso3c = `country/territory`) ## %>% dplyr::select(-indicator)
homi <- homi %>% gather(year, value, -iso3c) %>% dplyr::filter(!is.na(value))
homi$variablename <- "Homicide Rate"
homi <- homi[, c("iso3c", "variablename", "year", "value")]
pos = grep("Iraq \\(", homi$iso3c)
homi$iso3c[pos] =  "Iraq"
homi$year <- as.character((homi$year))
homi = homi %>% dplyr::group_by(iso3c, variablename, year) %>%
    dplyr::summarise(value = mean(as.numeric(value))) %>% ungroup()
homi <- homi[, c("iso3c", "variablename", "year", "value")]
raw.data$homi <- homi

rmExcept("raw.data") 
