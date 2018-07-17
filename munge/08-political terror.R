#Example preprocessing script 2017
indicators <- raw.data$log %>% dplyr::filter(source == "PTS")
filename <- "./data/2017 sfr model data/political terror scale.xlsx"
pts <- read_excel(filename, "political terror scale")
names(pts) <- tolower(names(pts))
pts$amnesty <- as.numeric(pts$amnesty)
pts$state <- as.numeric(pts$state)
pts$hrw <- as.numeric(pts$hrw)
#taking the mean of the 3 indicators
pts$value <- rowMeans(select(pts, amnesty, state, hrw), na.rm = T)
# drop older years, removes old countries North Yemen, South Yemen and USSR
pts$year <- as.numeric(pts$year)
pts <- pts %>% dplyr::filter(year > 2000)
pts <- pts %>% dplyr::filter(!(country %in% c("Israel and Occupied Territories", 
                                              "Israel in Occupied Territories", 
                                              "Israel in pre-1967 borders",
                                              "Serbia and Montenegro",
                                              "Gaza (Hamas)")))
pts$country <- trim(pts$country)
pts <- pts %>% dplyr::select(country, year, value)
indicators <- raw.data$log %>% dplyr::filter(source == "PTS")
pts$variablename <- unique(indicators$variablename)
pts$iso3c <- pts$country
pts <- pts[, c("iso3c", "variablename", "year", "value")]
raw.data$pts <- pts
rmExcept("raw.data") 
