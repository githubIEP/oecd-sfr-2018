# Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")

#### ILO NEET DATA
filename <- c("./data/2017 sfr model data/ILO_NEET.csv")
ilo <- read.csv(filename)
pos <- min(grep("Reference area", ilo[,1 ]))
names(ilo) <- tolower(ilo[pos, ])
ilo <- ilo[-c(1:pos), ]
ilo <- ilo[, -c(2,5,6)]
ilo <- ilo %>% dplyr::rename(iso3c = `reference area`, year = `time`, value=`total`)
ilo$variablename <-"Share of youth not in education, employment, or training, total (% of youth population)"
ilo <- ilo[, c("iso3c", "variablename", "year", "value")]
ilo$iso3c = gsub("R\xe9union", "Reunion", ilo$iso3c)
ilo$iso3c <- country.code.name(country.code.name(ilo$iso3c))
ilo <- ilo %>% dplyr::filter(complete.cases(iso3c))
#ilo <- most.recent(ilo)

###WORLD BANK DATA

# filename <- c("./data/2017 sfr model data/WB_Neet.csv")
# wb <- lapply(filename, function(i) {
#     x <- read.csv(i)
#     pos <- min(grep("Country", x[, 1]))
#     names(x) <- tolower(x[pos, ])
#     x <- x[-c(1:pos), ]
#     x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
#     x <- x[, -c(2, 4)]
#     x <- x %>% gather(year, value, -c(iso3c, variablename))
#     x <- x[, c("iso3c", "variablename", "year", "value")]
#     x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
#     return(x)
# })
# wb <- bind_rows(wb)
# wb <- wb %>% dplyr::filter(complete.cases(value))
# wb$iso3c <- country.code.name(country.code.name(wb$iso3c))
# wb <- wb %>% dplyr::filter(complete.cases(iso3c))
# wb <- most.recent(wb)
# temp <- setdiff(unique(wb$iso3c), unique(ilo$iso3c))
# wb <- wb %>% dplyr::filter(iso3c %in% temp)

#### Add OECD Data for Korea
oecd <- read.csv("./data/2017 sfr model data/OECD_neet.csv")
oecd$Value <- as.numeric(oecd$Value)
oecd <- oecd %>% dplyr::filter(complete.cases(Value))
names(oecd)[1] <- "LOCATION"
oecd <- oecd %>% group_by(LOCATION, TIME) %>% summarise(value = mean(Value, na.rm = T))
oecd$variablename <- ilo$variablename[1]
oecd <- oecd %>% dplyr::rename(iso3c = LOCATION, year = TIME)
#oecd <- most.recent(oecd)
oecd$iso3c <- country.code.name(country.code.name(oecd$iso3c))
temp <- setdiff(unique(oecd$iso3c), unique(ilo$iso3c))
oecd <- oecd %>% dplyr::filter(iso3c %in% temp)
oecd$value <- as.character(oecd$value)
oecd$year <- as.character(oecd$year)
ilo <- bind_rows(ilo, oecd)

### Supplement with WDR Job data
neet.wdr <- read_excel("./data/2017 sfr model data/WDR neet.xlsx")
names(neet.wdr) <- paste(unlist(neet.wdr[2, ]), letters[1:7], sep = "")
neet.wdr <- neet.wdr[-c(1:2), ]
names(neet.wdr)[1] <- "iso3c"
neet.wdr <- neet.wdr %>% gather(year, value, -iso3c)
neet.wdr$year <- as.numeric(substr(neet.wdr$year, 1, 4))
neet.wdr$value <- as.numeric(neet.wdr$value)
neet.wdr <- neet.wdr %>% dplyr::filter(!is.na(value))
neet.wdr <- neet.wdr %>% group_by(iso3c, year) %>% dplyr::summarise(value = mean(value, na.rm = T)) %>% ungroup()
neet.wdr$iso3c <- country.code.name(country.code.name(neet.wdr$iso3c))
temp <- setdiff(unique(neet.wdr$iso3c), unique(ilo$iso3c))
temp <- neet.wdr %>% dplyr::filter(iso3c %in% temp)
temp$variablename <- ilo$variablename[1]
ilo <- rbind(ilo, temp[, names(ilo)])
ilo <- bind_rows(ilo)
#ilo <- most.recent(ilo)
raw.data$neet <- ilo

rmExcept("raw.data") 
