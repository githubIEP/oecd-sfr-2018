# Example preprocessing script.
source("./lib/funcs.R")
#load("./data/sfr model data/124934_1ucdp-brd-conflict-2015.rdata")
load("./data/2017 sfr model data/ucdp-brd-conf-171.Rdata")

ucdp <- ucdp.brd.conf
# if Best estiamte is missing use low estimate (I think that this is already the case)
ucdp$BdBest[ucdp$BdBest < 0] <- ucdp$BdLow[ucdp$BdBest < 0]
# Take care of countries with multiple locations, generate a variable with number of locations per observation
ucdp$CountryCount <- stringr::str_count(as.character(ucdp$LocationInc), ",")
ToSplit <- subset(ucdp, CountryCount > 0)
#write.csv(ToSplit, "multiple_locations.csv")

Conflictsub <- subset(ucdp, CountryCount == 0)  # no need to split
if (nrow(ToSplit) + nrow(Conflictsub) != nrow(ucdp)) warning("DANGER DANGER")
# checks for duplications in the data
if (sum(ucdp$CountryCount) > 0) {
    for (i in 1:nrow(ToSplit)) {
        Splits <- ToSplit[rep(rownames(ToSplit[i, ]), ToSplit$CountryCount[i] + 1), ]
        Splits$LocationInc <- stringr::str_split(ToSplit$LocationInc[i], ", ")[[1]]
        Conflictsub <- rbind(Conflictsub, Splits)
    }
}
ucdp <- Conflictsub[, c("LocationInc", "Year", "BdBest")]# %>% dplyr::filter(Year == max(Year))

# aggregate value by country
ucdp <- ucdp %>% dplyr::group_by(LocationInc, Year) %>% dplyr::summarise(value = sum(BdBest, na.rm = T)) %>%
    ungroup()

# column names
# it's already a dataframe: ucdp <- as.data.frame(ucdp)
names(ucdp) <- tolower(names(ucdp))
ucdp <- ucdp %>% dplyr::rename(location = locationinc)
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)
# average deaths
ucdp <- ucdp %>% dplyr::group_by(iso3c, year) %>% dplyr::summarise(value = mean(value))
# need to get populations
#ucdp$year <- max(ucdp.brd.conf$Year)
ucdp <- per.capita.calc(ucdp)

# outliers (syria)
ucdp$value <- log(ucdp$value + 1)

# ucdp$value[ucdp$iso3c == 'SYR'] = rev(sort(ucdp$value))[2] syria = ucdp$iso3c == 'SYR'
# ucdp$value[syria] = 1.2 * sort(ucdp$value, decreasing = T)[2] get variablename
indicators <- raw.data$log %>% dplyr::filter(source == "UCDP-BD")
ucdp$variablename <- indicators$variablename
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
#### add zeros for missing values
ucdp <- extend.time.series(ucdp, replace.with = 0)
ucdp <- add.zeros.for.missing.countries(ucdp, raw.data)
###### 
ucdp$iso3c <- country.code.name(ucdp$iso3c)
# ucdp <- spread(ucdp, year, value)
# write.csv(ucdp, "./battle_related_deaths.csv")
raw.data$bd <- ucdp

######################################
years.of.peace <- Conflictsub %>% group_by(LocationInc) %>% dplyr::summarise(last.yr = max(Year))
years.of.peace$LocationInc <- gsub("Yemen \\(North Yemen\\)", "Yemen", years.of.peace$LocationInc)

years.of.peace$iso3c <- country.code.name(country.code.name(as.character(years.of.peace$LocationInc)))

years.of.peace <- years.of.peace %>% dplyr::select(-LocationInc) %>% group_by(iso3c) %>% dplyr::summarise(last.yr = max(last.yr))
ucdp <- left_join(ucdp, years.of.peace)
ucdp$value <- ucdp$year - ucdp$last.yr

### how many years without battledeath? But a bit misleading because UK, USA, Australia all result to be in war in 2003 
ucdp$value[is.na(ucdp$value)] <- max(ucdp$year) - 1945
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
ucdp$variablename <- "Years Since In Country No Battledeath"
raw.data$years.of.peace <- ucdp
rmExcept("raw.data") 

### how many years without conflict? But a bit misleading because UK, USA, Australia all result to be in war in 2003 

