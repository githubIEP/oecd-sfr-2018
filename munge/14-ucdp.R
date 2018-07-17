# Example preprocessing script.
source("./lib/funcs.R")

load("./data/2017 sfr model data/ucdp-nonstate-171.Rdata")

ucdp <- ucdp.nonstate

# Take care of countries with multiple locations, counting multiple locations
ucdp$CountryCount <- stringr::str_count(as.character(ucdp$location), ",")
ToSplit <- subset(ucdp, CountryCount > 0)
Conflictsub <- subset(ucdp, CountryCount == 0)  # no need to split
if (nrow(ToSplit) + nrow(Conflictsub) != nrow(ucdp)) warning("DANGER DANGER")
if (sum(ucdp$CountryCount) > 0) {
    for (i in 1:nrow(ToSplit)) {
        Splits <- ToSplit[rep(rownames(ToSplit[i, ]), ToSplit$CountryCount[i] + 1), ]
        Splits$location <- stringr::str_split(ToSplit$location[i], ", ")[[1]]
        Conflictsub <- rbind(Conflictsub, Splits)
    }
}

#we are taking only the most recent year 2016
ucdp <- Conflictsub[, c("location", "year", "fatbest")] #%>% dplyr::filter(year == max(year))
# aggregate by country
ucdp <- ucdp %>% group_by(location, year) %>% dplyr::summarise(value = sum(fatbest, na.rm = T))

#ucdp$year <- max(Conflictsub$year)
# column names
ucdp <- as.data.frame(ucdp)
names(ucdp) <- tolower(names(ucdp))
ucdp$location <- gsub("Yemen \\(North Yemen\\)", "Yemen", ucdp$location)
ucdp$iso3c <- country.code.name(ucdp$location)
#ucdp <- spread(ucdp, year, value)
#write.csv(ucdp, "./ucdp_spread_cora.csv", row.names = F)
ucdp <- per.capita.calc(ucdp)

#######################taking log does not change the ordering nor the security PC1/2 that makes Syrian an outlier
# ucdp$value <- log(ucdp$value + 1)
# ucdp$value = log(ucdp$value+1) ucdp$value[ucdp$iso3c == 'SYR'] = rev(sort(ucdp$value))[2] get
#######################
# variablename
indicators <- raw.data$log %>% dplyr::filter(source == "UCDP-NS")
ucdp$variablename <- indicators$variablename
ucdp <- ucdp[, c("iso3c", "variablename", "year", "value")]
#### add zeros for missing values
ucdp <- extend.time.series(ucdp, replace.with = 0)
ucdp <- add.zeros.for.missing.countries(ucdp, raw.data)
ucdp$iso3c <- country.code.name(ucdp$iso3c)

raw.data$ucdp <- ucdp

rmExcept("raw.data") 
