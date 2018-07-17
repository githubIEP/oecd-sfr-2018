# This script processes all the indicators in a standard format for the PCA

source("./lib/funcs.R")
if(!dir.exists("./data_out2017"))
{
    dir.create("./data_out2017")
}
save(list = ls(), file = "./cache/errortesting/errortesting.RData")
load("./cache/errortesting/errortesting.RData")

sfr.log <- raw.data$log %>% dplyr::select(reportname, variablename, dimension, type, doesmoreincreasefragility, 
    include)
old.raw <- raw.data
raw.data <- raw.data[-1]
raw.data <- lapply(raw.data, function(x) {
    x$iso3c <- as.character(x$iso3c)
    x$variablename <- as.character(x$variablename)
    x$year <- as.numeric(as.character(x$year))
    x$value <- as.numeric(as.character(x$value))
    return(x)
}) 

raw.data <- bind_rows(raw.data)
raw.data$iso3c <- trim(raw.data$iso3c)
raw.data$variablename <- trim(raw.data$variablename)
#raw.data$value = round(raw.data$value, 3)


raw.data$iso3c <- str_replace_all(raw.data$iso3c, "[[:punct:]]", "")
raw.data$country <- raw.data$iso3c
raw.data$iso3c <- country.code.name(raw.data$iso3c)
country.code.check = raw.data %>% dplyr::select(country, iso3c) %>% distinct()
write.csv(country.code.check, file = "./unit-tests/country-code-conversions.csv", row.names = F)
message("country code conversions written to ./unit-tests/country-code-conversions.csv")
message("****Please check that these are converted correctly****")
raw.data <- raw.data %>% dplyr::filter(complete.cases(value))
pos <- is.na(raw.data$iso3c)
unique(raw.data$country[pos])
# pos <- grepl("sudan", tolower(raw.data$country)) & is.na(raw.data$iso3c)
# raw.data$iso3c[pos] <- "SSD"
pos <- grepl("vietnam", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "VNM"

pos <- grepl("serbia", tolower(raw.data$country)) & is.na(raw.data$iso3c)
raw.data$iso3c[pos] <- "SRB"

raw.data <- raw.data %>% dplyr::filter(!is.na(iso3c))
raw.data$country <- country.code.name(raw.data$iso3c)

# filter data for index
raw.data <- left_join(raw.data, sfr.log)
raw.data <- ungroup(raw.data)
raw.data <- raw.data %>% dplyr::filter(include == 1)
raw.data <- raw.data %>% dplyr::select(iso3c, country, dimension, type, reportname, year, value, doesmoreincreasefragility)
raw.data <- raw.data %>% dplyr::rename(variablename = reportname)

#check time series
time.span = raw.data %>% dplyr::group_by(variablename) %>%
    dplyr::summarise(min.year = min(year), max.year = max(year), timespan = max.year-min.year+1) %>%
    dplyr::filter(timespan==1)

write.csv(time.span, "./graphs/only-one-year-of-data.csv", row.names = F)

# #####test that data has formatted properly #i.e. one data point per country-variablename
sfr.time.series <- raw.data
raw.data <- most.recent(raw.data)
df <- raw.data %>% dplyr::select(iso3c, variablename, value) %>% distinct()
df <- df %>% group_by(iso3c, variablename) %>% dplyr::summarise(n = n()) %>% ungroup() 
expect_that(max(df$n), equals(1))

#### There are some double entries for countries in Political Terror Scales with governance indicators
### Prevalence of Infectious Disease (deaths per 100,000)Formal for Sweden (the smallest is Sweden except Stochkolm)
#### This is fine to take avearges of because the scores are the same in the duplications raw.data2 =
# raw.data %>% group_by(iso3c, dimension, type, variablename, year, doesmoreincreasefragility) %>%
# summarise(value = mean(value, na.rm=T)) %>% ungroup() #test that there are no duplicates
pos <- duplicated(raw.data)
expect_that(sum(duplicated(raw.data)), equals(0))

###### remove countries with less than threshold percentage of data
threshold <- 0.7
availability <- as.data.frame(table(raw.data$iso3c, raw.data$variablename))
availability <- availability %>% group_by(Var1) %>% dplyr::summarise(n = n(), missing = sum(Freq == 0)/n())
availability <- availability %>% dplyr::filter(1 - missing >= threshold) %>% ungroup()
raw.data <- raw.data %>% dplyr::filter(iso3c %in% as.character(availability$Var1))

# invert indictors to be in the same direction
pos <- raw.data$doesmoreincreasefragility == 0
raw.data$value[pos] <- -raw.data$value[pos]

data.matrix <- raw.data %>% group_by(variablename) %>% dplyr::summarise(min.year = min(year), max.year = max(year), 
    num.countries = length(unique(iso3c)))
write.csv(data.matrix, "./data_out2017/indicator-country-years.csv", row.names = F)

#raw data matrix
data.matrix <- raw.data %>% dplyr::select(iso3c, country, variablename, year, value) %>% distinct() %>%
    spread(year, value)
write.csv(data.matrix, "./data_out2017/raw-data-country-years-matrix.csv", row.names = F)

# impute and perform pca transformation ## CHECK HERE
raw.data <- impute(raw.data, use.precomupted = T)
raw.data$country <- oecd.country.name(raw.data$iso3c, short = T)
raw.data$country <- iconv(raw.data$country, "latin1", "UTF-8")


pos <- sfr.time.series$doesmoreincreasefragility == 0
sfr.time.series$value[pos] <- -sfr.time.series$value[pos]
sfr.time.series = sfr.time.series %>% dplyr::filter(iso3c %in% raw.data$iso3c)
temp = raw.data %>% dplyr::select(-value) %>% 
    dplyr::rename(value = imputed)
temp = temp[, names(sfr.time.series)]
sfr.time.series <- rbind(sfr.time.series, temp)  %>% dplyr::distinct() 
sfr.time.series <- sfr.time.series %>% dplyr::group_by(iso3c, year, variablename) %>%
    dplyr::summarise(value = mean(value, na.rm=T))
sfr.time.series <- interpolate(sfr.time.series %>% select(iso3c, year, variablename, value))
sfr.time.series <- sfr.time.series %>% dplyr::rename(imputed = yhat, reportname = variablename)
sfr.time.series <- left_join(sfr.time.series, sfr.log)
sfr.time.series = sfr.time.series %>% dplyr::filter(year >= 2008)
sfr.time.series <- sfr.time.series %>% dplyr::select(-variablename) %>% 
    dplyr::rename(variablename = reportname)
sfr.time.series <- sfr.time.series %>% dplyr::filter(include == 1)
sfr.time.series$value = sfr.time.series$imputed
sfr.time.series$imputed = round(sfr.time.series$imputed, 3)
raw.data$imputed = round(raw.data$imputed, 3)
x = most.recent(sfr.time.series) %>% select(-year) %>% arrange(iso3c, variablename) %>%
    dplyr::select(iso3c, variablename,imputed)
y = raw.data  %>% select(-year)   %>% arrange(iso3c, variablename) %>%
    dplyr::select(iso3c, variablename,imputed)
#test time series matches most recent year
test <- identical(x,y)
expect_that(test, equals(TRUE))

#data direction check
df.check <- sfr.time.series %>% group_by(variablename, dimension) %>% 
    dplyr::summarise(worst = iso3c[which(value == min(value))[1]], 
                     worst.year = year[which(value == min(value))[1]], 
                     worst.value = min(value), 
                     best = iso3c[which(value == max(value))[1]], 
                     best.year = year[which(value == max(value))[1]], 
                     best.value = max(value)) 

rmExcept(c("raw.data", "sfr.time.series"))

pos <- sfr.time.series$type == "Coping"
sfr.time.series$variablename[pos] <- paste(sfr.time.series$variablename[pos], " (C)", sep = "")
sfr.time.series$variablename[!pos] <- paste(sfr.time.series$variablename[!pos], " (R)", sep = "")

df.check <- sfr.time.series %>% group_by(variablename) %>% 
    dplyr::summarise(worst = iso3c[which(value == min(value))[1]], 
                     worst.year = year[which(value == min(value))[1]], 
                     worst.value = min(value), 
                     best = iso3c[which(value == max(value))[1]], 
                     best.year = year[which(value == max(value))[1]], 
                     best.value = max(value)) 
write.csv(df.check, "./data_out2017/data-direction-check.csv", row.names = F)

