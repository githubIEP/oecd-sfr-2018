# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- "./data/2017 sfr model data/GDP_capita_WB.xlsx"
x <- read_excel(filename, "Data")
pos <- min(grep("Country Name", unlist(x[, 1])))
names(x) <- tolower(x[pos, ])
x <- x[-c(1:pos), ]
x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
x <- x[, -c(2, 4)]

#filter if different from missing in 2015 because 2016 lose a lot of countries like Venezuela, Lybia, Iran
x <- x %>% dplyr::filter(!is.na(`2015`))
#x <- x %>% dplyr::filter(!is.na(`2011`))
x <- x %>% gather(year, value, -c(iso3c, variablename))
x$value <- as.numeric(x$value)

#get rid of other years
x <- x %>% dplyr::filter(year %in% c(2010,2011,2014,2015))
x <- x %>% spread(year, value)

# 2015
x$value2015 <- (x$`2015`/x$`2011`)^(1/5) - 1

# 2014
x$value2014 <- (x$`2014`/x$`2010`)^(1/5) - 1
x <- x[-c(3:6)]
names(x) <- c("iso3c", "variablename","2015", "2014")
x <- x %>% gather(year,value,-c(iso3c, variablename))
x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
x$country <- x$iso3c
#x <- most.recent(x)
x$iso3c <- country.code.name(x$iso3c)
x <- x %>% dplyr::filter(complete.cases(.))
#x$value <- as.numeric(x$value)
# Need to also change name of the 2014 value but will not be taken in raw data list
x$variablename <- paste(x$variablename, "CAGR 2011-2015")
x <- x[, c("iso3c", "variablename", "year", "value")]
x$iso3c <- country.code.name(x$iso3c)
raw.data$gdpgrowth <- x
rmExcept("raw.data") 
