source("./lib/funcs.R")
filename <- c("./data/2017 sfr model data/WB_gini.xlsx")
# NEED to change indicators indicators <- raw.data$log %>% dplyr::filter(source == "WB")
x <- read_excel(filename)
pos <- min(grep("Country Name", unlist(x[, 1])))
names(x) <- tolower(x[pos, ])
x <- x[-c(1:pos), ]
x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
x <- x[, -c(2, 4)]
x <- x %>% gather(year, value, -c(iso3c, variablename))
x$year <- as.numeric(as.character(x$year))
x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
x$country <- x$iso3c
x <- most.recent(x)
x$iso3c <- country.code.name(x$iso3c)
x$value <- as.numeric(x$value)
x$iso3c <- country.code.name(x$iso3c)
x <- x[, c("iso3c", "variablename", "year", "value")]
x <- x %>% dplyr::filter(complete.cases(.))
#x$variablename <- "Gini_WB"
raw.data$Gini_WB <- x
rmExcept("raw.data") 
