source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "UNDP/HDI")
filename <- file.path("data", "sfr model data", "2015_statistical_annex_tables_all.xls")
x <- read_excel(filename, "Table 13")
pos <- c(min(grep("Country", unlist(x[3, ]))), min(grep("Vulnerable", unlist(x[2, ]))))
x <- x[, pos]
x$year <- 2013
names(x) <- c("iso3c", "value", "year")
x$value <- as.numeric(x$value)
x <- x %>% dplyr::filter(!is.na(value))
x$variablename <- indicators$variablename
x <- x[, c("iso3c", "variablename", "year", "value")]
x$iso3c <- country.code.name(x$iso3c)
x <- x %>% dplyr::filter(complete.cases(.))

# Get WB data
filename <- file.path("data", "sfr model data", "vulnerable employment.csv")
wb <- lapply(filename, function(i) {
    x <- read.csv(i)
    pos <- min(grep("Country", x[, 1]))
    names(x) <- tolower(x[pos, ])
    x <- x[-c(1:pos), ]
    x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
    x <- x[, -c(2, 4)]
    x <- x %>% gather(year, value, -c(iso3c, variablename))
    x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
    x$year <- as.numeric(as.character(x$year))
    return(x)
})

wb <- bind_rows(wb)
wb <- wb %>% dplyr::filter(complete.cases(value))
#wb <- most.recent(wb)
wb$iso3c <- country.code.name(wb$iso3c)
wb <- wb %>% dplyr::filter(complete.cases(.))
# add from WB what's not in HDI
test <- setdiff(wb$iso3c, x$iso3c)
wb <- wb %>% dplyr::filter(iso3c %in% test)
x <- rbind(x, wb)
x$iso3c <- country.code.name(x$iso3c)
x$variablename <- indicators$variablename
raw.data$vulnerable <- x
rmExcept("raw.data") 
