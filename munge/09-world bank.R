# Example preprocessing script.
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- c("./data/2017 sfr model data/natural resource rent.csv")  #, #'./data/sfr model data/vulnerable employment.csv', 
# './data/sfr model data/neet.csv')#'./data/sfr model data/central government debt.csv',
wb <- lapply(filename, function(i) {
    x <- read.csv(i)
    pos <- min(grep("Country", x[, 1]))
    names(x) <- tolower(x[pos, ])
    x <- x[-c(1:pos), ]
    x <- x %>% dplyr::rename(iso3c = `country name`, variablename = `indicator name`)
    x <- x[, -c(2, 4)]
    x <- x %>% gather(year, value, -c(iso3c, variablename))
    x <- x[, c("iso3c", "variablename", "year", "value")]
    x <- x %>% dplyr::filter(!grepl("Sub-Saharan Africa", iso3c))
    return(x)
})
wb <- bind_rows(wb)
wb <- wb %>% dplyr::filter(complete.cases(value))
raw.data$wb <- wb
rmExcept("raw.data") 
