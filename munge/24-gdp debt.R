# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "WB")
filename <- "./data/2017 sfr model data/imf gov debt.xlsx"
x <- read_excel(filename)
x$X2016 <- as.numeric(x$X2016)
x <- x %>% dplyr::filter(!grepl(" net debt", Subject.Descriptor), !is.na(X2016))
x <- x %>% dplyr::filter(!grepl(" currency", Units), !is.na(X2016))

# No need for this I think because opened in xlsx.
#x[, 1] <- iconv(x[, 1], "latin1", "UTF-8")

# this implies that for example estimates for 2016 done in 2014 will result in Year 2014 values!
x <- x %>% dplyr::rename(iso3c = Country, variablename = Subject.Descriptor, year = Estimates.Start.After, value = X2016)
x <- x[, c("iso3c", "variablename", "year", "value")]
x <- x %>% dplyr::filter(complete.cases(.))
raw.data$gdpdebt <- x
rmExcept("raw.data") 
