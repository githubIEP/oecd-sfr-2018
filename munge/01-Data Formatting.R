# Example preprocessing script.
source("./lib/load-libraries.R")
sfr.log <- read_excel("./data/indicator master list/SFR2017 Indicator Master List.xlsx", "raw.data.for.R")
sfr.log <- sfr.log %>% fill(dimension)
raw.data <- list(log = sfr.log)
expect_that(length(unique(sfr.log$variablename[sfr.log$include == 1])), equals(length(unique(sfr.log$reportname[sfr.log$include == 
    1]))))
rmExcept("raw.data")
 
