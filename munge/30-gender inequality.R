# # Example preprocessing script.
source("./lib/funcs.R")
indicators <- raw.data$log %>% dplyr::filter(source == "UNDP/HDI Gender Inequality")
filename <- "./data/2017 sfr model data/Gender Inequality Index (GII).xlsx"
x <- read_excel(filename)

# pos <- c(min(grep("Country", unlist(x[4, ]))), 
#          min(grep("Gender", unlist(x[2, ]))),
#          min(grep("Labour", unlist(x[2, ]))) + 2,
#          min(grep("Labour", unlist(x[2, ]))))
# x <- x[, pos]
# names(x) <- c("rank","iso3c", trim(unlist(x[2, ])[-1]))

names(x) <- as.character(unlist(x[1,]))
x <- x[-1,-1]
colnames(x)[1] <- "iso3c"

# names(x)[3:4] <- c(paste(names(x)[3], "Male"), paste(names(x)[4], "Female"))
# x <- x[-c(1:(grep("Norway", x$iso3c) - 1)), ]
# x <- x[1:grep("Tuvalu", x$iso3c), ]

x <- x %>% gather(year, value, -iso3c)
x$value <- as.numeric(x$value)
x <- x %>% dplyr::filter(!is.na(value))
#x$year <- 2015
x$variablename <- paste(x$variablename, "Gender Inequality Index")
x <- x[, c("iso3c", "variablename", "year", "value")]

# labour force participation rate female

filename <- "./data/2017 sfr model data/Labour force participation rate, female (% ages 15 and older).xlsx"
x1 <- read_excel(filename)
names(x1) <- as.character(unlist(x1[1,]))
x1 <- x1[-1,-1]
colnames(x1)[1] <- "iso3c"
x1 <- x1 %>% gather(year, value, -iso3c)
x1$value <- as.numeric(x1$value)
x1 <- x1 %>% dplyr::filter(!is.na(value))
x1$variablename <- paste(x1$variablename, "Labour force participation rate Female")
x1 <- x1[, c("iso3c", "variablename", "year", "value")]

#labour force participation rate male
filename <- "./data/2017 sfr model data/Labour force participation rate, male (% ages 15 and older).xlsx"
x2 <- read_excel(filename)

names(x2) <- as.character(unlist(x2[1,]))
x2 <- x2[-1,-1]
colnames(x2)[1] <- "iso3c"
x2 <- x2 %>% gather(year, value, -iso3c)
x2$value <- as.numeric(x2$value)
x2 <- x2 %>% dplyr::filter(!is.na(value))
x2$variablename <- paste(x2$variablename, "Labour force participation rate Male")
x2 <- x2[, c("iso3c", "variablename", "year", "value")]

total <- rbind(x,x1,x2)
total$year <- as.numeric(total$year)
total$iso3c = gsub(" C\xf4te d'Ivoire", "Cote d'Ivoire", total$iso3c)
total$iso3c <- country.code.name(country.code.name(total$iso3c))

raw.data$gender <- total
rmExcept("raw.data") 
