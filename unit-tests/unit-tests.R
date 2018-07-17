# #unit tests of 2017 - 2016
# library(daff)
# load("./cache/raw.data.RData")
# sfr2017 = raw.data
# load("./cache/2016/raw.data-oecd-2016.RData")
# sfr2016 = raw.data
# 
# 
# 
# y = diff_data(sfr2016, sfr2017)

# #####test that data has formatted properly 

unit.test = function(df){
    require(sqldf)
    require(pbapply)
    load("./cache/2016/raw.data-oecd-2016.RData")
    df = split(df, factor(df$variablename))
    df = pblapply(df, function(x){
        x = x %>% dplyr::filter(complete.cases(.)) %>% dplyr::select(iso3c, year, variablename, value)
        temp = sfr2016 %>% dplyr::filter(variablename %in% x$variablename, 
                                         iso3c %in% x$iso3c, !is.na(value))
        temp = temp[,names(x)]
        temp$value = abs(temp$value)
        x$value = abs(x$value)
        y = NULL
        if (nrow(sqldf("select * from `x` except select * from `temp`"))>0){
            y = data.frame(type = "in 2017 not in 2016", 
                           sqldf("select * from `x` except select * from `temp`"))
        }
        
        if (nrow(sqldf("select * from `temp` except select * from `x`"))>0){
            y = rbind(y, data.frame(type = "in 2016 not in 2017", 
                                    sqldf("select * from `temp` except select * from `x`")))
        }
        
        return(y)
    })
    pos = unlist(lapply(df, nrow)) > 0
    df = bind_rows(df[pos], .id = "name")
    return(df)
    
}

#HAVE PROBLEM WITH HOMI - WHERE DOES IT GO?
#HAVE PROBLEM WITH CALCULATION OF GDP GROWTH RATES AND URBANISATION RATES EVEN THOUGH THEY ARE THE SAME

#expect_that(length(df), equals(0), label = names(df))
#Everything is ok till here so the difference happens later...
