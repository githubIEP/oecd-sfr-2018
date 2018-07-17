#create raw base data
agg = lapply(all.pca$Aggregate, function(x){
    x = data.frame(iso3c = x$iso3c, value = x$x[,1])
    return(x)
})
agg = bind_rows(agg, .id = "year")


##Which countries have been fragile over time
extr.fragility = -2.5
fragility = -1.2

tmp = agg %>% dplyr::group_by(year) %>% dplyr::mutate(fragile = between(value, extr.fragility, fragility), 
                                                      extreme = value < extr.fragility) %>% 
    dplyr::ungroup() 
tmp = tmp %>% dplyr::select(-value)  %>% gather(variablename, value, -c(iso3c, year))
tmp = tmp %>% dplyr::filter(value)
tmp$country = oecd.country.name(tmp$iso3c)

write.csv(tmp %>% dplyr::filter(year > 2014), "./data_out2017/fragile-countries-time-series.csv", 
          row.names = F)

#Averages over time

tmp = agg %>% dplyr::group_by(year) %>% 
    dplyr::mutate(fragility = findInterval(value, c(-50, extr.fragility, fragility, 50))) %>% 
    dplyr::ungroup() 
tmp$fragility = c("Extreme", "Fragile", "Rest of the World")[tmp$fragility]


tmp = tmp %>% dplyr::group_by(fragility, year) %>% dplyr::summarise(value = mean(value)) %>% ungroup() %>%
    mutate(year = as.numeric(year)) 
p = ggplot(tmp, aes(x = year, y = value, colour = fragility)) + geom_line(size = 2)
p = oecd.plot(p) + scale_x_continuous(breaks = tmp$year)
p = p + labs(x = "", y = "Global Average Fragility Score",
             title = "Global Average Fragility Trend",
             subtitle = "Extremely Fragile countries have become more fragile since 2008.")


###Indicator Changes Libya Case Study
pc = read.csv("./data_out2017/principal-components-2014-2017.csv")
pc = pc %>% dplyr::filter(Aggregate.PC1 < fragility) %>% 
    gather(variablename, value, -c(iso3c, year)) %>% dplyr::filter(grepl("PC1", variablename))

deltas = hpc.change(pc) %>% dplyr::filter(num.years == max(num.years)) %>% 
    dplyr::arrange(absolute.diff) %>% dplyr::filter(iso3c == "LBY") %>% 
    dplyr::select(-c(prop.growth, annual.prop.growth))
deltas$change = ifelse(deltas$absolute.diff < 0, "Deterioration", "Improvement")

load("./cache/sfr.time.series.RData")

raw.deltas = hpc.change(sfr.time.series %>% dplyr::filter(year >= 2014, iso3c == "LBY")) %>% 
    dplyr::filter(num.years == max(num.years))
raw.deltas = left_join(raw.deltas, sfr.time.series %>% dplyr::select(variablename, dimension) %>%
                           distinct())
raw.deltas = MoveFront(raw.deltas, "dimension") %>% dplyr::arrange(absolute.diff)
raw.deltas$change = ifelse(raw.deltas$absolute.diff > 0, "Deterioration", "Improvement")
raw.deltas$change = ifelse(raw.deltas$absolute.diff == 0, "No Change", raw.deltas$change)
raw.deltas = raw.deltas %>% dplyr::select(-c(prop.growth, annual.prop.growth))


###Dimensional Changes using PC1
pc = read.csv("./data_out2017/principal-components-2014-2017.csv")
pc = pc %>% dplyr::filter(Aggregate.PC1 < fragility) %>% 
    gather(variablename, value, -c(iso3c, year)) %>% dplyr::filter(grepl("PC1", variablename))


deltas = hpc.change(pc) %>% dplyr::filter(num.years == max(num.years)) %>% 
    dplyr::arrange(absolute.diff) %>% 
    dplyr::select(-c(prop.growth, annual.prop.growth))
deltas$change = ifelse(deltas$absolute.diff < 0, "Deterioration", "Improvement")

fragile.countries = read.csv("./data_out2017/fragile-countries-time-series.csv")
fragile.countries = fragile.countries %>% dplyr::rename(fragility = variablename)
deltas = left_join(deltas, fragile.countries %>% dplyr::select(iso3c, year, fragility))
deltas = deltas %>% dplyr::group_by(variablename) %>% dplyr::summarise(value = mean(absolute.diff)) %>%
    dplyr::filter(!grepl("Aggregate", variablename))
deltas = deltas %>% dplyr::arrange(value)
deltas$variablename = factr(deltas$variablename)
deltas$type = ifelse(sign(deltas$value) < 0, "Became More Fragile", "Became Less Fragile")
deltas$type = factr(deltas$type, c("Became More Fragile", "Became Less Fragile"))
p = ggplot(deltas, aes(x = variablename, y = value, fill = type)) +
    geom_bar(stat = "Identity") + coord_flip() + 
    labs(x = "", y = "",
        title = "Changes in Fragility for the 58 Fragile Countries 2014 - 2017",
        subtitle = "The 58 Fragile countries became less Fragile in Political and Security Dimensions.\nThey became more fragile in Economic, Societal and Enviromental.")
p = oecd.plot(p)