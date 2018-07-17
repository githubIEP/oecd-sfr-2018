source("./lib/funcs.R")
library(rio)

#NEED TO SELECT ONLY LAST PCA

aggr.pca <- data.frame(iso3c = all.pca$Aggregate$`2017`$iso3c, 
                       Fragility = all.pca$Aggregate$`2017`$x[,1])
aggr.pca <- aggr.pca %>% arrange((Fragility))
aggr.pca$fragility.level <- "Rest of the World"
pos <- aggr.pca$Fragility <= -1.2 #Take until Malawi
#pos <- as.numeric(rownames(aggr.pca)) < nrow(aggr.pca)/3  #Take 1/3
aggr.pca$fragility.level[pos] <- "Fragile"
pos <- aggr.pca$Fragility <= -2.5
aggr.pca$fragility.level[pos] <- "Extreme Fragility"
aggr.pca$country <- oecd.country.name(aggr.pca$iso3c, short = F)
aggr.pca  <- aggr.pca %>% dplyr::select(iso3c, fragility.level) %>% 
    dplyr::filter(fragility.level != "Rest of the World")

# Number of clusters
clusters = lapply(all.pca, function(x){
    temp = last(x)
    temp = cluster(temp, num.clusters = 6)
    temp = temp$labels %>% select(iso3c, cluster)
})
clusters = bind_rows(clusters, .id = "dimension")
aggr.pca  = left_join(aggr.pca , clusters)

# Import qualitative decisions on clusters
fragile.levels <- import("./data/additional data/dimensional fragility.xlsx")
aggr.pca  = left_join(aggr.pca , fragile.levels) %>% dplyr::filter(dimension != last(names(all.pca)))
aggr.pca $country <- oecd.country.name(aggr.pca $iso3c, short = F)
aggr.pca $country <- factor(aggr.pca $country, 
                            levels = unique(aggr.pca $country), ordered = T)

################### Step 8 ######################################### Pie Chart

aggr.pca  <- aggr.pca  %>% dplyr::select(country, dimension, fragility.level, Fragility)
angles <- -90 - 270/length(unique(aggr.pca $country)) * seq_along(aggr.pca $country)
angles <- angles%%360
pos <- angles > 90
angles[pos] <- angles[pos] - 180
aggr.pca $dimension <- factor(aggr.pca$dimension, levels = rev(c("Political", "Societal", "Economic", "Environmental", 
                                                        "Security")), ordered = T)

#COLOURS OF COUNTRY NAMES
ybreaks <- levels(aggr.pca $dimension)
aggr.pca $col <- brewer.pal(9, "Blues")[9]
aggr.pca $col[aggr.pca $fragility.level == "Fragile"] <- brewer.pal(9, "Blues")[6]
col <- aggr.pca  %>% dplyr::select(country, col) %>% distinct()
p <- ggplot(aggr.pca , aes(x = country, y = dimension)) + geom_tile(aes(fill = Fragility), alpha = 0.6, colour = grey(0.6)) # geom_bar(stat = 'identity') 
upper.lim <- ceiling(10 * nlevels(aggr.pca $country))
cols <- brewer.pal(6, "Reds")

#COLOURS OF DIMENSIONAL SNAIL
p <- p + scale_fill_gradientn(colours = colorRampPalette(brewer.pal(9, "Blues")[9:4])(5), 
                                        limits = c(1, 5), 
                                        breaks = c(1, 5), 
                                        labels = c("Extreme", "Minor"))

p <- p + scale_x_discrete(expand = c(0.163, 0))
p <- p + scale_y_discrete(expand = c(0.5, 0))

p <- p + theme(axis.text.x = element_text(angle = angles, vjust = 2, colour = col$col))
p <- oecd.plot(p)

p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.text.y = element_blank(), 
               axis.ticks.y = element_blank(), legend.direction = "horizontal")
p <- p + coord_polar(start = -0.8)
p <- p + xlab("") + ylab("")

p <- p + geom_text(data = data.frame(x = 0.5, y = ybreaks, label = ybreaks), aes(x = x, y = y, label = label), 
                   inherit.aes = F, size = 3)
p <- p + theme(legend.position = c(0.2, 0.8), legend.background = element_rect(fill = rgb(233/255, 237/255, 
                                                                                           247/255)))
fname = paste0("./graphs/", length(unique(aggr.pca $country)), "-Fragile-Situations.pdf")
ggsave(p, filename = fname, height = 8, width = 10)
