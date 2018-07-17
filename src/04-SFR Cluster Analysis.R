
source("./lib/funcs.R")


#####Parameters for this script
#Change the numbers of clusters
num.clusters = 6
#Change the numbers of clusters to be compared too
plot.threshold = 4
#Change significance level
signif = 0.95
#Run script

factr = function (x, y = unique(x)) 
{
    factor(x, levels = y, ordered = T)
}
try(dir.create("./graphs/cluster-analysis"))

for (the.pca in names(all.pca)[1:5]){
     
    pca = last(all.pca[[the.pca]])


    tmp = cluster(pca, num.clusters)
    tmp = tmp$labels
    
    
    load("./cache/sfr.time.series.RData")
    # invert indictors to be in the right direction for the tables
  
    # pos <- sfr.time.series$doesmoreincreasefragility == 1
    # sfr.time.series$value[pos] <- -sfr.time.series$value[pos]
    
    temp = left_join(sfr.time.series %>% dplyr::filter(dimension == the.pca), 
                     tmp %>% dplyr::select(iso3c, cluster))
    temp = temp %>% dplyr::filter(year == max(year))
    
     #####Simple mean plot
    
    mean.plot = temp %>% dplyr::group_by(variablename) %>%
        mutate(banded = (value - min(value))/diff(range(value))) 
    mean.plot = mean.plot %>% ungroup() %>% 
        dplyr::group_by(cluster, variablename) %>%
        dplyr::summarise(mean.banded = mean(banded))
    mean.plot = mean.plot %>% ungroup() %>% dplyr::group_by(variablename) %>%
        mutate(rank = rank(mean.banded, ties.method = "min"))
    
    mean.plot$type = "Risks" 
    pos = grepl("\\(C\\)", mean.plot$variablename)
    mean.plot$type[pos] = "Coping Capacity"
    #mean.plot$rank = factr(mean.plot$rank)
    
    
    #######
    temp = split(temp, factor(temp$variablename))

    #do an analysis of variance test and then a tukey test to 
    #identify statistically significant difference between clusters
    temp = lapply(temp, function(x){
        x = x %>% dplyr::filter(complete.cases(.))
        fit = aov(value ~ cluster, data = x)
        results = my.tukey.Test(fit)
        results = results %>% dplyr::filter(p.adj < 1 - signif)
        return(results)
    })
    
    
    library(igraph)
    #plot graph of cluster dominance per variablename
    degrees.bad = sapply(names(temp), function(i){
        df = temp[[i]]
        df$cluster1.1 = df$cluster1
        df$cluster2.1 = df$cluster2
        #if the difference is more than 0, i.e. for ppi cluster 1 scores worse than cluster 2
        pos = df$diff > 0 
        #switch the positions 
        df$cluster1[pos] = df$cluster2.1[pos]
        df$cluster2[pos] = df$cluster1.1[pos]
        g = graph_from_edgelist(as.matrix(df %>% select(cluster1, cluster2)) )
        #plot(g, main = i)
        degrees = degree(g, mode = "in", loops = TRUE, normalized = FALSE)
        degrees = data.frame(cluster = names(degrees), degree = degrees)
        degrees = degrees %>% arrange(desc(degree))
        return(degrees)
    }, simplify = F)
    
    degrees.bad = bind_rows(degrees.bad, .id = "variablename")
    degrees.bad$degree = - degrees.bad$degree
    
    degrees.good = sapply(names(temp), function(i){
        df = temp[[i]]
        df$cluster1.1 = df$cluster1
        df$cluster2.1 = df$cluster2
        #if the difference is more than 0, i.e. for ppi cluster 1 scores worse than cluster 2
        pos = df$diff < 0 
        #switch the positions 
        df$cluster1[pos] = df$cluster2.1[pos]
        df$cluster2[pos] = df$cluster1.1[pos]
        g = graph_from_edgelist(as.matrix(df %>% select(cluster1, cluster2)) )
        #plot(g, main = i)
        degrees = degree(g, mode = "in", loops = TRUE, normalized = FALSE)
        degrees = data.frame(cluster = names(degrees), degree = degrees)
        degrees = degrees %>% arrange(desc(degree))
        return(degrees)
    }, simplify = F)
    
    degrees.good = bind_rows(degrees.good, .id = "variablename")
    
    degrees = rbind(degrees.good, degrees.bad)
    degrees = degrees %>% dplyr::filter(abs(degree) >= plot.threshold)
    mean.plot = as.data.frame(mean.plot)
    mean.plot = left_join(mean.plot, degrees)
    pos = is.na(mean.plot$degree)
    mean.plot$degree[!pos] = "*"
    #write.csv(excludes2[,1], "./graphs/cluster-analysis/indicators-not-distinct.csv", row.names = F)
    myPalette <- brewer.pal(3, "Blues")
    
    
    #position of colours
    mean.plot$colour = match(mean.plot$cluster, LETTERS[])
    mean.plot$colour = cluster.colour.names[mean.plot$colour]
    mean.plot$colour = factr(mean.plot$colour, cluster.colour.names)
    mean.plot$rank = findInterval(mean.plot$rank, c(0,2.5, 7))
    mean.plot$rank[mean.plot$rank == 2] = 3
    mean.plot$rank = ifelse(is.na(mean.plot$degree), 2, mean.plot$rank)
    #mean.plot = mean.plot %>% filter(!is.na(rank))
    mean.plot$rank = factr(mean.plot$rank, 1:3)
    p = ggplot(mean.plot, aes(x = colour, y = variablename, label = degree)) +
        geom_tile(aes(fill = rank), na.rm = T, alpha = 0.8) + facet_wrap(~type, ncol = 1, scales = "free") +
        scale_fill_manual(values = rev(myPalette)[c(1,3,2)], name = "Global Comparison",
                          labels = c("Strong", "No Significance","Weak"), na.value = "grey") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
        theme(legend.position = "bottom") +
        guides(fill = guide_legend(keywidth = 1, keyheight = 1, 
                                   title.position = "top",
                                   title.hjust = 0.5,
                                   label.position = "bottom",
                                   label.hjust = 0.5,
                                   nrow = 1, reverse = T)) +
        geom_text() +
        labs(title = paste0("Cluster Comparisons for ", the.pca), x = "", y = "", 
             subtitle = paste0("* indicators show difference significant at ", 100*signif, "% confidence level"," compared to ", plot.threshold," other clusters " )) +
        theme(plot.caption = element_text(hjust = 0))
    fname = paste0("./graphs/cluster-analysis/", the.pca, "-cluster-indicators.pdf")
    ggsave(p, filename = fname, height = 10, 
           width = 10)
    fname = paste0("./graphs/cluster-analysis/", the.pca, "-cluster-indicators.png")
    ggsave(p, filename = fname, height = 10, 
           width = 10)
    
    write.csv(mean.plot, paste0("./graphs/cluster-analysis/cluster-indicators-averages-", the.pca, ".csv"), row.names = F)

    tmp$countryname<- oecd.country.name(tmp$iso3c)
    write.csv(tmp, paste0("./graphs/cluster-analysis/cluster-countries-", the.pca, ".csv"), row.names = F)
    
    # #TODO: What do the change in?
    # 
    # #get regions
    # library(WDI)
    # regions = as.data.frame(WDI::WDI_data$country) %>% dplyr::select(iso3c, region, income)
    # regions$region = gsub(" \\(all income levels\\)", "", regions$region)
    # regions$income = gsub(": nonOECD", "", regions$income)
    # regions$income = gsub(": OECD", "", regions$income)
    # regions$income = factr(regions$income, unique(regions$income)[c(4,3,2,1, 5, 6)])
    # regions = left_join(tmp, regions)
    # 
    # region = regions %>% dplyr::group_by(cluster, region) %>% dplyr::summarise(n = n()) %>%
    #     ungroup() %>% dplyr::group_by(cluster) %>% dplyr::mutate(pc = n/sum(n))
    # 
    # p = ggplot(region, aes(x = region, y = pc)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
    #     scale_y_continuous(labels = scales::percent) + facet_wrap(~cluster, ncol = 2) + 
    #     labs(y = "Percentage of Countries",
    #          title = "Regions of clusters") +
    #     theme(axis.text.x = element_text(angle = 65, hjust = 1))
    # p = oecd.plot(p)
    # ggsave(p, filename = paste("./graphs/cluster-analysis/Cluster-Regions.pdf", sep = ""), height = 10, 
    #        width = 10)
    # 
    # region = regions %>% dplyr::group_by(cluster, income) %>% dplyr::summarise(n = n()) %>%
    #     ungroup() %>% dplyr::group_by(cluster) %>% dplyr::mutate(pc = n/sum(n))
    # region$income = factr(region$income, levels(region$income)[c(4,1,2,3,6,5)])
    # 
    # p = ggplot(region, aes(x = income, y = pc)) + geom_bar(stat = "identity", fill = "cornflowerblue") +
    #     scale_y_continuous(labels = scales::percent) + facet_wrap(~cluster, ncol = 2) + 
    #     labs(y = "Percentage of Countries",
    #          title = "Income of clusters") +
    #     theme(axis.text.x = element_text(angle = 65, hjust = 1))
    # p = oecd.plot(p)
    # ggsave(p, filename = paste("./graphs/cluster-analysis/Cluster-Income.pdf", sep = ""), height = 10, 
    #        width = 10)
}
