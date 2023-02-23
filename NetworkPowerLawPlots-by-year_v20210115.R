# genbank networks

# Power law alpha value, fit, and x-min. 
# Then we can calculate and visualize the change in power law alpha per year. 

# Do this: shape param of powerlaw for whole network and for sub and pub individually
# Assortativity 


library(igraph)
library(scales)
pub.dat.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/publication/"
sub.dat.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/datasubmissions/"
plot.dir <- "C:/Users/jjhemsle/Dropbox/GenBank datasets/Analysis result files/NetworkPlots-sub.pub/"
years <- 1992:2018

# debug years <- 2018:2021

# how big do you want the pngs?
plot.width <- plot.height <- 2000

pub.dat.files <- paste(pub.dat.dir, "pub-network-graph", years, ".rda", sep = "")
sub.dat.files <- paste(sub.dat.dir, "sub-network-graph", years, ".rda", sep = "")

network.ratio.df <- data.frame(year = years, pubs = rep(0, 27), subs = rep(0, 27), both = rep(0, 27))


options(scipen = 999)
plot.fname <- paste0(plot.dir, "NetworkPowerlawPlots_all_years-sorted-log-log.png")
png(plot.fname, width = 7.5, height = 8.5, units = "in", res = 300)
par(mfrow = c(6,5), mar = c(2,2,1,.5))
for (i in 1:length(years)) {
    # i <- 10
    print(paste0(" . . . STARTING ", years[i]))
    fname <- pub.dat.files[i]
    print(fname)
    load(fname)
    pub.g <- g
    V(pub.g)$type <- "pub"
    #V(pub.g)$color <- "green" # setting colors just as a way to verify
    rm(g)
    
    fname <- sub.dat.files[i]
    print(fname)
    load(fname)
    sub.g <- g
    V(sub.g)$type <- "sub"
    #V(sub.g)$color <- "red"
    rm(g)

    print(paste0(years[i], ":: Pub nodes: ", vcount(pub.g), ", sub nodes: ", vcount(sub.g)))
    
    g <- pub.g %u% sub.g
    g
    rm(pub.g, sub.g)
    
    # need a way to know who is in which network. Some are in both. No one is in neither. :-)
    pub.v.index <- which(V(g)$type_1 == "pub")
    sub.v.index <- which(V(g)$type_2 == "sub")
    
    in.both.v.index <- intersect(pub.v.index, sub.v.index)
    print(paste0(years[i], ":: nodes in both networks: ", length(in.both.v.index)))
    
    V(g)$color <- "cornflowerblue"
    V(g)$color[sub.v.index] <- "brown1"
    V(g)$color[in.both.v.index] <- "darkorchid"
    
    V(g)$degree <- degree(g)
    quantile(V(g)$degree)
    percentile <- ecdf(V(g)$degree)
    tmp.alpha <- percentile(V(g)$degree)
    tmp.alpha <- .499 + tmp.alpha/4
    #summary(tmp.alpha)
    #V(g)$color <- alpha(V(g)$color, .4)
    V(g)$color <- alpha(V(g)$color, tmp.alpha)
    
    
    o1 <- order(V(g)$name) # just kind of randomizing the order
    temp.df <- data.frame(deg = V(g)$degree[o1], col = V(g)$color[o1])
    o <- order(temp.df$deg, decreasing = T)
    temp.df <- temp.df[o, ]
    x <- 1:length(temp.df$deg)
    
    #plot(x, temp.df$deg, col = temp.df$col, pch = 16, cex = 1, bty = "n", ylab = "", xlab = "")
    plot(log10(x), log10(temp.df$deg), col = temp.df$col, pch = 16, cex = 1, bty = "n", ylab = "", xlab = "")
    mtext(text = years[i], side = 3, line = -3, cex = 1.5)
    
    #tmp.table <- table(temp.df$col)
    
    #network.ratio.df$pubs[i] <- as.numeric(tmp.table[1])
    #network.ratio.df$subs[i] <- as.numeric(tmp.table[2])
    #network.ratio.df$both[i] <- as.numeric(tmp.table[3])
    
    #pie(tmp.table, col = names(tmp.table))
    #mtext(text = years[i], side = 3, line = -0.5, adj = 0, cex = 1.8)
}    
dev.off()

par(mfrow = c(1,1))

library(ggplot2)
library(tidyverse)
library(dplyr)

colnames(network.ratio.df)
head(network.ratio.df)
View(network.ratio.df)


networks.plot.df <- network.ratio.df %>% gather(key = network, nodes, pubs:both)
zap2018 <- which(networks.plot.df$year == 2018)

colnames(networks.plot.df)
ggplot(networks.plot.df) + aes(x = year, y = nodes, color = network) +
  geom_line(size = 2) + ylim(0,60000) +
  scale_color_manual(values=c("#9932CC", "#6495ED", "#FF4040")) +
  theme_minimal() +
  ggtitle("Number of nodes in each network")


write_csv(networks.plot.df, paste0(plot.dir, "net.dat.csv"))





