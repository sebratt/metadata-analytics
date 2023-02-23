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

# how big do you want the pngs?
plot.width <- plot.height <- 2000

pub.dat.files <- paste(pub.dat.dir, "pub-network-graph", years, ".rda", sep = "")
sub.dat.files <- paste(sub.dat.dir, "sub-network-graph", years, ".rda", sep = "")

for (i in 1:length(years)) {
    # i <- 10
    print(paste0(" . . . STARTING ", years[i]))
    fname <- pub.dat.files[i]
    print(fname)
    load(fname)
    pub.g <- g
    E(pub.g)$color <- "cornflowerblue"
    V(pub.g)$color <- "green"
    rm(g)
    
    fname <- sub.dat.files[i]
    print(fname)
    load(fname)
    sub.g <- g
    E(sub.g)$color <- "brown1"
    V(sub.g)$color <- "red"
    rm(g)
    
    if (FALSE) {
      plot.igraph(sub.g, edge.arrow.size=0, edge.arrow.width=0, vertex.size = 1, vertex.label = "")
      
      keep.index <- union(sample(1:vcount(pub.g), size = 4000), which(V(pub.g)$name %in% V(sub.g)$name))
      drop.names <- V(pub.g)$name[-keep.index]
      
      pub.g <- delete_vertices(pub.g, drop.names)
      
      plot.igraph(pub.g, edge.arrow.size=0, edge.arrow.width=0, vertex.size = 1, vertex.label = "")
    }

    print(paste0(years[i], ":: Pub nodes: ", vcount(pub.g), ", sub nodes: ", vcount(sub.g)))
    
    g <- sub.g %u% pub.g
    g
    rm(pub.g, sub.g)
    
    # need a way to know who is in which network. Some are in both. No one is in neither. :-)
    pub.v.index <- which(!is.na(V(g)$color_2))
    pub.e.index <- which(!is.na(E(g)$color_2))
    sub.v.index <- which(!is.na(V(g)$color_1))
    sub.e.index <- which(!is.na(E(g)$color_1))
    
    in.both.v.index <- intersect(pub.v.index, sub.v.index)
    in.both.e.index <- intersect(pub.e.index, sub.e.index)
    print(paste0(years[i], ":: nodes in both networks: ", length(in.both.v.index)))
    
    V(g)$frame.color <- NA
    V(g)$label <- NA
    E(g)$curved <- .5
    
    V(g)$color <- "cornflowerblue"
    E(g)$color <- "cyan2"
    
    V(g)$color[sub.v.index] <- "brown3"
    E(g)$color[sub.e.index] <- "orangered"
    
    V(g)$color[in.both.v.index] <- "darkorchid"
    E(g)$color[in.both.e.index] <- "deeppink"
    
    V(g)$color <- alpha(V(g)$color, 1)
    E(g)$color <- alpha(E(g)$color, 1)
    E(g)$width <- .3
    
    
    #size <- 1 + log10(degree(g))
    #plot(size)
    #V(g)$size <- size
    V(g)$size <- 1
    V(g)$size <- rescale(degree(g), c(1,5))
    
    g
    
    V(g)$comp <- components(g)$membership
    sort(table(V(g)$comp), decreasing = T)[1:10]
    # g <- induced_subgraph(g, V(g)[components(g)$membership == which(components(g)$csize == 57)])
    #g <- induced_subgraph(g, V(g)[components(g)$membership == which.max(components(g)$csize)])
    
    sort(components(g)$csize, decreasing = TRUE)[1:5]
    
    g <- induced_subgraph(g, V(g)[components(g)$membership %in% which(components(g)$csize > 9)])
    
    
    #l <- layout_with_drl(g, options = drl_defaults$final)
    #igraph.drl.experiment <- drl_defaults$default
    #igraph.drl.experiment <- drl_defaults$coarsen # nodes crunched, links spread
    #igraph.drl.experiment <- drl_defaults$coarsest # nodes crunched, links spread
    #igraph.drl.experiment <- drl_defaults$final # nodes slightly spread, links tight
    igraph.drl.experiment <- drl_defaults$refine # nodes spread, links spread <- best?
    
    l <- layout_with_drl(g, options = igraph.drl.experiment, weights = E(g)$weight) # ! a good one!
    
    plot.igraph(g, layout = l, edge.arrow.size=0, edge.arrow.width=0)
    mtext(text = "refine")
    
    
    # to make refine a little more spreadout
    igraph.drl.experiment$edge.cut <- .9
    igraph.drl.experiment$expansion.iterations <- 50 # 50
      igraph.drl.experiment$expansion.temperature <- 400
      igraph.drl.experiment$expansion.attraction <- .09
      igraph.drl.experiment$expansion.damping.mult <- .2
}


df.settings <- cbind(t(as.data.frame(drl_defaults$refine)),
                t(as.data.frame(drl_defaults$final)),
                t(as.data.frame(drl_defaults$default)),
                t(as.data.frame(drl_defaults$coarsen)),
                t(as.data.frame(drl_defaults$coarsest)))
colnames(df.settings) <- c("refine", "final", "default", "coarsen", "coarsest")
View(df.settings)
