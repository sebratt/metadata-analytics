# Practice netork for Abhishek's work

library(igraph)


edge.list <- read.csv("abhisheks-play-data.csv")
head(edge.list)

g <- graph_from_data_frame(edge.list, directed=FALSE)
g


plot(g, edge.arrow.size=0, edge.arrow.width=0)

V(g)$color <- "cadetblue"
V(g)$degree <- degree(g)
V(g)$close <- closeness(g)
V(g)$bet <- betweenness(g)
V(g)$size <- 1 + (14 * degree(g)/max(degree(g)))
#V(g)$label <- ""
V(g)$frame.color <- NA

plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

l <- layout_in_circle(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]

plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)


my.bet <- 1 + round(9 * V(g)$bet/max(V(g)$bet), 0)
my.pallet <- colorRampPalette(c("yellow", "red"))(10)
V(g)$color <- my.pallet[my.bet]
  
l <- layout_with_kk(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

l <- layout.fruchterman.reingold(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)

E(g)$curved <- .5
E(g)$color <- "orange2"
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)


# pick some random person in the network
random.person <- sample(V(g)$name, 1)
E(g)[.to(random.person)]$color <- "red"
E(g)$width <- 1
E(g)[.to(random.person)]$width <- 4
plot.igraph(g, edge.arrow.size=0, edge.arrow.width=0)



?plot.igraph()
?igraph.plotting()
?layout_()
