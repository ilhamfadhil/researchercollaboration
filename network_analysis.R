library(igraph)
library(tidyverse)

path <- file.path("I:", "R", "Network Analysis", "Publication Collaboration")

df <- read_csv(paste(path, "edgelist_total.csv", sep = "/"))

network <- graph_from_data_frame(df[, 1:2], directed = FALSE)

V(network)$size <- 2
plot.igraph(network, layout = layout.fruchterman.reingold, vertex.label = NA)

png("network-basic.png",
    width  = 3.25,
    height = 3.25,
    units  = "in",
    res    = 1200,
    pointsize = 4)
plot.igraph(network, layout = layout.fruchterman.reingold, vertex.label = NA)
dev.off()

hist(degree(network), breaks = 50, 
     main = "Degree Distribution of Researcher Collaboration",
     xlab = "Degree")

deg <- degree(network)
sort(deg, decreasing = TRUE)[1:10]

close.net <- closeness(network)
sort(close.net, decreasing = TRUE)[1:10]

between.net <- betweenness(network)
sort(between.net, decreasing = TRUE)[1:10]

cores <- graph.coreness(network)
V(network)$color <- cores
plot.igraph(network, layout = layout.fruchterman.reingold,
            vertex.label = NA)

comps <- decompose.graph(network) 
table(sapply(comps, vcount))

gc <- comps[[1]]
average.path.length(gc)
diameter(gc)
transitivity(gc)