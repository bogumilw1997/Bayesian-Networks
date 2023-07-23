rm(list = ls())

library (bnlearn)
labels <- c ("Burglar", "Alarm", "Watson", "Earthquake", "News")

M <- matrix (c("Burglar","Alarm",
               "Alarm","Watson",
               "Earthquake","Alarm",
               "Earthquake","News"),
             ncol = 2,
             byrow = TRUE)
g <- empty.graph (labels)
arcs (g) <- M

library(Rgraphviz)
g.plot <- graphviz.plot(g, highlight = list(nodes=c("Earthquake"),
                                            arcs=c("Earthquake", "Alarm", "Earthquake", "News"),
                                            col="red", textCol="red"),
                        render = FALSE)
nodeRenderInfo(g.plot) <- list(fill="lightblue", fontsize=20)
renderGraph(g.plot)
