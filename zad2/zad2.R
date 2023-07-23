rm(list = ls())
library(bnlearn)
library(Rgraphviz)

data(asia)
?asia
?table

counts <- sapply(asia, table)
?barplot
barplot(counts, ylab = "counts", legend = TRUE)


labels <- c ("D", "T", "L", "B", 
             "A", "S", "X", "E")

M <- matrix (c("A","T",
               "T","E",
               "L","E",
               "E","D",
               "S","L",
               "S","B",
               "E","X",
               "B","D"),
             
             ncol = 2,
             byrow = TRUE)
M
g <- empty.graph (labels)
arcs (g) <- M

print("Rodzice:")

sapply(labels,parents, x=g)

print("Dzieci:")

sapply(labels,children, x=g)

print("Sąsiedzi:")

sapply(labels,nbr, x=g)

g.plot <- graphviz.plot(g, highlight=list(arcs=vstructs(g, arcs = TRUE), col="red", lwd = 2), render = FALSE)
nodeRenderInfo(g.plot) <- list(fill=c(A="lightblue",
                                      T="lightgreen",
                                      S="lightblue",
                                      L="lightgreen",
                                      B="lightgreen",
                                      D="lightsalmon",
                                      E="lightsalmon",
                                      X="lightsalmon"))
renderGraph(g.plot)
