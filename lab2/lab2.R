rm(list = ls())

library (bnlearn)

labels <- c ("A", "S", "T", "L", "B", "E", "X", "D")
g <- empty.graph (labels)
plot (g)

g <- set.arc (g, "A", "T")
g <- set.arc (g, "S", "L")
g <- set.arc (g, "S", "B")
g <- set.arc (g, "T", "E")
g <- set.arc (g, "L", "E")
g <- set.arc (g, "B", "D")
g <- set.arc (g, "E", "D")
g <- set.arc (g, "E", "X")
plot (g)
g <- set.arc (g, "E", "A")
g$nodes["B"]

s <- "[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E]"
g2 <- model2network (s)
plot (g2)
g2

M <- matrix (c("A","T",
               "S","L",
               "S","B",
               "T","E",
               "L","E",
               "B","D",
               "E","D",
               "E","X"),
             ncol = 2,
             byrow = TRUE)

g3 <- empty.graph (labels)
arcs (g3) <- M
plot (g3)
g3
arcs(g3)
A <- matrix(c(0, 0, 1, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 1, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 0, 0, 0, 0, 1, 0, 
              0, 0, 0, 0, 0, 0, 0, 0, 
              0, 0, 0, 0, 0, 0, 0, 0),
            nrow = 8,
            byrow = TRUE,
            dimnames = list(labels, labels))
print(A)
g4 <- empty.graph(labels)
amat(g4) <- A
plot(g4)
library(Rgraphviz)
graphviz.plot(g)
graphviz.plot(g, highlight = list(nodes=c("A", "S"), col="red", textCol="red"))
graphviz.plot(g, highlight = list(nodes=c("A"),
                                  arcs=c("A", "T", "T", "E"),
                                  col="red", textCol="red"))
g.plot <- graphviz.plot(g, highlight = list(nodes=c("A"),
                                            arcs=c("A", "T", "T", "E"),
                                            col="red", textCol="red"),
                        render = FALSE)

nodeRenderInfo(g.plot) <- list(fill="lightblue", fontsize=10)
renderGraph(g.plot)
nbr(g, "E")
parents(g, "E")
# Dzieci
children(g, "E")
# Węzły bez połączeń wchodzących (korzenie)
root.nodes(g)
# Węzły bez połączeń wychodzących (liście)
leaf.nodes(g)
# Krawędzie skierowane
arcs(g)
# Sprawdzenie czy istnieje ścieżka między dwoma węzłami
path.exists(g, from="A", to="X")
path.exists(g, from="X", to="A")
