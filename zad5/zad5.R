rm(list = ls())

library(bnlearn)
library(Rgraphviz)
library(gRain)

load("asia.rda")
asia.grain <- as.grain (asia.bn.fit)
  
querygrain (setEvidence (asia.grain, c("smoke","dysp","xray", "asia"), c("no","yes", "yes", "yes")), nodes = c("tub", "lung"))
# gruzlica jest bardziej prawdopodobna

asia.grain <- retractEvidence (asia.grain)
querygrain (setEvidence (asia.grain, c("smoke","dysp"), c("yes","yes")), nodes = c("bronc", "lung"))
# zapalenie oskrzeli bardziej prawdopodobne

set.seed(627)
asia.prob <- cpquery (asia.bn.fit,
                      event = (xray == "yes" & smoke == "no"),
                      evidence = TRUE,
                      n = 100)
print(asia.prob)

absolute.error <- function(m){
  
  approx.prob <- cpquery (asia.bn.fit,
                        event = (xray == "yes" & smoke == "no"),
                        evidence = TRUE,
                        n = m)
  
  real.prob <- querygrain (asia.grain, nodes = c("smoke", "xray"), type = "joint")
  
  return (real.prob["no", "yes"] - approx.prob)
  
}

m.vec <- c(10^1, 10^2, 10^3, 10^4, 10^5, 10^6)
abs.err <- sapply(m.vec, absolute.error)

plot(m.vec, abs.err, log="x")

