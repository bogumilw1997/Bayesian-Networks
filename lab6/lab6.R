rm(list = ls())

library(bnlearn)
library(Rgraphviz)

labels <- c ("Burglar", "Alarm", "Watson", "Earthquake", "News")
# Stworzenie pustego grafu
g <- empty.graph (labels)

# Dodanie pojedynczo linków
g <- set.arc (g, "Burglar", "Alarm")
g <- set.arc (g, "Alarm", "Watson")
g <- set.arc (g, "Earthquake", "News")
g <- set.arc (g, "Earthquake", "Alarm")
graphviz.plot(g)

# Stworzenie tablic z prawdopodobieństwami brzegowymi (dla B i E) oraz warunkowymi
yes.no <- c ("yes","no")
B <- array (dimnames = list (Burglar = yes.no), dim = 2, c(0.30,0.70))
E <- array (dimnames = list (Earthquake = yes.no), dim = 2, c(0.35,0.65))
A <- array (dimnames = list (Alarm = yes.no, Earthquake = yes.no, Burglar = yes.no),
            dim = c(2, 2, 2),
            c(0.95,0.05,0.90,0.10,0.60,0.40,0.01,0.99))
W <- array (dimnames = list (Watson = yes.no, Alarm = yes.no),
            dim = c(2, 2),
            c(0.80,0.20,0.40,0.60))
N <- array (dimnames = list (News = yes.no, Earthquake = yes.no),
            dim = c(2, 2),
            c(0.95,0.05,0.05,0.95))

A
N
g.bn <- custom.fit(g, list(Burglar=B, Earthquake=E, Alarm=A, Watson=W, News=N))
g.bn

B.no <- g.bn$Burglar$prob["no"]
E.no <- g.bn$Earthquake$prob["no"]
A.no.no.no <- g.bn$Alarm$prob["no","no","no"]
N.no.no <- g.bn$News$prob["no","no"]
W.no.no <- g.bn$Watson$prob["no","no"]
B.no*E.no*A.no.no.no*N.no.no*W.no.no

library(gRain)
g.grain <- as.grain (g.bn)
g.grain
g.grain$cptlist

querygrain (g.grain, "Earthquake")
querygrain (g.grain, "Alarm") 
querygrain (g.grain, "Watson")

g.grain$evidence

g.grain <- setEvidence(object=g.grain, nodes=labels, states=rep("no", 5))
pEvidence (g.grain)
querygrain (g.grain, "Watson")

g.grain <- retractEvidence (g.grain)
g.grain2 <- setEvidence (g.grain, labels[c(1,2,4,5)], rep("no", 4))
pEvidence (g.grain2)

querygrain (g.grain2, "Watson")

g.grain <- retractEvidence (g.grain)
g.grain3 <- setEvidence (g.grain, labels[c(1,4,5)], c("yes", "no", "no"))
querygrain (g.grain3, "Watson")

g.grain <- retractEvidence (g.grain)
# a) telefon od Watsona
g.grain <- setEvidence (g.grain, "Watson", "yes")
querygrain (g.grain, "Burglar")
g.grain$cptlist$Burglar
querygrain (g.grain, "Earthquake")
g.grain$cptlist$Earthquake

# b) telefon od Watsona + informacja radiowa o trzęsieniu ziemi
g.grain <- setEvidence (g.grain, "News", "yes")
g.grain$evidence$nodes
querygrain (g.grain, "Burglar")
querygrain (g.grain, "Earthquake")

load("asia.rda")
asia.grain <- as.grain (asia.bn.fit)

querygrain (asia.grain, node="tub")
system.time (replicate (1e3, 
                        querygrain (asia.grain, node="tub")))

querygrain (setEvidence (object = asia.grain, nodes="asia", states = "no"), node="tub")
system.time (replicate (1e3, 
                        querygrain (setEvidence (asia.grain, "asia", "no"), 
                                    node="tub")))

querygrain (setEvidence (asia.grain, c("asia","dysp","xray"), rep("yes",3)), node="tub")
system.time (replicate(1e3,
                       querygrain (setEvidence (asia.grain, c("asia","dysp","xray"), rep("yes",3)),
                                   node="tub")))

asia.grain <- retractEvidence(asia.grain)
asia.grain <- propagate (asia.grain)
querygrain (asia.grain, node="tub")
system.time (replicate(1e3, querygrain(asia.grain, node="tub")))

asia.grain <- setEvidence (asia.grain, "asia", "no", propagate = TRUE)
querygrain (asia.grain, node="tub")
system.time (replicate(1e3, querygrain(asia.grain, node="tub")))


set.seed(0)

asia.samples <- cpdist (asia.bn.fit,
                        nodes = nodes(asia.bn.fit),
                        evidence = TRUE,
                        n = 15)
summary (asia.samples)
asia.samples

t <- table (asia.samples[, "dysp"])
prop.table (t)

t <- table (asia.samples[, c("smoke", "dysp")])
prop.table (t)

set.seed(0)
asia.s0d1 <- cpquery (asia.bn.fit,
                      event = (smoke == "no" & dysp == "yes"),
                      evidence = TRUE,
                      n = 15)
asia.s0d1

asia.grain <- retractEvidence (asia.grain)
q <- querygrain (asia.grain, nodes = c("smoke", "dysp"), type = "joint")
q
abs(q["no","yes"]-asia.s0d1)

asia.samples <- cpdist (asia.bn.fit, 
                        nodes = nodes(asia.bn.fit), 
                        evidence = (asia=="yes"), 
                        n = 5000)
nrow (asia.samples)

set.seed(0)
asia.samples <- cpdist (asia.bn.fit, 
                        nodes = nodes(asia.bn.fit), 
                        evidence = list (asia="yes"), 
                        n = 5000, 
                        method = "lw")
summary(asia.samples)

w <- attr(asia.samples, "weights")
table(w)

asia.samples <- cpdist (asia.bn.fit,
                        nodes = nodes(asia.bn.fit),
                        evidence=list(lung = "yes"),
                        n=5000,
                        method = "lw")
summary(asia.samples)
w <- attr(asia.samples, "weights")
table(w)

asia.samples <- cbind(asia.samples, w)
head(asia.samples, n=10)

asia.bn.fit$lung

t <- xtabs(w ~ smoke + dysp, data=asia.samples)
prop.table(t)

asia.s0d1 <- cpquery (asia.bn.fit,
                      event = (smoke == "no" & dysp == "yes"),
                      evidence = list(lung = "yes"),
                      n = 5000,
                      method = "lw")
asia.s0d1
