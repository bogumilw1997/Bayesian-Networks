rm(list = ls())

library (bnlearn)

data(asia)

phiA <- table(asia$A)

pA <- phiA/sum(phiA)

potentials <- lapply(1:dim(asia)[2], function(i) table(asia[,i]))
names(potentials) <- colnames(asia)

probs <- lapply(potentials, function(p) p/sum(p))

pot.AB <- table(asia$A, asia$B)

pot.AgB <- matrix(0,2,2)

pot.AgB[,1] <- pot.AB[,1]/potentials$B[1]
pot.AgB[,2] <- pot.AB[,2]/potentials$B[2]

AB.no <- table(asia$A[which(asia$B=="no")])
AB.yes <- table(asia$A[which(asia$B=="yes")])
AB <- cbind(AB.no, AB.yes)
