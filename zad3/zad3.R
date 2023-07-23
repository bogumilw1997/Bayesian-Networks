rm(list = ls())

library (bnlearn)

phi.X <- array(c(0.1, 0.4, 0.9, 0.6), dim  = c(2,2),
               dimnames = list(c("b1", "b2"), c("a1", "a2")))

names (dimnames (phi.X)) <- c("B","A")
print("phi.X")
phi.X

phi.Y <- array(c(0.11, 0.23, 0.14, 0.07, 0.06, 0.18, 0.09, 0.12), dim = c(2,2,2), 
               dimnames = list(c("b1", "b2"), c("d1","d2"), c("c1", "c2")))

names (dimnames (phi.Y)) <- c("B","D", "C")
print("phi.Y")
phi.Y

phi.B <- apply (phi.Y, 1, sum)
print("phi.B")
phi.B

P.B <- phi.B/sum(phi.B)
print("P")
P.B

psi <- array(0, dim = c(2,2,2,2), dimnames = list(c("b1","b2"),c("a1","a2"),c("d1","d2"),c("c1","c2")))
names (dimnames (psi)) <- c("B","A","D","C")

psi[,,"d1","c1"] <- phi.X*phi.Y[,"d1","c1"]
psi[,,"d2","c1"] <- phi.X*phi.Y[,"d2","c1"]

psi[,,"d1","c2"] <- phi.X*phi.Y[,"d1","c2"]
psi[,,"d2","c2"] <- phi.X*phi.Y[,"d2","c2"]

print("psi")
psi

psi.AB <- apply (psi, c("B", "A"), sum)
P.AB <- psi.AB/sum(psi.AB)

print("P(A,B)")
P.AB

P.A_B <- psi.AB/phi.B 

print("P(A|B)")
P.A_B

print("phi.X")
phi.X

print("P(A|B) - phi.X")
round(P.A_B - phi.X, 5)
