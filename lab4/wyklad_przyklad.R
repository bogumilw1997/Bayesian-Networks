library (bnlearn)
library (magrittr)

rm(list = ls())
phi.X <- array(c(0.1, 0.4, 0.9, 0.6), dim  = c(2,2),
               dimnames = list(c("b1", "b2"), c("a1", "a2")))

phi.Y <- array(c(0.11, 0.23, 0.14, 0.07, 0.06, 0.18, 0.09, 0.12), dim = c(2,2,2), 
               dimnames = list(c("b1", "b2"), c("d1","d2"), c("c1", "c2")))
phi.X
phi.Y


as.vector(phi.Y)
rep(phi.X, each =2)
?rep
rep(t(phi.X), times =2) * as.vector(phi.Y)%>% 
  array(dim = c(2,2,2,2), dimnames = list(c("b1", "b2"), c("a1","a2"), c("d1","d2"), c("c1", "c2"))) -> phi.XY

phi.XY
rep(phi.Y, times =2)
rep(phi.X, each =4)

get.pot <- function(phi.X, phi.Y) {
  
  
  output.vec -> c()
  
  dimnames(phi.Y)[2:3]
  
}

dimnames(phi.Y)



as.vector(phi.X) * rep(phi.Y[,"d1","c1"], times =2)

rep(phi.Y[,"d1","c1"], times =2)
as.vector(phi.X)
