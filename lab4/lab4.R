library (bnlearn)
library (magrittr)

# Wczytanie danych
data (asia)

# Podgląd
head (asia)

phi_A <- table (asia$A)
phi_A

P_A <- phi_A/sum(phi_A)
P_A

phi_S <- table (asia$S)
phi_S

P_S <- phi_S/sum(phi_S)
P_S

phi_AS <- table (asia$A, asia$S)
phi_AS

P_AS <- phi_AS/sum(phi_AS)
P_AS

# P(A=no lub S=yes) = P(A=no) + P(S=yes) - P(A=no i S=yes)
P_A["no"] + P_S["yes"] - P_AS["no","yes"]

length (which(asia$A=="no" | asia$S=="yes"))/nrow(asia)

P_A["yes"] + P_S["no"] - P_AS["yes","no"]

length (which (asia$A=="yes" | asia$S=="no"))/nrow(asia)

# P(A=no | S=yes) = P(A=no i S=yes)/P(S=yes)
P_AS["no","yes"]/P_S["yes"]

asia_sub <- asia[which(asia$S=="yes"),c("A","S")]
length (which (asia_sub$A=="no"))/nrow(asia_sub)


# P(S=yes | A=no) = P(A=no i S=yes)/P(A=no)
P_AS["no","yes"]/P_A["no"]

asia_sub <- asia[which(asia$A=="no"),c("A","S")]
length (which (asia_sub$S=="yes"))/nrow(asia_sub)

# P(S=no) = P(A=yes i S=no) + P(A=no i S=no)
P_AS["yes","no"]+P_AS["no","no"]

# P(S=yes) = P(A=yes i S=yes) + P(A=no i S=yes)
P_AS["yes","yes"]+P_AS["no","yes"]

P_S

colSums(P_AS)
apply(P_AS, 2, sum)

phi_all <- sapply (1:dim(asia)[2], function(i) asia[,i] %>% table)
colnames (phi_all) <- colnames (asia)
sapply(asia,table)

phi_AS <- asia %$% table (A,S)
phi_AST <- asia %$% table (A,S,T)
phi_ASTL <- asia %$% table (A,S,T,L)

# Projekcja (marginalizacja) phi(A,S,T) względem zmiennej T
phi_AST[,,"yes"] + phi_AST[,,"no"] # = phi_AS
phi_AS

# Projekcja (marginalizacja) phi(A,S,T,L) względem zmiennej L
phi_ASTL[,,,"yes"] + phi_ASTL[,,,"no"] # = phi_AST

phi_ASTL[,,"yes","yes"] + phi_ASTL[,,"yes","no"] + phi_ASTL[,,"no","yes"] + phi_ASTL[,,"no","no"] # = phi_AS

# Projekcja (marginalizacja) phi(A,S,T) względem zmiennej T
apply (phi_AST, c("A","S"), sum)

# Projekcja (marginalizacja) phi(A,S,T,L) względem zmiennej L
apply (phi_ASTL, c("A","S","T"), sum)

# Projekcja (marginalizacja) phi(A,S,T,L) względem zmiennych L i T
apply (phi_ASTL, c("A","S"), sum)

phi_tot <- table (asia)
dim(phi_tot)

apply (phi_tot, c("T","E"), sum)
apply (phi_tot, c("L","B"), sum)

phi_AS/rep (phi_S, each=2)

P_AS/rep (P_S, each=2)

phi_X <- apply (phi_tot, c("A","S"), sum)
phi_Y <- apply (phi_tot, c("S","T","L"), sum)
# Tablica wynikowa phi_XY będzie miała 4 wymiary, ponieważ będzie rozpięta na dom(X,Y)={A,S,T,L}

phi_XY <- array(0, dim = c(2,2,2,2), dimnames = list(c("no","yes"),c("no","yes"),c("no","yes"),c("no","yes")))
names (dimnames (phi_XY)) <- c("A","S","T","L")
phi_XY[,,"no","no"] <- phi_X*phi_Y[,"no","no"]
phi_XY[,,"no","yes"] <- phi_X*phi_Y[,"no","yes"]
phi_XY[,,"yes","no"] <- phi_X*phi_Y[,"yes","no"]
phi_XY[,,"yes","yes"] <- phi_X*phi_Y[,"yes","yes"]
phi_XY

phi_XY2 <- array(0, dim = c(2,2,2,2), dimnames = list(c("no","yes"),c("no","yes"),c("no","yes"),c("no","yes")))
names (dimnames (phi_XY2)) <- c("A","S","T","L")
phi_XY2[,,"no","no"] <- phi_X/phi_Y[,"no","no"]
phi_XY2[,,"no","yes"] <- phi_X/phi_Y[,"no","yes"]
phi_XY2[,,"yes","no"] <- phi_X/phi_Y[,"yes","no"]
phi_XY2[,,"yes","yes"] <- phi_X/phi_Y[,"yes","yes"]
phi_XY2
