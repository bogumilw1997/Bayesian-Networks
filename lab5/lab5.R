rm(list = ls())

P_E <- c (no=0.99, yes=0.01)
P_B <- c (no=0.9, yes=0.1)
P_AwBE <- array (c (0.99, 0.01, 0.1, 0.9, 0.1, 0.9, 0.01, 0.99),
                 dim = c(2,2,2),
                 dimnames = list (A = c ("no","yes"), E = c ("no","yes"), B = c ("no","yes"))
)
# Krok 1
P_ABE <- array (0,
                dim = c(2,2,2),
                dimnames = list (A = c ("no","yes"), E = c ("no","yes"), B = c ("no","yes"))
)

P_ABE[,,"no"] <- P_AwBE[,,"no"]*rep(P_E, each=2)*P_B["no"]
P_ABE[,,"yes"] <- P_AwBE[,,"yes"]*rep(P_E, each=2)*P_B["yes"]

P_ABE[] <- sapply(c("no", "yes"), function(i) P_AwBE[,,i] * rep(P_E, each=2)*P_B[i])

P_AB <- apply (P_ABE, c("A","B"), sum)
P_AB

P_A <- apply (P_AB, "A", sum)
P_A
s
# Krok 4
P_BwA <- t(P_AB)/rep(P_A, each=2)
P_BwA

# Krok 4
P_BwA <- t(P_AB)/rep(P_A, each=2)
t(P_BwA)

# Przyklad 2
P_X <- c(0.5, 0.5)
P_Y <- c(0.0425, 0.9575)
P_YwX <-matrix(c(0.005, 0.095, 0.08, 0.92), nrow = 2)

#H0: X = X[1] (kobieta)
#H1: X = X[2] (mężczyzna)
prior_ratio <- P_X[1]/P_X[2]
posterior_ratio <- P_YwX[1,1]/P_YwX[1,2]
B <- posterior_ratio/prior_ratio
B
