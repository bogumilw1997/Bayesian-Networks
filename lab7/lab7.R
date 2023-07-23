rm(list = ls())


library (bnlearn)

data(learning.test)
data(gaussian.test)

learn.net <- empty.graph(names(learning.test))
modelstring (learn.net) <- "[A][C][F][B|A][D|A:C][E|B:F]"
learn.net

summary(learning.test)
plot(learn.net)

gauss.net <- empty.graph(names(gaussian.test))
modelstring(gauss.net) <- "[A][B][E][G][C|A:B][D|B][F|A:D:E:G]"
gauss.net

summary(gaussian.test)
plot(gauss.net)

score (learn.net, learning.test)
score (gauss.net, gaussian.test)

score (learn.net, learning.test, type = "aic")
score (learn.net, learning.test, type = "bde")
score (learn.net, learning.test, type = "bde", iss = 1)
score (learn.net, learning.test, type = "bde", iss = 3)

eq.net <- set.arc (gauss.net, "D", "B")
plot(eq.net)
score (gauss.net, gaussian.test, type = "bic-g")
score (eq.net, gaussian.test, type = "bic-g")

all.equal (cpdag (gauss.net), cpdag (eq.net))

eq.net <- set.arc (gauss.net, "F", "D")
score (eq.net, gaussian.test, type = "bic-g")

?cpdag

data (lizards)
summary(lizards)

ci.test ("Height", "Diameter", "Species", data = lizards, test = "mi")
ci.test ("Height", "Diameter", data = lizards)

ci.test ("Height", "Diameter", "Species", data = lizards, test = "sp-mi")
ci.test ("Diameter", "Height", "Species", data = lizards, test = "mc-x2")

ci.test ("A", "C", "B", data = gaussian.test, test = "cor")
ci.test ("A", "B", data = gaussian.test, test = "cor")
ci.test ("A", "G", data = gaussian.test, test = "cor")
ci.test ("A", "B", "C", data = gaussian.test, test = "cor")

plot (inter.iamb (learning.test))
plot(learn.net)
plot (inter.iamb (learning.test, blacklist = c("A", "B")))

bl <- matrix (c("A", "B", "B", "A"), ncol = 2, byrow = TRUE)
plot (inter.iamb (learning.test, blacklist = bl))

pdag <- iamb (learning.test)
dag <- set.arc(pdag, from = "B", to = "A")
plot (dag)

dag <- pdag2dag (pdag, ordering = c("A", "B", "C", "D", "E", "F"))
plot (dag)

dag <- cextend(pdag)
plot (dag)

plot (hc (learning.test, blacklist = c("A", "B")))
plot (hc (learning.test, blacklist = bl))
plot (hc (learning.test, whitelist = c("A", "F")))
plot (tabu (learning.test, blacklist = c("A", "B")))

pdag <- iamb (learning.test)
dag <- pdag2dag (pdag, ordering = c("A", "B", "C", "D", "E", "F"))

# Maximum likelihood estimator
learning.fit <- bn.fit (dag, learning.test, method = "mle")

# Bayesian posterior estimator
learning.fit <- bn.fit (dag, learning.test, method = "bayes")

# Hierarchical Dirichlet posterior estimator
learning.fit <- bn.fit (dag, learning.test, method = "hdir")

bn.fit.barchart (learning.fit$D)


dag <- hc (gaussian.test)
pdag <- iamb (gaussian.test)
plot(pdag)
# Maximum likelihood estimator for least squares regression
gaussian.fit <- bn.fit (dag, gaussian.test, method = "mle-g")
gaussian.fit
