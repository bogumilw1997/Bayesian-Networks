rm(list = ls())
library (bnlearn)
library(Rgraphviz)

data("marks")
head(marks, 6)
summary(marks)
nrow(marks)
# 88 wierszy

?pc.stable

dag.pc <- pc.stable(marks)

dag.pc$learning
#korelacja Pearsona, próg = 0.05

plot(dag.pc)
plot(pc.stable(marks, alpha = 0.03))
plot(pc.stable(marks, alpha = 0.07))
plot(pc.stable(marks, alpha = 0.08))

?hc
dag.hc <- hc(marks)
plot(dag.pc)
plot(dag.hc)
# Odwrocone linki z Vect do ALG, z Stat do Alg, z Stat do Anl, z Anl do Alg

dag.hc$learning
dag.hc
score(dag.hc, marks)
# 34 testy przy uczeniu, score = -1731.407

learning.fit <- bn.fit (dag.hc, marks)

cpquery (learning.fit, event = (STAT > 60 & MECH > 60),
                        evidence = (ALG>60))

cpquery (learning.fit, event = (STAT > 60),
         evidence = (ALG>60))

cpquery (learning.fit, event = (STAT > 60),
         evidence = (ALG>60 & MECH > 60))
#Wniosek: zmienna STAT jest zależna od zmiennej MECH pod warunkiem ze znamy zmienną ALG