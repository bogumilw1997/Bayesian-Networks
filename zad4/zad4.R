rm(list = ls())

# H0: Y = k
# H0: Y = m

total_people <- 160 + 220

P_Y <- c(160/total_people, 220/total_people)
P_YwX <- matrix(c(0.15, 0.85, 0.1, 0.9), nrow = 2)

prior_ratio <- P_Y[1]/P_Y[2]
posterior_ratio <- P_YwX[1,1]/P_YwX[1,2]
B <- posterior_ratio/prior_ratio
B
