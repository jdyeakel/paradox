library(Rcpp)
sourceCpp("sim_biomass_effort.cpp")

library(microbenchmark)
microbenchmark(
testC <- sim_biomass_effort(t_end = 1000, alpha = rnorm(20, 0.5, 0),
  beta = 1/150, m = 0.01, q = 0.01, n = 3, num_pop = 20, cpar = 1, p =
  0.5, sigma = 0.05),
testR <- sim_biomass_effortR(t_end = 1000, alpha = rnorm(20, 0.5, 0),
  beta = 1/150, m = 0.01, q = 0.01, n = 3, num_pop = 20, cpar = 1, p =
  0.5, sigma = 0.05), times = 100L)

par(mfrow = c(2, 1), mar = c(3, 4, 1, 1))
matplot(t(testC[-21,-c(1:100)]), type = "l", col = "#00000070", lty = 1)
matplot(t(testR[-21,-c(1:100)]), type = "l", col = "#00000070", lty = 1)
