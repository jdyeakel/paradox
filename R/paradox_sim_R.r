#' Simulate biomass and effort trajectories with R
#'
#' @export

paradox_sim_R <- function(num_pop = 5, t_end = 1000, alpha =
  numeric(), beta = 1/150, n = 1, m = 0.01, q = 0.01, p =
  0.5, cpar = 1.4, sigma = 0.05) {

  Eff <- num_pop+1
  ts <- matrix(0,num_pop+1,t_end)
  ts[,1] <- c(rep(50,num_pop),10)

  stoch_m <- matrix(data = exp(rnorm(num_pop*t_end,-(sigma^2)/2,sigma)),num_pop,t_end)

  for (t in 2:t_end) {
    # Dynamics of Fish biomass
    for (i in 1:num_pop) {
      # Shepherd
      ts[i,t] <- (ts[i,t-1]*(alpha[i] * stoch_m[i,t-1]))/
      (1 + beta*ts[i,t-1]^(1/n)) + ts[i,t-1] * exp(-m-q*ts[Eff,t-1])
    }
    # Dynamics of Effort
    total_biomass <- sum(ts[1:num_pop,t-1])
    ts[Eff,t] <- ts[Eff,t-1]*exp(-cpar) + p * total_biomass *
    ((q * ts[Eff, t-1])/(m + q * ts[Eff, t-1])) * (1-exp(-m-q*ts[Eff,t-1]))
  }
  return(ts)
}
