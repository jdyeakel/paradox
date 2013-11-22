# A manipulate plot to diagnose what's going on

library(paradox)
library(manipulate)
library(doParallel)
registerDoParallel(cores = 2)

manipulate({
burnin <- round(t_end / 2)
  out <- paradox_sim_reps(m = m, q = q, cpar = cpar, n = n, p = p, alpha
    = alpha, beta = beta, num_pop = seq(1, maxn), 
    sigma = seq(0, max_sigma, length.out = 6), reps = reps, t_end =
      t_end, burnin = burnin, return_ts = FALSE, .parallel = TRUE)
paradox_diag_plot(out)
},
m = slider(0.01, 0.2, 0.1),
q = slider(0.001, 0.2, 0.01),
cpar = slider(0.5, 4, 2),
n = slider(0.4, 1.5, 1),
p = slider(0.5, 1.5, 1),
alpha = slider(0.1, 1.5, 0.5),
beta = slider(1/500, 1/10, 1/150),
maxn = slider(2,40, 20),
reps = slider(5, 100, 20),
max_sigma = slider(0.1, 1.5, 0.5),
t_end = slider(100, 2000, 700)
)

# A manipulate plot of the time series:

plot_subpops <- function(lower_sigma = 0, upper_sigma = 0.3, lower_num_pop = 2, upper_num_pop = 20, years_to_plot = 60, y_min = 0, y_max = 100, alpha = 0.5, ...){
  
  sigmas <- seq(lower_sigma, upper_sigma, length.out = 4)
  num_pops <- round(seq(lower_num_pop, upper_num_pop, length.out = 4))
  par(mfrow = c(4,4), mar = c(0,0,0,0), oma = c(3, 3, 3, 3))
  ignore <- sapply(sigmas, function(s) {
    sapply(num_pops, function(np) {
      out <- paradox_sim(alpha = rep(alpha, np), num_pop = np, return_ts = TRUE, ...)$biomass
      if(np > 1) {
        matplot(t(out[,1:years_to_plot]), type = "l", lty = 1, col = RColorBrewer::brewer.pal(6, "Dark2"), axes = FALSE, ylim = c(y_min, y_max))
      }  else {
        matplot(out[,1:years_to_plot], type = "l", lty = 1, col = RColorBrewer::brewer.pal(6, "Dark2"), axes = FALSE, ylim = c(y_min, y_max))
      }
      box(col = "lightgrey")
    })})
  mtext("Numer of populations", side = 1, outer = TRUE , line = 1)
  mtext("Process noise (sigma)", side = 2, outer = TRUE , line = 1
  
}
manipulate({
  plot_subpops(alpha = alpha, m = m, q = q, cpar = cpar, n = n,p = p, beta = beta)},
  m = slider(0.01, 0.2, 0.1),
  q = slider(0.001, 0.2, 0.01),
  cpar = slider(0.5, 4, 2),
  n = slider(0.4, 1.5, 1),
  p = slider(0.5, 1.5, 1),
  alpha = slider(0.1, 1.5, 0.5),
  beta = slider(1/500, 1/10, 1/150),
  y_max = slider(10, 600, 100),
  upper_sigma = slider(0.2, 2, 0.5)
)