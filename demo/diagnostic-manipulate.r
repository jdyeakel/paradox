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
maxn = slider(2,40,15),
reps = slider(5, 100, 15),
max_sigma = slider(0.1, 1.5, 0.5),
t_end = slider(100, 2000, 600)
)


