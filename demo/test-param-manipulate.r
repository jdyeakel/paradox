library(paradox)
library(manipulate)

manipulate({
d <- expand.grid(
  alpha = alpha,
  beta = beta,
  sigma = seq(0, 0.3, 0.05),
  num_pop = seq(1, MaxN, 2),
  m = m,
  q = q,
  cpar = cpar,
  p = p,
  n = n)

repititions <- 30


paradox_sim_out <- plyr::adply(d, 1, function(x) {
  junk <- plyr::rdply(repititions, function(y) {
    temp <- with(x, paradox_pe_sim(t_end = 1000, alpha = rep(alpha,
          num_pop), beta = beta, m = m, q = q, n = n, num_pop =
        num_pop, cpar = cpar, p = p, sigma = sigma))
    data.frame(pe = temp$pe)
})
  data.frame(pe = mean(junk$pe))
})

col <- RColorBrewer::brewer.pal(9, "YlOrRd")
col <- smooth_pal(col, 5)
z <- as.matrix(reshape2::dcast(paradox_sim_out, num_pop ~ sigma, value.var = "pe")[,-1])
filled.contour(seq(1, MaxN, length.out = nrow(z)), seq(0, 0.3,
     length.out = ncol(z)), z, col = col, ylab = "Sigma", xlab = "N",
   levels = seq(1, max(z), length.out = length(col)),
   main = "Portfolio Effect", lwd = 0.1)}
, m = slider(0.01, 0.2, 0.1), 
  q = slider(0.001, 0.2, 0.01), 
  cpar = slider(0.5, 4, 2), 
  n = slider(0.4, 1.5, 1), 
  p = slider(0.5, 1.5, 1), 
  alpha = slider(0.1, 1.5, 0.5), 
  beta = slider(1/500, 1/10, 1/150),
  MaxN = slider(2,40,15))