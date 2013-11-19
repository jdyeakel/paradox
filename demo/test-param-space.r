library(paradox)

# parameter space to explore over:
d <- expand.grid(
  alpha = 0.5,
  beta = 1/150,
  sigma = seq(0, 0.3, 0.05),
  num_pop = seq(1, 10, 2),
  m = c(0.05, 0.1),
  q = c(0.01, 0.05, 0.1),
  cpar = c(1, 2),
  p = c(0.75, 1, 1.25),
  n = c(0.75, 1, 1.25))

# counters:
nrow_d <- nrow(d)
i <<- 0
repititions <- 40

paradox_sim_out2 <- plyr::adply(d, 1, function(x) {
  junk <- plyr::rdply(repititions, function(y) {
    temp <- with(x, paradox_pe_sim(t_end = 1000, alpha = rep(alpha,
          num_pop), beta = beta, m = m, q = q, n = n, num_pop =
        num_pop, cpar = cpar, p = p, sigma = sigma))
    data.frame(pe = temp$pe)
})
  i <<- i + 1
  message(paste0(i, " of ", nrow_d, " complete."))
  data.frame(pe = mean(junk$pe))
})

# previous code (would need to subset the data now to run this)
# col <- RColorBrewer::brewer.pal(9, "YlOrRd")
# col <- smooth_pal(col, 5) # included in package
#
# z <- as.matrix(reshape2::dcast(paradox_sim_out, num_pop ~ sigma, value.var = "pe")[,-1])
# filled.contour(seq(1, 10, length.out = nrow(z)), seq(0, 0.3,
#     length.out = ncol(z)), z, col = col, ylab = "Sigma", xlab = "N",
#   levels = seq(1, max(z), length.out = length(col)),
#   main = "Portfolio Effect", lwd = 0.1)

library(ggplot2)
j <- subset(paradox_sim_out2, m == 0.1 & cpar == 1 & p == 1)
p <- ggplot(j, aes(num_pop, sigma, z = pe)) + geom_tile(aes(fill =
    pe)) + stat_contour(colour = "white") + facet_grid(q~n)
ggsave("n-on-horizontal-and-q-on-vertical.pdf", width = 7, height = 7)

j <- subset(paradox_sim_out2, m == 0.1 & n == 1 & p == 1)
p <- ggplot(j, aes(num_pop, sigma, z = pe)) + geom_tile(aes(fill =
    pe)) + stat_contour(colour = "white") + facet_grid(cpar~q)
ggsave("q-on-horizontal-and-cpar-on-vertical.pdf", width = 7, height = 5)

j <- subset(paradox_sim_out2, n == 1 & p == 1 & cpar == 1)
p <- ggplot(j, aes(num_pop, sigma, z = pe)) + geom_tile(aes(fill =
    pe)) + stat_contour(colour = "white") + facet_grid(m~q)
ggsave("q-on-horizontal-and-m-on-vertical.pdf", width = 7, height = 5)

j <- subset(paradox_sim_out2, n == 1 & m == 0.1 & cpar == 1)
p <- ggplot(j, aes(num_pop, sigma, z = pe)) + geom_tile(aes(fill =
    pe)) + stat_contour(colour = "white") + facet_grid(p~q)
ggsave("q-on-horizontal-and-p-on-vertical.pdf", width = 7, height = 7)
