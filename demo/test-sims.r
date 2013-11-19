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



#########



library(paradox)

d <- expand.grid(sigma = seq(0, 0.3, 0.02), num_pop = seq(1, 10, 1))
paradox_sim_out <- plyr::alply(d, 1, function(x) {
  junk <- plyr::rlply(200, paradox_sim(t_end = 1000, alpha = rep(0.5,
        x$num_pop), beta = 1/150, m = 0.1, q = 0.01, n = 1, num_pop =
      x$num_pop, cpar = 2, p = 1, sigma = x$sigma))
  message(paste0("num_pop = ", x$num_pop, ", sigma = ", x$sigma))
  return(junk)
})




 paradox_pe_sim(t_end = 1000, alpha = rep(0.5, 5), beta =
   1/150, m = 0.1, q = 0.01, n = 1, num_pop = 5, cpar = 2, p =
   1, sigma = 0.01)

pe_out <- plyr::ldply(paradox_sim_out, function(x) {
  temp <- plyr::ldply(x, function(y) {
    num_pop <- nrow(y) - 1
    effort_row <- nrow(y)
    burn <- 1:500
    t_end <- ncol(y) - max(burn)
    if(num_pop == 1) {
      junk <- get_pe(t(as.matrix(y[-effort_row,-burn])), num_pop, t_end)
    } else {
      junk <- get_pe(y[-effort_row,-burn], num_pop, t_end)
    }
    data.frame(PE = junk$pe, sync = junk$sync)
})
  #message("...")
  data.frame(PE = mean(temp$PE), sync = mean(temp$PE))
})

col <- RColorBrewer::brewer.pal(9, "YlOrRd")
col <- smooth_pal(col, 5)

z <- as.matrix(reshape2::dcast(pe_out, num_pop ~ sigma, value.var = "PE")[,-1])
# image(seq(1,10,length.out=nrow(z)),seq(0,0.3,length.out=ncol(z)), z =
# z, col = col, breaks = seq(1-0.0001, max(z)+ 0.00001, length.out =
  # length(col) + 1))
filled.contour(seq(1, 10, length.out = nrow(z)), seq(0, 0.3,
    length.out = ncol(z)), z, col = col, ylab = "Sigma", xlab = "N",
  levels = seq(1, max(z), length.out = length(col)),
  main = "Portfolio Effect", lwd = 0.1)
