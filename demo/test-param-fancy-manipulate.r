
d <- data.frame(expand.grid(alpha = 0.5, beta = 1/150, sigma = seq(0, 0.3, 0.05), num_pop = seq(1, 20), m = 0.1, q = 0.01, cpar = 2, p = 1, n = 1))

repititions <- 80

out <- plyr::adply(d, 1, function(x) {
  junk <- plyr::rdply(repititions, function(y) {
    temp <- with(x, paradox_pe_sim(t_end = 1000, alpha = rep(alpha,
          num_pop), beta = beta, m = m, q = q, n = n, num_pop =
        num_pop, cpar = cpar, p = p, sigma = sigma))
    data.frame(pe = temp$pe, vuln = temp$vuln)
})
  data.frame(pe = mean(junk$pe), vuln = mean(junk$vuln))
})

col <- RColorBrewer::brewer.pal(9, "YlOrRd")
col <- smooth_pal(col, 5)
z <- as.matrix(reshape2::dcast(out, num_pop ~ sigma, value.var = "pe")[,-1])

levels <- seq(1, max(z), length.out = length(col))

plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
.filled.contour(seq(1, 20, length.out = nrow(z)), seq(0, 0.3,
    length.out = ncol(z)), z, levels = levels, col = col)

par(mfrow = c(1, 2))
filled_contour(seq(1, 20, length.out = nrow(z)), seq(0, 0.3,
    length.out = ncol(z)), z, levels = levels, col = col)




