library(paradox)

d <- expand.grid(sigma = seq(0, 0.3, 0.01), num_pop = seq(1, 10, 1))
paradox_sim_out <- plyr::alply(d, 1, function(x) {
  junk <- plyr::rlply(50, paradox_sim(t_end = 1000, alpha = rep(0.5,
        x$num_pop), beta = 1/150, m = 0.1, q = 0.01, n = 1, num_pop =
      x$num_pop, cpar = 2, p = 1, sigma = x$sigma))
  message(paste0("num_pop = ", x$num_pop, ", sigma = ", x$sigma))
  return(junk)
})

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
source("~/Dropbox/Rpackages/dataink/R/smooth.pal.R")
col <- smooth.pal(col, 5)

z <- as.matrix(reshape2::dcast(pe_out, num_pop ~ sigma, value.var = "PE")[,-1])
# image(seq(1,10,length.out=nrow(z)),seq(0,0.3,length.out=ncol(z)), z =
# z, col = col, breaks = seq(1-0.0001, max(z)+ 0.00001, length.out =
  # length(col) + 1))
filled.contour(seq(1, 10, length.out = nrow(z)), seq(0, 0.3,
    length.out = ncol(z)), z, col = col, ylab = "Sigma", xlab = "N",
  levels = seq(1, max(z), length.out = length(col)),
  main = "Portfolio Effect", lwd = 0.1)













pe_out <- plyr::ldply(paradox_sim_out, function(x) {
  temp <- plyr::ldply(x, function(y) {

  num_pop <- nrow(y) - 1
  num_t <- ncol(y)
  burn <- 1:500
  effort_row <- num_pop + 1

  biomass <- y[-effort_row, -burn]
  effort <- y[effort_row, -burn]

  if(num_pop > 1) {
	  sd_ts <- apply(biomass,1,sd)
	  mean_ts <- apply(biomass,1,mean)
 	  total <- colSums(biomass)
  } else {
    sd_ts <- sd(biomass)
    mean_ts <- mean(biomass)
 	  total <- biomass
  }

	sd_total <- sd(total, na.rm = TRUE)
	mean_total <- mean(total, na.rm = TRUE)

	CV_ts <- sd_ts / mean_ts
	CV_total <- sd_total / mean_total

  PE <- mean(CV_ts / CV_total, na.rm = TRUE)
  #if(is.na(PE)) browser()
  sync <- sd(total)^2/(sum(sd_ts))^2

  data.frame(PE, sync)
})
  message("...")
  data.frame(PE = mean(temp$PE), sync = mean(temp$PE))
})

col <- RColorBrewer::brewer.pal(9, "YlOrRd")
source("~/Dropbox/Rpackages/dataink/R/smooth.pal.R")
col <- smooth.pal(col, 5)

z <- as.matrix(reshape2::dcast(pe_out, num_pop ~ sigma, value.var = "PE")[,-1])
image(seq(1,10,length.out=nrow(z)),seq(0,0.3,length.out=ncol(z)), z = z, col = col, breaks = seq(1-0.0001, max(z)+ 0.00001, length.out = length(col) + 1))
pdf("pe-paradox-test.pdf", width = 5, height = 5)
filled.contour(seq(1,10,length.out=nrow(z)),seq(0,0.3,length.out=ncol(z)),z,col=col,ylab="sigma",xlab="N",levels = seq(1, max(z), length.out = length(col)), main="Portfolio Effect", lwd = 0.1)
dev.off()
