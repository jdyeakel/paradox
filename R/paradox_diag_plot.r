#' Make a multipanel diagnostic plot

#' @export
#' @param x A data.frame as output from \code{\link{paradox_sim_reps}}.
#' @examples
#' out <- paradox_sim_reps(reps = 10)
#' paradox_diag_plot(out)

paradox_diag_plot <- function(x) {

  col <- RColorBrewer::brewer.pal(9, "YlOrRd")
  col <- smooth_pal(col, 5)

  x_vals <- unique(x$num_pop)
  y_vals <- unique(x$sigma)
  pe_vals <- as.matrix(reshape2::dcast(x, num_pop ~ sigma, value.var = "pe")[,-1])
  pe_levels <- seq(1, max(pe_vals), length.out = length(col))

  titles <- c("pe", "mean_mean_ts", "mean_sd_ts", "mean_total", "sd_total")
  z <- plyr::llply(titles, function(y) get_zvals(x, y, col.length =
      length(col)))

  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(3, 3, .5, .5), oma = c(2, 2, 2, 2))
  par(mfrow = c(3, 2), cex = 0.7)
  for(i in seq_len(length(titles))) {
    with(z[[i]], filled_contour(x_vals, y_vals, vals, levels = levels_, col = col))
    mtext(titles[i])
   }
  with(x, plot(num_pop, vuln, col = "#00000050", pch = 19, cex = 0.9))
  invisible()
}

#' A function to get z values for the filled_contour
get_zvals <- function(x, colname, col.length) {
  vals <- as.matrix(reshape2::dcast(x, num_pop ~ sigma, value.var
      = colname)[,-1])
  levels_ <- seq(1, max(vals), length.out = col.length)
  list(vals = vals, levels_ = levels_)
}
