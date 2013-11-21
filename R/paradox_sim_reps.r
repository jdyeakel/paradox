#' Run the paradox simulation over multiple repetitions
#'
#' This function takes care of running \code{\link{paradox_sim}} over
#' many parameter values and over many repetitions.
#' @param ... Anything extra to pass to \code{\link{paradox_sim}}.
#' @export


paradox_sim_reps <- function(t_end = 500, burnin = 200, alpha = 0.5,
  beta = 1/150, sigma = seq(0, 0.3, 0.05), num_pop = seq(1, 20), m =
  0.1, q = 0.01, cpar = 2, p = 1, n = 1, reps = 20, return_ts = FALSE,
  .parallel = FALSE, ...) {


  d_ <- data.frame(expand.grid(alpha = alpha, beta = beta, sigma =
      sigma, num_pop = num_pop, m = m, q = q, cpar = cpar, p = p, n = n))

  # add IDs for scenario (state of nature) and repetitions:
  d_$scenario <- seq_len(nrow(d_))
  d <- plyr::ldply(seq_len(reps), function(x) data.frame(d_,
      repetition = x))

  # run the models:
  rows_ <- seq_len(nrow(d))
  if(.parallel) {
    setup_parallel()
    out_ <- foreach(row_i = rows_, .verbose = FALSE) %dopar% {
      args_i <- d[row_i, ]
      with(args_i, paradox_sim(t_end = t_end, alpha = rep(alpha, num_pop),
          beta = beta, sigma = sigma, num_pop = num_pop, m = m, q = q,
          cpar = cpar, p = p, n = n, return_ts = FALSE, burnin =
          burnin))$performance
    }
  } else {
    require(foreach)
    out_ <- foreach(row_i = rows_, .verbose = FALSE) %do% {
      args_i <- d[row_i, ]
      with(args_i, paradox_sim(t_end = t_end, alpha = rep(alpha, num_pop),
          beta = beta, sigma = sigma, num_pop = num_pop, m = m, q = q,
          cpar = cpar, p = p, n = n, return_ts = FALSE, burnin =
          burnin))$performance
    }
  }

  # now manipulate and condense to means per scenario of parameters
  out_df <- data.frame(d, plyr::ldply(out_)) # switch to data.frame and add `d`
  out_df$repetition <- as.factor(out_df$repetition)

  out_list <- split(out_df, out_df$scenario)
  out_list_ <- lapply(out_list, function(x) with(x, data.frame(pe =
        mean(pe), mean_mean_ts = mean(mean_mean_ts), mean_sd_ts =
        mean(mean_sd_ts), sd_total = mean(sd_total), mean_total =
        mean(mean_total), sync = mean(sync), vuln = mean(vuln))))
  out <- plyr::ldply(out_list_)
  out$.id <- NULL
  out <- data.frame(d_, out)
  out
}
