\name{paradox_sim_reps}
\alias{paradox_sim_reps}
\title{Run the paradox simulation over multiple repetitions}
\usage{
  paradox_sim_reps(t_end = 500, burnin = 200, alpha = 0.5,
    beta = 1/150, sigma = seq(0, 0.3, 0.05),
    num_pop = seq(1, 20), m = 0.1, q = 0.01, cpar = 2,
    p = 1, n = 1, reps = 20, return_ts = FALSE,
    .parallel = FALSE, ...)
}
\arguments{
  \item{...}{Anything extra to pass to
  \code{\link{paradox_sim}}.}
}
\description{
  This function takes care of running
  \code{\link{paradox_sim}} over many parameter values and
  over many repetitions.
}

