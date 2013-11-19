\name{get_pe}
\alias{get_pe}
\title{Calculate the portfolio effect and other metrics from a simulation
system}
\usage{
  get_pe(x, num_pop, t_end)
}
\arguments{
  \item{x}{The matrix minus the effort row}

  \item{num_pop}{Number of populations}

  \item{t_end}{Number of years (after removing the burn-in
  period yourself).}
}
\value{
  A named list with the elements: \code{pe} (portfolio
  effect), \code{sd_ts} (vector of standard deviations for
  the populations), \code{mean_ts} (vector of means for the
  subpopulations), \code{sd_total} (standard deviation of
  the total biomass), \code{mean_total} (mean of the total
  biomass), \code{sync} (Loreau and de Mazancourt synchrony
  index).
}
\description{
  This function takes the output from
  \code{\link{paradox_sim}} (minus the row containing the
  effort time series) and calculates various portfolio
  effect metrics. It is implemented in C++ for speed.
}
\examples{
d <- paradox_sim(alpha = rep(0.5, 10))
get_pe(d[-11, -c(1:500)], 10, 5)
}

