\name{paradox_pe_sim}
\alias{paradox_pe_sim}
\title{Simulate biomass and effort trajectories}
\usage{
  paradox_pe_sim(t_end = 1000L, num_pop = 10L,
    alpha = numeric(), beta = 1/150, m = 0.01, n = 1,
    sigma = 0.05, q = 0.01, cpar = 1.4, p = 0.5,
    effort_init = 10, biomass_init = 50, vuln_thresh = 0.1,
    burnin = 500L, return_ts = FALSE)
}
\arguments{
  \item{t_end}{Number of time steps to simulate over}

  \item{num_pop}{Number of populations}

  \item{alpha}{alpha parameter in the Shepherd
  stock-recruit function (density-independent growth).
  Entered as a numeric vector with one alpha per
  population.}

  \item{beta}{beta parameter in the Shepherd stock-recruit
  function (capacity parameter)}

  \item{m}{Natural mortality}

  \item{n}{n parameter in the Shepherd stock-recruit
  function. Controls compensation level.}

  \item{sigma}{Standard deviation on the simulated alpha
  values}

  \item{q}{Effectiveness parameter in the effort function}

  \item{cpar}{Operational costs parameter in the effort
  function}

  \item{p}{Intrinsic value of the fishery parameter in the
  effort function}

  \item{effort_init}{Initial effort.}

  \item{biomas_init}{Initial population biomass. Recycled
  across all populations.}

  \item{vuln_threshold}{The vulnerability threshold. A
  proportion (between 0 and 1) indicating the fraction of
  the mean biomass of a subpopulation before that
  subpopulation is declared "vulnerable".}

  \item{burnin}{The number of years to discard as burnin.}

  \item{return_ts}{Logical indicating whether the time
  series should be returned as part of the output.
  \code{FALSE} by default to save memory if running many
  repetitions.}
}
\value{
  A list object. \code{$performance} contains the
  performance attributes. If \code{return_ts = TRUE}:
  \code{$biomass} contains the biomass matrix (time is
  incremented along the columns and populations down the
  rows without the burnin period removed); \code{$effort}
  contains the effort in a numeric vector.
}
\description{
  Simulate biomass and effort trajectories
}
\examples{
out <- paradox_pe_sim(alpha = rep(0.5, 10), return_ts = TRUE)
names(out)
print(out$performance)
op <- par(mfrow = c(2, 1), mar = c(4, 4, .5, .5))
matplot(t(out$biomass[, -c(1:500)]), type = "l", lty = 1, xlab = "Year",
  ylab = "Biomass")
plot(out$effort[-c(1:500)], type = "l", ylab = "Effort", xlab = "Year")
par(op)
}
\author{
  Original model developed by Justin Yeakel. C++ version
  originally ported by Sean Anderson.
}

