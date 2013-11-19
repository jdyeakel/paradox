\name{paradox_sim}
\alias{paradox_sim}
\title{Simulate biomass and effort trajectories}
\usage{
  paradox_sim(t_end = 1000L, num_pop = 10L,
    alpha = numeric(), beta = 1/150, m = 0.01, n = 1,
    sigma = 0.05, q = 0.01, cpar = 1.4, p = 0.5,
    effort_init = 10, biomass_init = 50)
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

  \item{n}{Depensation parameter in the Shepherd
  stock-recruit function}

  \item{sigma}{Standard deviation on the simulated alpha
  values}

  \item{q}{Effectiveness parameter in the effort function}

  \item{cpar}{Operational costs parameter in the effort
  function}

  \item{p}{Intrinsic value of the fishery parameter in the
  effort function}

  \item{biomas_init}{Initial population biomass. Recycled
  across all populations.}

  \item{effort_init}{Initial effort.}
}
\value{
  A matrix. Time is incremented along the columns and
  populations down the rows. The last row represents the
  effort.
}
\description{
  Simulate biomass and effort trajectories
}
\examples{
d <- paradox_sim(alpha = rep(0.5, 10))
matplot(t(d[-11, -c(1:100)]), type = "l", lty = 1)
}
\author{
  Original model developed by Justin Yeakel. C++ version
  implemented by Sean Anderson.
}

