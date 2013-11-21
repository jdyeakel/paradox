\name{paradox_diag_plot}
\alias{paradox_diag_plot}
\title{Make a multipanel diagnostic plot}
\usage{
  paradox_diag_plot(x)
}
\arguments{
  \item{x}{A data.frame as output from
  \code{\link{paradox_sim_reps}}.}
}
\description{
  Make a multipanel diagnostic plot
}
\examples{
out <- paradox_sim_reps(reps = 10, t_end = 300, burnin = 150)
paradox_diag_plot(out)
}

