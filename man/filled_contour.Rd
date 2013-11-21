\name{filled_contour}
\alias{filled_contour}
\title{A customized version of \code{filled.contour} that doesn't set
\code{layout}}
\usage{
  filled_contour(x, y, z, xlim = range(x, finite = TRUE),
    ylim = range(y, finite = TRUE),
    zlim = range(z, finite = TRUE), levels, col, ...)
}
\arguments{
  \item{x}{x values}

  \item{y}{y values}

  \item{z}{z values (what is coloured)}

  \item{xlim}{x limit values (a numeric vector of length
  2)}

  \item{ylim}{y limit values (a numeric vector of length
  2)}

  \item{zlim}{z (colour) limit values (a numeric vector of
  length 2)}

  \item{levels}{Levels at which to split the z data.}

  \item{col}{Colour palette to use.}
}
\description{
  This is a (highly) stripped down version of
  \code{filled.contour} from base R that doesn't set
  \code{layout}. It is therefore useful for multipanel
  plotting. Many arguments have been removed to simplify
  the function. See \code{?filled.contour} for further
  description of the arguments.
}
\details{
  This function merely calls \code{.filled.contour}, but
  also deals with setting up an initial plotting window.
}

