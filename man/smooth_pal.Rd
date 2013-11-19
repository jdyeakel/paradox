\name{smooth_pal}
\alias{smooth_pal}
\title{Smooth a colour palette by interpolation.}
\usage{
  smooth_pal(pal, n = 10)
}
\arguments{
  \item{pal}{A character vector of hex code colours.}

  \item{n}{The number of colours to interpolate between
  colours. A larger value creates a smoother palette.}
}
\value{
  A character vector containing the smoothed colour
  palette.
}
\description{
  Takes a sequence of hex code colour codes and smooths the
  colour palette to a specified extended length.
}
\details{
  You might want to consider using colour palettes from the
  package \code{RColorBrewer}.
}
\examples{
require(RColorBrewer)
pal <- brewer.pal(9, "YlOrRd")
par(mfrow = c(1, 2))
# original version:
plot(1:length(pal), pch = 19, cex = 2, col = pal)
smoothed_palette <- smooth_pal(pal)
# smoothed version:
plot(1:length(smoothed_palette), pch = 19, cex = 2, col = smoothed_palette)
}
\author{
  Sean Anderson
}

