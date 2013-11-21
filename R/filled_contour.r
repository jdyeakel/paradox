#' A customized version of \code{filled.contour} that doesn't set
#' \code{layout}
#'
#' This is a (highly) stripped down version of \code{filled.contour}
#' from base R that doesn't set \code{layout}. It is therefore useful
#' for multipanel plotting. Many arguments have been removed to
#' simplify the function. See \code{?filled.contour} for further
#' description of the arguments.
#'
#' This function merely calls \code{.filled.contour}, but also deals
#' with setting up an initial plotting window.
#' @param x x values
#' @param y y values
#' @param z z values (what is coloured)
#' @param xlim x limit values (a numeric vector of length 2)
#' @param ylim y limit values (a numeric vector of length 2)
#' @param zlim z (colour) limit values (a numeric vector of length 2)
#' @param levels Levels at which to split the z data.
#' @param col Colour palette to use.
#' @export

filled_contour <- function (x, y, z, xlim = range(x, finite = TRUE),
  ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE),
  levels, col, ...) {

    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing 'x' and 'y' values expected")

    plot.new()
    plot.window(xlim, ylim, ...)
    .filled.contour(x, y, z, levels, col)
}
