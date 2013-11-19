#' Smooth a colour palette by interpolation.
#'
#' Takes a sequence of hex code colour codes and smooths the colour
#' palette to a specified extended length.
#'
#' @details
#' You might want to consider using colour palettes from the package
#' \code{RColorBrewer}.
#' @param pal A character vector of hex code colours.
#' @param n The number of colours to interpolate between colours. A
#' larger value creates a smoother palette.
#' @export
#' @return
#' A character vector containing the smoothed colour palette.
#' @author Sean Anderson
#' @examples
#' require(RColorBrewer)
#' pal <- brewer.pal(9, "YlOrRd")
#' par(mfrow = c(1, 2))
#' # original version:
#' plot(1:length(pal), pch = 19, cex = 2, col = pal)
#' smoothed_palette <- smooth_pal(pal)
#' # smoothed version:
#' plot(1:length(smoothed_palette), pch = 19, cex = 2, col = smoothed_palette)

smooth_pal <- function(pal, n = 10){
pal.out <- sapply(1:(length(pal) - 1), function(i) {
  rgb(sapply(1:3, function(j) {
    seq(col2rgb(pal[i])[j]/255, col2rgb(pal[i+1])[j]/255, length.out = n)
  }))})
  as.character(pal.out)
}
