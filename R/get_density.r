#' Get 2d density
#'
#' A convenience function to find 2d densities using MASS::kde2d.
#' get_density function found at https://slowkow.com/notes/ggplot2-color-by-density/
#'
#' @param x A numeric vector
#' @param y A numeric vector
#' @param ... Create a square n by n grid to compute density.
#'
#' @return The density within each square
#' @export
#'
#' @examples
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- base::findInterval(x, dens$x)
  iy <- base::findInterval(y, dens$y)
  ii <- base::cbind(ix, iy)
  return(dens$z[ii])
}
