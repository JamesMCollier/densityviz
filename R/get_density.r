# get_density function found at https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- base::findInterval(x, dens$x)
  iy <- base::findInterval(y, dens$y)
  ii <- base::cbind(ix, iy)
  return(dens$z[ii])
}
