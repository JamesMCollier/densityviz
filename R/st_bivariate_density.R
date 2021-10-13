#' Produce density plots of bivariate data with 'raster' geom
#'
#' @param starsObj1 a stars object
#' @param attr1 attribute of starsObj1 to be plotted
#' @param starsObj2 a stars object
#' @param attr2 attribute of starsobj2 to be plotted
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' st_2d_raster_density(tmax[,,14613:18263],1,tmin[,,14613:18263],1)
st_2d_raster_density = function(starsObj1, attr1, starsObj2, attr2){

  data = st_getAttributes(starsObj1, attr1, starsObj2, attr2)


  a = ggplot2::ggplot(data, ggplot2::aes(x=attr1, y=attr2)) +
    ggplot2::stat_density_2d(geom = "raster", ggplot2::aes(fill = ggplot2::after_stat(density)), contour = FALSE) +
    ggplot2::scale_fill_distiller(palette = "RdBu")

  a
}

#' Produce points and density contour map for bivariate data
#'
#' @param starsObj1 a stars object
#' @param attr1 attribute of starsObj1 to be plotted
#' @param starsObj2 a stars object
#' @param attr2 attribute of starsObj2 to be plotted
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' st_bivariate_contour_map(tmax[,,14613:18263],1,tmin[,,14613:18263],1)
st_bivariate_contour_map = function(starsObj1, attr1, starsObj2, attr2){

  data = st_getAttributes(starsObj1, attr1, starsObj2, attr2)

  b = ggplot2::ggplot(data, ggplot2::aes(x=attr1,y=attr2)) +
    ggplot2::geom_point(alpha = 0.25) +
    ggplot2::geom_density_2d_filled(alpha = 0.9) +
    ggplot2::geom_density_2d(size = .30, color = "black")

  b
}

# get_density function found at https://slowkow.com/notes/ggplot2-color-by-density/
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- base::findInterval(x, dens$x)
  iy <- base::findInterval(y, dens$y)
  ii <- base::cbind(ix, iy)
  return(dens$z[ii])
}

#' Produce point density plot of bivariate data
#'
#' @param starsObj1 a stars object
#' @param attr1 attribute of starsObj1 to be plotted
#' @param starsObj2 a stars object
#' @param attr2 attribute of starsObj2 to be plotted
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' st_bivariate_pointdensity(tmax[,,14613:18263],1,tmin[,,14613:18263],1)
st_bivariate_pointdensity = function(starsObj1, attr1, starsObj2, attr2){

  data = st_getAttributes(starsObj1, attr1, starsObj2, attr2)
  data$density = get_density(data$attr1, data$attr2, n=100)

  ggplot2::ggplot(data, ggplot2::aes(x=attr1, y=attr2, color = density)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::scale_color_distiller(palette = "RdBu")
}
