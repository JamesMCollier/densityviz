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
#'
st_bivariate_pointdensity <- function(starsObj1, attr1, starsObj2, attr2){

  data = st_getAttributes(starsObj1, attr1, starsObj2, attr2)
  data$density = get_density(data$attr1, data$attr2, n=100)

  ggplot2::ggplot(data, ggplot2::aes(x=attr1, y=attr2, color = density)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::scale_color_distiller(palette = "RdBu")
}
