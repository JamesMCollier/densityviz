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
