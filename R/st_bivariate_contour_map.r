#' Produce points and density contour map for bivariate data
#'
#' @param data the data in long form, as returned from st_getAttributes or st_assign_colors.
#' The first two columns should be the variables of interest.
#'
#' @return a ggplot
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' library(stars)
#' # matched time to decadal time periods
#' time <- as.numeric(st_get_dimension_values(tmax, 2))
#' periods <- list(c(1950:1959), c(1960:1969), c(1970:1979), c(1980:1989),c(1990:1999))
#' decades <- lapply(periods, function(x, time) which(time %in% x), time)
#'
#' # We wish to view data from 1990-1999
#' data = st_getAttributes(tmax[,,decades[[5]]], 1, tmin[,,decades[[5]]], 1)
#' colnames(data) = c("MaxTemp", "MinTemp")
#' st_bivariate_contour_map(data)
st_bivariate_contour_map = function(data){
  attr1 = names(data)[1]
  attr2 = names(data)[2]

  b = ggplot2::ggplot(data, ggplot2::aes_string(x=attr1,y=attr2)) +
    ggplot2::geom_point(alpha = 0.25) +
    ggplot2::geom_density_2d_filled(alpha = 0.9) +
    ggplot2::geom_density_2d(size = .30, color = "black")

  b
}
