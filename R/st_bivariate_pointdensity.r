#' Produce point density plot of bivariate data
#'
#' @param data the data in long form, as returned from st_getAttributes and get_density.
#' The first two columns should be the variables of interest and the third should be their density
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
#' colnames(data) = c("MaxTemp", "MinTemp)
#' data$density = get_density(data$MaxTemp, data$MinTemp, n=100)
#'
#' st_bivariate_pointdensity(data)
st_bivariate_pointdensity <- function(data){
  attr1 = names(data)[1]
  attr2 = names(data)[2]
  density = names(data)[3]

  ggplot2::ggplot(data, ggplot2::aes_string(x=attr1, y=attr2, color = density)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::scale_color_distiller(palette = "RdBu")
}
