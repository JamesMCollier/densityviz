#' Produce density plots of bivariate data with 'raster' geom
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
#' st_2d_raster_density(data)
st_2d_raster_density <- function(data){
  attr1 = names(data)[1]
  attr2 = names(data)[2]

  rasdens <- ggplot(data, aes_string(x = attr1, y = attr2)) +
             stat_density_2d(geom = "raster",
                             aes(fill = after_stat(density)), contour = FALSE) +
             scale_fill_distiller(palette = "RdBu")

  rasdens
}





