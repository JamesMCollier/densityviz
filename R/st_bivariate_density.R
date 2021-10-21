#' Produce density plots of bivariate data with 'raster' geom
#'
#' @param data data
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' # Preparing the data
#' data <- st_getAttributes(tmax[,,14613:18263], 1, tmin[,,14613:18263], 1)
#' rdens <- st_2d_raster_density(data)
#' rdens
#'
st_2d_raster_density <- function(data){


  rasdens <- ggplot(data, aes(x = attr1, y = attr2)) +
             stat_density_2d(geom = "raster",
                             aes(fill = after_stat(density)), contour = FALSE) +
             scale_fill_distiller(palette = "RdBu")

  rasdens
}





