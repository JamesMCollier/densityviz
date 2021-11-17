#' st_bivmap
#'
#' Plot bivariate spatial data. Grid points are colored according to the bivariate distribution.
#' First bin the data with st_assign_colors or assign_colors.df.
#'
#' @param data a data frame as passed from st_assign_colors or assign_colors.df.
#' Columns for Longitude and Latitude are in the third and fourth columns, respectively, and
#' the color corresponding to each row lies in the sixth column.
#' @param ... arguments passed to borders() from ggplot2. A convenient way to get borders from the maps
#' package onto the plot.
#'
#' @return a ggplot object
#' @export
#' @import ggplot2
#'
#' @examples
#'
st_bivmap = function(data, ...){
  Long = names(data)[3]
  Lat = names(data)[4]
  hex = names(data)[6]

  ggplot(data, aes_string(Long, Lat)) +
    geom_raster(aes_string(fill = hex)) + scale_fill_identity() +
    borders(...) + coord_quickmap() +
    scale_size_area()
}
