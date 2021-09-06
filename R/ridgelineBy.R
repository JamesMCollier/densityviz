#' Create Ridgeline Plots
#'
#' Generate density plots on the same graph, grouped by a dimension of choice
#'
#' @param starsObj stars object
#' @param attr attribute of interest
#' @param dim grouping dimension
#' @param plotTitle title of plot
#' @param y_reverse should the y axis be reversed?
#' @param sd if TRUE (the default), the range of the x axis is within three standard deviations
#' of the mean, if FALSE the range of the x axis is the minimum and maximum of the attribute
#' @param ... arguments passed to scale_y_continuous() or scale_y_reverse()
#'
#' @return a gg or ggplot object
#'
#' @importFrom rlang .data
#' @importFrom stats sd
#' @import rnaturalearth
#' @import stars
#' @import ggridges
#' @import ggplot2
#'
#'
#' @export
#'
#' @examples
#' ridgelineBy(starsObj = GISTEMP_Decadal, attr = 1, dim = 3, plotTitle = "Ridgeline",
#'   y_reverse = TRUE, breaks = seq(1970, 2010, 10))
#'
#' # can subset stars objects by sf objects, such as countries from the rnaturalearth package
#' sobj <- GISTEMP_Decadal[ne_countries(country = "Australia", returnclass = "sf")]
#' ridgelineBy(starsObj = sobj, attr = 1, dim = 3, plotTitle = "Australia Ridgeline",
#'   breaks = seq(1970,2010,10))
ridgelineBy <- function(starsObj, attr, dim, plotTitle, y_reverse = FALSE, sd = TRUE, ...){
  if(sd == TRUE)
    range <- c(-3 * sd(starsObj[[attr]], na.rm = TRUE), 3 * sd(starsObj[[attr]], na.rm = TRUE))
  else
    range <- c(min(as.numeric(starsObj[[attr]]), na.rm = TRUE), max(as.numeric(starsObj[[attr]]), na.rm = TRUE))

  df <- data.frame(attr = as.vector(starsObj[[attr]]))
  # num data points per time value
  n <- length(as.vector(starsObj[[attr]])) / length(st_get_dimension_values(starsObj, dim))
  df$states <- rep(stars::st_get_dimension_values(starsObj, dim), each = n)

  # ggplot2 needs data in long form
  gg <- ggplot(df, aes(x = .data$attr, y = .data$states, fill = .data$..x..,
                      group = .data$states)) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_distiller(palette = "RdBu", limits = range) +
    theme_bw() + theme(
      axis.title.x = element_text(size = 6),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 5),
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) + xlim(range) +
    labs(title = plotTitle)

  if(y_reverse == TRUE){
    return(gg + scale_y_reverse(...))
  }else{
    return(gg + scale_y_continuous(...))
  }
}
