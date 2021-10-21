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
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ridgelineBy(GISTEMP_Decadal, 1, 3, "Ridgeline", y_reverse = TRUE, breaks = seq(1970,2010,10))
#'
#' # can subset stars objects by sf objects, such as countries from the rnaturalearth package
#' ridgelineBy(GISTEMP_Decadal[rnaturalearth::ne_countries(country = "Australia", returnclass = "sf")], 1, 3, "Australia Ridgeline", breaks = seq(1970,2010,10))
ridgelineBy = function(starsObj, attr, dim, plotTitle, y_reverse = FALSE, sd = TRUE, ...){
  if(sd == TRUE) range = c(-3 * stats::sd(starsObj[[attr]], na.rm = TRUE), 3 * stats::sd(starsObj[[attr]], na.rm = TRUE))
  else range = c(base::min(base::as.numeric(starsObj[[attr]]), na.rm = TRUE), base::max(base::as.numeric(starsObj[[attr]]), na.rm = TRUE))

  df = base::data.frame(attr = base::as.vector(starsObj[[attr]]))
  # num data points per time value
  n = base::length(base::as.vector(starsObj[[attr]])) / base::length(stars::st_get_dimension_values(starsObj, dim))
  df$states = base::rep(stars::st_get_dimension_values(starsObj, dim), each = n)

  # ggplot2 needs data in long form
  gg = ggplot2::ggplot(df, ggplot2::aes(x = .data$attr, y = .data$states, fill = .data$..x..,
                      group = .data$states)) +
    ggridges::geom_density_ridges_gradient(scale = 3,rel_min_height = 0.01) +
    ggplot2::scale_fill_distiller(palette = "RdBu", limits = range) +
    ggplot2::theme_bw() + ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 6),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 5),
      legend.position="none",
      panel.spacing = ggplot2::unit(0.1, "lines"),
      strip.text.x = ggplot2::element_text(size = 8)
    ) + ggplot2::xlim(range) +
    ggplot2::labs(title = plotTitle)

  if(y_reverse == TRUE){
    return(gg + ggplot2::scale_y_reverse(...))
  }else{
    return(gg + ggplot2::scale_y_continuous(...))
  }
}
