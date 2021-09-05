#' Animate Densities by Dimension Values
#'
#' @param starsObj stars object
#' @param attr attribute of interest
#' @param dim grouping dimension
#' @param title plot title
#' @param anim_range if specified, dimension values along which to animate
#' @param ... arguments passed to plotDensityByDimensionValue()
#'
#' @return
#' @export
#'
#' @examples
#' animateHTMLBy(GISTEMP, 1, 3, "Australia", anim_range = 1981:2020, baseline = c(1951,1980), sf = rnaturalearth::ne_countries(country = "Australia", returnclass = "sf"))
animateHTMLBy = function(starsObj, attr, dim, title, anim_range = NULL, ...){
  animation::ani.options(interval = .4)
  animation::saveHTML(anim_factory(starsObj, attr, dim, title, ..., anim_range),
           autoplay = FALSE,
           loop = FALSE,
           verbose = FALSE,
           outdir = ".",
           htmlfile = "index.html")
}


#' @rdname animateHTMLBy
#' @export
anim_factory = function(starsObj, attr, dim, title, anim_range = NULL, ...){
  if(!is.null(anim_range)){
    for(i in anim_range){
      base::print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
    }
  }else{
    for(i in base::unique(stars::st_get_dimension_values(starsObj, dim))){
      base::print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
    }
  }
}

#' Plot Density for a Chosen Dimension Value
#'
#' @param starsObj a stars object
#' @param attr attribute of interest
#' @param dim dimension of choice
#' @param value dimension value by which to plot
#' @param title title of plot
#' @param sf if not null, an sf object (e.g., a country from the rnaturalearth package) that will be plotted as an inset on the graph
#' @param baseline a vector of two dimension values that will provide a baseline density curve on the graph within the specified range.
#' @param ylim if specified, the range of y values for the density curve (by default, 0-0.5)
#' @param xlim if specified, the range of the x axis
#' @param sd how should the x axis range be determined programmatically? if TRUE (the default),
#' the x axis range is within three standard deviations of the attribute, if FALSE the range is the
#' minimum and maximum of the attribute.
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' plotDensityByDimensionValue(GISTEMP, 1, 3, 1980, "Australia", sf = rnaturalearth::ne_countries(country = "Australia", returnclass = "sf"))
#'
plotDensityByDimensionValue = function(starsObj, attr, dim, value, title, sf = NULL, baseline = NULL, ylim = c(0,0.5), xlim = NULL, sd = TRUE){
  if(sd == TRUE) range = base::c(-3 * stats::sd(starsObj[[attr]], na.rm = TRUE), 3 * stats::sd(starsObj[[attr]], na.rm = TRUE))
  else range = base::c(base::min(base::as.numeric(starsObj[[attr]]), na.rm = TRUE), base::max(base::as.numeric(starsObj[[attr]]), na.rm = TRUE))

  df = base::data.frame(attr = base::as.vector(starsObj[[attr]]))
  # num data points per dim value
  # consider a different method, sometimes n is not an integer and an error occurs
  # depending on what starsObj is passed to the function
  n = base::length(base::as.vector(starsObj[[attr]])) / base::length(stars::st_get_dimension_values(starsObj, dim))
  df$states = base::rep(stars::st_get_dimension_values(starsObj, dim), each = n)

  data_x = data_y = data_states = base::vector()
  x = dplyr::pull(dplyr::filter(df, .data$states == value, !base::is.na(attr)), attr)
  y = stats::density(x, n = 2^12, adjust = 3)
  state = base::rep(value, 2^12)

  data_x = base::c(data_x, y$x)
  data_y = base::c(data_y, y$y)
  data_states = base::c(data_states, state)

  data = base::data.frame(x = data_x, y = data_y, states = data_states)

  # ggplot2 needs data in long form, and geom_density() doesn't have the
  # functionality for color fill we'd like, so there are a few extra steps above
  gg = ggplot2::ggplot(data, ggplot2::aes(.data$x, .data$y)) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$x, yend = 0, color = .data$x)) +
    ggplot2::scale_color_distiller(palette = "RdBu", limits = range) +
    ggplot2::geom_line() +
    ggplot2::xlim(range) +
    ggplot2::ylim(ylim) +
    ggplot2::labs(title = base::paste(title, value))

  if(!base::is.null(baseline)){
    d1 = dplyr::filter(df, dplyr::between(.data$states, baseline[1], baseline[2]), !base::is.na(attr))
    gg = gg + ggplot2::geom_density(data = d1,
                           adjust = 3, inherit.aes = FALSE, mapping = ggplot2::aes(x=.data$attr),
                           linetype = "dotted")
  }

  if(!base::is.null(sf)){
    sf$avg = base::rep(base::mean(dplyr::pull(dplyr::filter(df, .data$states == value), attr), na.rm = TRUE), base::nrow(sf))
    base::print(base::unique(sf$avg))
    m1 = ggplot2::ggplot(sf::st_as_sf(sf)) + ggplot2::geom_sf(ggplot2::aes(fill = .data$avg)) +
      ggplot2::coord_sf(expand = FALSE) +
      ggplot2::scale_fill_distiller(palette = "RdBu", limits = range) +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 8),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none"
      )

    x.min = .3 * range[2]
    x.max = .9 * range[2]

    gg = gg + ggplot2::annotation_custom(
      grob = ggplot2::ggplotGrob(m1),
      xmin = x.min,
      xmax = x.max,
      ymin = .35,
      ymax = Inf
    )
  }

  if(!base::is.null(xlim)){
    gg = gg + ggplot2::xlim(xlim)
    range = xlim
  }

  return(gg)
}
