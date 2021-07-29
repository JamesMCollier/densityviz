#' Animate Densities by Dimension Values
#'
#' @param starsObj stars object
#' @param attr attribute of interest
#' @param dim grouping dimension
#' @param title plot title
#' @param anim_range if specified, range of dimension values along which to animate
#' @param ... arguments passed to plotDensityByDimensionValue()
#'
#' @return
#' @export
#'
#' @examples
#'
animateHTMLBy = function(starsObj, attr, dim, title, anim_range = NULL, ...){
  ani.options(interval = .4)
  saveHTML(anim_factory(starsObj, attr, dim, title, ..., anim_range),
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
      print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
    }
  }else{
    for(i in unique(st_get_dimension_values(starsObj, dim))){
      print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
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
#' @export
#'
#' @examples
plotDensityByDimensionValue = function(starsObj, attr, dim, value, title, sf = NULL, baseline = NULL, ylim = c(0,0.5), xlim = NULL, sd = TRUE){
  if(sd == TRUE) range = c(-3 * sd(starsObj[[attr]], na.rm = TRUE), 3 * sd(starsObj[[attr]], na.rm = TRUE))
  else range = c(min(as.numeric(starsObj[[attr]]), na.rm = TRUE), max(as.numeric(starsObj[[attr]]), na.rm = TRUE))

  df = data.frame(attr = as.vector(starsObj[[attr]]))
  # num data points per dim value
  # consider a different method, sometimes n is not an integer and an error occurs
  # depending on what starsObj is passed to the function
  n = length(as.vector(starsObj[[attr]])) / length(st_get_dimension_values(starsObj, dim))
  df$states = rep(st_get_dimension_values(starsObj, dim), each = n)

  data_x = data_y = data_states = vector()
  x = pull(filter(df, states == value, !is.na(attr)), attr)
  y = density(x, n = 2^12, adjust = 3)
  state = rep(i, 2^12)

  data_x = c(data_x, y$x)
  data_y = c(data_y, y$y)
  data_states = c(data_states, state)

  data = data.frame(x = data_x, y = data_y, states = data_states)

  # ggplot2 needs data in long form, and geom_density() doesn't have the
  # functionality for color fill we'd like, so there are a few extra steps above
  gg = ggplot(data, aes(x, y)) +
    geom_segment(aes(xend = x, yend = 0, color = x)) +
    scale_color_distiller(palette = "RdBu", limits = range) +
    geom_line() +
    xlim(range) +
    ylim(ylim) +
    labs(title = paste(title, value))

  if(!is.null(baseline)){
    gg = gg + geom_density(data = filter(df, between(states, baseline[1], baseline[2]), !is.na(attr)),
                           adjust = 3, inherit.aes = FALSE, mapping = aes(x=attr),
                           linetype = "dotted")
  }

  if(!is.null(sf)){
    sf$avg = rep(mean(pull(filter(df, states == value), attr), na.rm = TRUE), nrow(sf))
    print(unique(sf$avg))
    m1 = ggplot(st_as_sf(sf)) + geom_sf(aes(fill = avg)) +
      coord_sf(expand = FALSE) +
      scale_fill_distiller(palette = "RdBu", limits = range) +
      theme(
        axis.title = element_blank(),
        plot.title = element_text(size = 8),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )

    x.min = .3 * range[2]
    x.max = .9 * range[2]

    gg = gg + annotation_custom(
      grob = ggplotGrob(m1),
      xmin = x.min,
      xmax = x.max,
      ymin = .35,
      ymax = Inf
    )
  }

  if(!is.null(xlim)){
    gg = gg + xlim(xlim)
    range = xlim
  }

  return(gg)
}
