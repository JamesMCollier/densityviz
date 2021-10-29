#' Add arrows to bivariate histograms
#'
#' Show change in bins from one histogram to another
#'
#' @param histogram.primary the histogram that arrows will be plotted on
#' @param histogram.base the histogram from which arrows will be calculated
#'
#' @return histogram.primary
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' library(stars)
#' library(Vizumap)
#' library(gridExtra)
#' palette = build_palette(name = "BlueRed")
#' # matched time to decadal time periods and produce histograms
#' time <- as.numeric(st_get_dimension_values(tmax, 2))
#' periods <- list(c(1950:1959), c(1960:1969), c(1970:1979), c(1980:1989),c(1990:1999))
#' decades <- lapply(periods, function(x, time) which(time %in% x), time)
#'
#' # Find bounds on whole data set
#' out = st_assign_colors(tmax, 1, tmin, 1, FALSE, palette = palette)
#' bounds = out$bounds
#'
#' # Preparing the data
#' out2 <- st_assign_colors(tmax[,,decades[[5]]], 1, tmin[,,decades[[5]]], 1, terciles = FALSE,
#' palette = palette, bounds = bounds)
#' data2 <- out2$data
#' out1 <- st_assign_colors(tmax[,,decades[[1]]], 1, tmin[,,decades[[1]]], 1, terciles = FALSE,
#' palette = palette, bounds = bounds)
#' data1 <- out1$data
#'
#'
#' # generating and plotting the histogram
#' bvh1 <- st_bivariate_histogram(data = data1,
#'             title = "Max/Min temperature distribution (1950-1959)", ymax = 1)
#' bvh1
#'
#' bvh2 <- st_bivariate_histogram(data = data2,
#'            title = "Max/Min temperature distribution (1990-1999)", ymax = 1)
#' bvh2
#'
#' colnames(data1) = c("Tmax","Tmin","bothVars","hex_code")
#' colnames(data2) = c("Tmax","Tmin","bothVars","hex_code")
#' key <- build_bkey(data1, palette, terciles = FALSE)
#' key = st_change_key_bounds(key, bounds)
#'
#' # add arrows
#' bvh2 = add_arrows(histogram.primary = bvh2, histogram.base = bvh1)
#'
#' # attach key
#' densityviz::attach_key(arrangeGrob(bvh1, bvh2, ncol = 1), key)
add_arrows = function(histogram.primary,histogram.base){
  layer.base = layer_data(histogram.base)
  layer.primary = layer_data(histogram.primary)
  layer.base = arrange(layer.base, x)
  layer.primary = arrange(layer.primary, x)

  if(nrow(layer.base) != nrow(layer.primary)){
    stop("AH")
  }

  ggData = data.frame(x = as.vector(layer.primary$x))
  ggData$yPrimary = layer.primary$y
  ggData$yBase = layer.base$y
  ggData$change = layer.primary$y - layer.base$y
  sign = sign(ggData$change)
  sign[sign==1] = "up"
  sign[sign==-1] = "down"
  sign[sign==0] == ""
  ggData$sign = sign

  j = 1
  for(i in ggData$x){
    if(ggData$sign[j] == "up"){
      histogram.primary = histogram.primary +
        annotate("segment", x = i, xend = i, y = ggData$yPrimary[j] + 0.05, yend = ggData$yPrimary[j] + 0.05 + 0.1, arrow = arrow(), color = "green")
    }else if(ggData$sign[j] == "down"){
      histogram.primary = histogram.primary +
        annotate("segment", x = i, xend = i, y = ggData$yPrimary[j] + 0.05 + 0.1, yend = ggData$yPrimary[j] + 0.05, arrow = arrow(), color = "red")
    }
    j = j + 1
  }

  histogram.primary
}
