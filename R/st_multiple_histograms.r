#' st_multiple_histograms
#'
#' Create multiple plots of bivariate spatio-temporal data along chosen dimension values
#'
#' @param starsObj1 a stars object
#' @param attr1 which attribute of starsObj1 should be plotted?
#' @param starsObj2 a stars object
#' @param attr2 which attribute of starsObj2 should be plotted?
#' @param dim_values the dimension values along which to create plots. Should be a list. Note that the actual indices of the dimension values are needed, see examples
#' @param terciles should terciles be calculated?
#' @param which which dimension is the one of interest? Should take a value between 1 and 3 (inclusive), use aperm() if need be
#' @param titles titles of the plots, should be of the same length as dim_values
#' @param bounds if custom bounds should be plotted, default NULL
#' @param palette palette
#'
#' @return
#' @export
#'
#' @import gridExtra
#' @import stars
#' @import Vizumap
#'
#' @examples
#' # matched time to decadal time periods and produce histograms
#' time <- as.numeric(st_get_dimension_values(tmax, 2))
#' periods <- list(c(1950:1959), c(1960:1969), c(1970:1979), c(1980:1989),c(1990:1999))
#' decades <- lapply(periods, function(x, time) which(time %in% x), time)
#'
#' histograms <- st_multiple_histograms(tmax, 1, tmin, 1, dim_values = decades,
#'                  terciles = TRUE, which = 2, titles = c(1:5))
#' colnames(data) = c("Tmax","Tmin","bothVars","hex_code")
#' key <- build_bkey(data, palette, terciles = TRUE)
#'
#' attach_key(arrangeGrob(histograms[[1]], histograms[[5]], ncol = 1), key)
#'
st_multiple_histograms <- function(starsObj1, attr1, starsObj2, attr2, dim_values,
                                   terciles, which = 1, titles, bounds = NULL, palette = NULL){
  if(length(titles) != length(dim_values)){
    stop("Please ensure 'titles' and 'dim_values' are the same length")
  }

  value <- st_assign_colors(starsObj1, attr1, starsObj2, attr2, terciles, bounds, palette)
#  if(is.null(bounds)) bounds <- st_findNbounds(value$data, terciles)
  browser()
  ymax <- st_findYMax(value$data)

  key <- build_bkey(data = value$data, palette = value$color, terciles = terciles)

  pL <- st_generate_plotsList(starsObj1, attr1, starsObj2, attr2, dim_values, terciles,
                        value$bounds, ymax, which = which, titles = titles)

  list(pL = pL, data = data, key = key)
}

