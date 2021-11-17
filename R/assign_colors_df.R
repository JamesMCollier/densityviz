#' Bin bivariate ST data and assign colors
#'
#' @param data a data frame with the variables of interest in the first two columns.
#' @param bounds How should the data be binned? Use custom bounds or those returned from st_findNbounds.
#' @param palette A palette built using Vizumap::build_palette or st_build_palette.
#'
#' @return data with two extra columns: bothVars, a factor, and hex_code, the color corresponding to each
#' data point.
#' @export
#' @import ggplot2
#'
#' @examples
#' temps.DF = as.data.frame(tmax)
#' names(temps.DF)[3] = "tmax"
#' tmin.DF = as.data.frame(tmin)
#'
#' temps.DF$tmin = tmin.DF$tmin.nc
#' temps.DF = temps.DF[,c(3,4,1,2)]
#' temps.DF = na.omit(temps.DF)
#'
#' bounds = st_findNbounds(temps.DF, terciles = TRUE)
#' temps.DF = assign_colors.df(temps.DF, bounds = bounds, palette = Vizumap::build_palette("BlueRed"))
#' summary(temps.DF$bothVars)
#'
#' st_bivariate_histogram(temps.DF, title = "Max/Min Temperature Distribution (1949-1999)")
assign_colors.df = function(data, bounds, palette = NULL){
  var1 = names(data)[1]
  var2 = names(data)[2]
  if(is.null(palette)) palette = Vizumap::build_palette(name = "usr", colrange = list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))

  # function logic taken from Vizumap::build_bmap
  var1_col <- cut(data[, var1], breaks = bounds[1:4])
  var2_col <- cut(data[, var2], breaks = bounds[5:8])

  var1_var2_levels <- c(paste(levels(var1_col), levels(var2_col)[1]),
                        paste(levels(var1_col), levels(var2_col)[2]),
                        paste(levels(var1_col), levels(var2_col)[3]))

  bothVars <- factor(paste(as.vector(var1_col), as.vector(var2_col)),
                     levels = var1_var2_levels, exclude = NULL)
  data$bothVars = bothVars

  levels(bothVars) <- palette
  data$hex_code <- as.character(bothVars)
  data
}
