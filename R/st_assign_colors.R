#' st_assign_colors
#'
#' Bin numeric data and assign colors for plotting
#'
#' @param starsObj1 a stars object
#' @param attrOf1 which attribute of starsObj1 should be considered?
#' @param starsObj2 a stars object
#' @param attrOf2 which attribute of starsObj2 should be considered?
#' @param terciles should terciles be calculated?
#' @param bounds if custom bounds/levels are desired, default NULL
#' @param palette see Vizumap::build_palette if a custom palette is desired
#'
#' @return a data frame of the attributes, factor levels, and color (in hexadecimal)
#' @export
#'
#' @import Vizumap
#'
#' @examples
#' out <- st_assign_colors(tmax, 1, tmin, 1, terciles = FALSE)
#' data <- out$data
#' colors <- out$colors
#' bounds <- out$bounds
#'
st_assign_colors <- function(starsObj1, attrOf1, starsObj2, attrOf2, terciles,
                             bounds = NULL, palette = NULL){
  if(is.null(palette))
    colors <- build_palette(name = "usr",
                            colrange = list(colour = c("darkblue", "chartreuse4"),
                                            difC = c(3, 4)))
  else{
    if(class(palette)[1] != "palette")
      stop("Object is not of class 'palette'.")
    colors <- palette
  }
  data <- st_getAttributes(starsObj1, attrOf1, starsObj2, attrOf2)
  if(is.null(bounds)) bounds <- st_findNbounds(data, terciles)

  # function logic taken from Vizumap::build_bmap
  var1_col <- cut(data[, "attr1"], breaks = bounds[1:4])
  var2_col <- cut(data[, "attr2"], breaks = bounds[5:8])

  var1_var2_levels <- c(paste(levels(var1_col), levels(var2_col)[1]),
                        paste(levels(var1_col), levels(var2_col)[2]),
                        paste(levels(var1_col), levels(var2_col)[3]))

  bothVars <- factor(paste(as.vector(var1_col), as.vector(var2_col)),
                     levels = var1_var2_levels, exclude = NULL)
  data$bothVars <- bothVars

  levels(bothVars) <- colors
  data$hex_code <- as.character(bothVars)

  list(data = data, colors = colors, bounds = bounds)
}





