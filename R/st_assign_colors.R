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
#' @examples
#' st_assign_colors(tmax,1,tmin,1,FALSE)
st_assign_colors = function(starsObj1, attrOf1, starsObj2, attrOf2, terciles, bounds = NULL, palette = NULL){
  if(base::is.null(palette)) colors <- Vizumap::build_palette(name = "usr", colrange = list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))
  else{
    if(base::class(palette)[1] != "palette")
      stop("Object is not of class 'palette'.")
    colors = palette
  }
  data = st_getAttributes(starsObj1, attrOf1, starsObj2, attrOf2)
  if(base::is.null(bounds)) bounds = st_findNbounds(data, terciles)

  # function logic taken from Vizumap::build_bmap
  var1_col <- base::cut(data[, "attr1"], breaks = bounds[1:4])
  var2_col <- base::cut(data[, "attr2"], breaks = bounds[5:8])

  var1_var2_levels <- c(base::paste(base::levels(var1_col), base::levels(var2_col)[1]),
                        base::paste(base::levels(var1_col), base::levels(var2_col)[2]),
                        base::paste(base::levels(var1_col), base::levels(var2_col)[3]))

  bothVars <- base::factor(base::paste(base::as.vector(var1_col), base::as.vector(var2_col)),
                     levels = var1_var2_levels, exclude = NULL)
  data$bothVars = bothVars

  base::levels(bothVars) <- colors
  data$hex_code <- base::as.character(bothVars)
  data
}

st_getAttributes = function(starsObj1, attrOf1, starsObj2, attrOf2){
  dimsOfObj1 = base::dim(starsObj1[[attrOf1]]);
  dimsOfObj2 = base::dim(starsObj2[[attrOf2]]);
  if(!base::isTRUE(base::all.equal(dimsOfObj1,dimsOfObj2, check.attributes = FALSE))){
    stop("Please ensure that the objects have the same dimensions
              - They need to be on the same size grid, if applicable
              - They may have different size time dimensions\nConsider dplyr::filter() or otherwise resizing the objects")
  }

  s1 = starsObj1[[attrOf1]]
  s2 = starsObj2[[attrOf2]]
  NonMissingIndices = (!base::is.na(s1)) & (!base::is.na(s2))
  s1 = s1[NonMissingIndices];
  s2 = s2[NonMissingIndices];

  data = base::data.frame(attr1 = s1, attr2 = s2)
  data

}

#' Calculate bins for coloring
#'
#' @param data dataset from which bounds are to be determined, column 1 is called "attr1" and column 2 is called "attr2"
#' @param terciles should terciles be calculated?
#'
#' @return vector of 8 with bounding values for the two variables
#' @export
#'
#'
st_findNbounds = function(data, terciles){
  bounds = findNbounds_helper(data, "attr1", "attr2", terciles)
  bounds
}

findNbounds_helper = function(data, estimate, error, terciles){
  # taken from Vizumap::findNbounds
  # max and mins
  max_estimate <- 1.05*base::max(data[ ,estimate])
  min_estimate <- 1.05*base::min(data[ ,estimate])

  max_error <- 1.05*base::max(data[ ,error])
  min_error <- 1.05*base::min(data[ ,error])

  # find width of a color bin for equal interval option
  width_estimate <- (max_estimate  - min_estimate) / 3
  width_error <- (max_error  - min_error) / 3

  # find numerical bounds
  estimate_bin1 <- Vizumap:::calcbin(terciles = terciles, data = data, x = estimate, q = 1/3, bin = 1, width = width_estimate,
                                     min = min_estimate)
  estimate_bin2 <- Vizumap:::calcbin(terciles = terciles, data = data, x = estimate, q = 2/3, bin = 2, width = width_estimate,
                                     min = min_estimate)
  estimate_bin3 <- Vizumap:::calcbin(terciles = terciles, data = data, x = estimate, q = 1, bin = 3, width = width_estimate,
                                     min = min_estimate)

  error_bin1 <- Vizumap:::calcbin(terciles = terciles, data = data, x = error, q = 1/3, bin = 1, width = width_error,
                                  min = min_error)
  error_bin2 <- Vizumap:::calcbin(terciles = terciles, data = data, x = error, q = 2/3, bin = 2, width = width_error,
                                  min = min_error)
  error_bin3 <- Vizumap:::calcbin(terciles = terciles, data = data, x = error, q = 1, bin = 3, width = width_error,
                                  min = min_error)


  # creating a data frame of labels for the color key grid
  bound <- c(base::round(min_estimate, 2), estimate_bin1, estimate_bin2, estimate_bin3,
             base::round(min_error, 2), error_bin1, error_bin2, error_bin3)

  bound
}
