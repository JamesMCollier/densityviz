st_assign_colors = function(starsObj1, attrOf1, starsObj2, attrOf2, terciles, bounds = NULL){
  colors <- Vizumap::build_palette(name = "usr", colrange = list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))
  data = st_getAttributes(starsObj1, attrOf1, starsObj2, attrOf2)
  if(is.null(bounds)) bounds = st_findNbounds(starsObj1, attrOf1, starsObj2, attrOf2, terciles)

  # function logic taken from Vizumap::build_bmap
  var1_col <- cut(data[, "attr1"], breaks = bounds[1:4])
  var2_col <- cut(data[, "attr2"], breaks = bounds[5:8])

  var1_var2_levels <- c(paste(levels(var1_col), levels(var2_col)[1]),
                        paste(levels(var1_col), levels(var2_col)[2]),
                        paste(levels(var1_col), levels(var2_col)[3]))

  bothVars <- factor(paste(as.vector(var1_col), as.vector(var2_col)),
                     levels = var1_var2_levels, exclude = NULL)
  data$bothVars = bothVars

  levels(bothVars) <- colors
  data$hex_code <- as.character(bothVars)
  data
}

st_getAttributes = function(starsObj1, attrOf1, starsObj2, attrOf2){
  dimsOfObj1 = dim(starsObj1[[attrOf1]]);
  dimsOfObj2 = dim(starsObj2[[attrOf2]]);
  if(!isTRUE(all.equal(dimsOfObj1,dimsOfObj2, check.attributes = FALSE))){
    stop("Please ensure that the objects have the same dimensions
              - They need to be on the same size grid, if applicable
              - They may have different size time dimensions\nConsider dplyr::filter() or otherwise resizing the objects")
  }

  s1 = starsObj1[[attrOf1]]
  s2 = starsObj2[[attrOf2]]
  NonMissingIndices = (!is.na(s1)) & (!is.na(s2))
  s1 = s1[NonMissingIndices];
  s2 = s2[NonMissingIndices];

  data = data.frame(attr1 = s1, attr2 = s2)
  data

}

st_findNbounds = function(starsObj1, attrOf1, starsObj2, attrOf2, terciles){
  data = st_getAttributes(starsObj1, attrOf1, starsObj2, attrOf2)

  bounds = findNbounds_helper(data, "attr1", "attr2", terciles)
  bounds
}

findNbounds_helper = function(data, estimate, error, terciles){
  # taken from Vizumap::findNbounds
  # max and mins
  max_estimate <- 1.05*max(data[ ,estimate])
  min_estimate <- 1.05*min(data[ ,estimate])

  max_error <- max(data[ ,error]) + .01*max(data[ ,error])
  min_error <- min(data[ ,error]) + .01*min(data[ ,error])

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
  bound <- c(round(min_estimate, 2), estimate_bin1, estimate_bin2, estimate_bin3,
             round(min_error, 2), error_bin1, error_bin2, error_bin3)

  bound
}
