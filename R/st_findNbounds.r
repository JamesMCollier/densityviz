#' st_findNbounds
#'
#' Calculate bins for coloring
#'
#' @param data dataset from which bounds are to be determined, column 1 is called "attr1" and column 2 is called "attr2"
#' @param terciles should terciles be calculated?
#'
#' @return vector of 8 with bounding values for the two variables
#' @export
#'
#'
st_findNbounds <- function(data, terciles){
  var1 = names(data)[1]
  var2 = names(data)[2]
  bounds = findNbounds_helper(data, var1, var2, terciles)
  bounds
}
