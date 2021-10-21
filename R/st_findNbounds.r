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
  bounds <- findNbounds_helper(data, "attr1", "attr2", terciles)
  bounds
}
