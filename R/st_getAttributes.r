#' st_getAttributes
#'
#' Function to extract the attributes of two stars objects
#'
#' @param starsObj1 stars object
#' @param attrOf1 attrOf1
#' @param starsObj2 stars object
#' @param attrOf2 attrOf2
#'
#' @export

st_getAttributes <- function(starsObj1, attrOf1, starsObj2, attrOf2){


  dimsOfObj1 <- dim(starsObj1[[attrOf1]])
  dimsOfObj2 <- dim(starsObj2[[attrOf2]])

  if(!isTRUE(all.equal(dimsOfObj1,dimsOfObj2, check.attributes = FALSE))){
    stop("Please ensure that the objects have the same dimensions
              - They need to be on the same size grid, if applicable
              - They may have different size time dimensions\nConsider dplyr::filter() or otherwise resizing the objects")
  }

  s1 <- starsObj1[[attrOf1]]
  s2 <- starsObj2[[attrOf2]]
  NonMissingIndices <- (!is.na(s1)) & (!is.na(s2))
  s1 <- s1[NonMissingIndices];
  s2 <- s2[NonMissingIndices];

  data <- data.frame(attr1 = s1, attr2 = s2)

  data

}
