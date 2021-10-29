#' st_change_key_bounds
#'
#' Alter a key's bounds for display
#'
#' @param key a key build using Vizumap::build_bkey
#' @param bounds the new bounds to be added
#'
#' @return a key, of class "bivkey"
#' @export
#'
#' @examples
#' newBounds = c(-104, -2, 2, 202, -117, -2, 2, 150)
#' out = st_assign_colors(tmax, 1, tmin, 1, FALSE, bounds = newBounds)
#' data = out$data
#' colors = out$colors
#'
#' bvh = st_bivariate_histogram(data = data, title = "Min/Max Temperature Distributions")
#' key = Vizumap::build_bkey(data, colors, terciles = FALSE)
#' key = st_change_key_bounds(key, newBounds)
#'
#' attach_key(bvh, key)
st_change_key_bounds = function(key, bounds){
  if(class(key)[1] != "bivkey")
    stop("Object is not of class 'bivkey'; please use Vizumap::build_bkey and Vizumap::view to construct and view a key")

  if(length(bounds) != 8){
    stop("Please ensure bounds is of length 8")
  }

  key$labels$bound = bounds
  key
}
