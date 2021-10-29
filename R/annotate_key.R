#' annotate_key
#'
#' Adds key annotations that correspond to the bin's position on the x-axis
#'
#' @param key a key created by Vizumap::build_bkey
#'
#' @return the key with annotations; of class "bivkey"
#' @import ggplot2
#' @import Vizumap
#' @import dplyr
#' @export
#'
#' @examples
#' out = st_assign_colors(tmax, 1, tmin, 1, terciles = TRUE)
#' data = out$data
#' colors = out$colors
#'
#' key = Vizumap::build_bkey(data, colors, terciles = TRUE)
#' annotate_key(key)
annotate_key = function(key){
  if(class(key)[1] != "bivkey")
    stop("Object is not of class 'bivkey'\nBuild key using Vizumap::build_bkey")

  tiles = data.frame(x = key$tiles$x, y = key$tiles$y, group = key$tiles$group)
  by_group = tiles %>% group_by(group) %>% dplyr::summarise(centroidx = mean(x), centroidy = mean(y))

  Vizumap::view(key) + geom_label(data=by_group, aes(x = centroidx, y = centroidy, label = group, fontface = "bold"))
}
