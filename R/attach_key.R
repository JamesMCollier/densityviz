#' View bivariate histograms with their keys
#'
#' @param plot a plot such as those returned from st_bivariate_histogram()
#' @param VizuKey a key built with Vizumap::build_bkey()
#' @param draw should the plot be drawn on the current device (default)? if FALSE, returns a grob without drawing. See gridExtra::arrangeGrob()
#'
#' @return draws on the current device or returns a grob
#' @export
#'
#' @examples
#' plot = st_bivariate_histogram(tmax,1,tmin,1,terciles=FALSE,title="Max/Min temperature distribution")
#' palette = Vizumap::build_palette(name = "usr", colrange = list(colour = c("darkblue", "chartreuse4"), difC = c(3, 4)))
#' data = st_assign_colors(tmax,1,tmin,1,terciles=FALSE,palette=palette)
#' colnames(data) = c("Tmax","Tmin","bothVars","hex_code")
#' key = Vizumap::build_bkey(data, palette, terciles = TRUE)
#'
#' attach_key(plot,key)
attach_key = function(plot, VizuKey, draw = TRUE){
  k <- Vizumap::view(VizuKey)


  lay <- base::rbind(c(1, 1, 1, 1, NA, NA),
               c(1, 1, 1, 1, 2, 2),
               c(1, 1, 1, 1, 2, 2),
               c(1, 1, 1, 1, NA, NA))

  if(draw) gridExtra::grid.arrange(plot, k, layout_matrix = lay)
  else gridExtra::arrangeGrob(plot, k, layout_matrix = lay)
}
