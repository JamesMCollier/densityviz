attach_key = function(plot, VizuKey, draw = TRUE){
  k <- Vizumap::view(VizuKey)


  lay <- rbind(c(1, 1, 1, 1, NA, NA),
               c(1, 1, 1, 1, 2, 2),
               c(1, 1, 1, 1, 2, 2),
               c(1, 1, 1, 1, NA, NA))

  if(draw) gridExtra::grid.arrange(plot, k, layout_matrix = lay)
  else gridExtra::arrangeGrob(plot, k, layout_matrix = lay)
}
