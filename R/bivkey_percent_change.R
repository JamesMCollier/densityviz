#' bivkey_percent_change
#'
#' Plot change in bivariate space time data over time, built on keys from Vizumap::build_bkey
#'
#' @param data.primary the data for which change in bins (from data.base levels) should be calculated.
#' Data built using st_assign_colors.
#' @param data.base the data upon which the change in data.primary will be calculated. It is usually
#' further in the past than data.primary. Build using st_assign_colors.
#' @param key a key build using Vizumap::build_bkey. Ensure data has same bounds as key, either with
#' st_change_key_bounds or building data in st_assign_colors with custom bounds
#'
#' @return draws on the  plot window
#' @export
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' library(stars)
#' library(Vizumap)
#' library(gridExtra)
#' palette = build_palette(name = "BlueRed")
#' # matched time to decadal time periods and produce histograms
#' time <- as.numeric(st_get_dimension_values(tmax, 2))
#' periods <- list(c(1950:1959), c(1960:1969), c(1970:1979), c(1980:1989),c(1990:1999))
#' decades <- lapply(periods, function(x, time) which(time %in% x), time)
#'
#' # Find bounds on whole data set
#' out = st_assign_colors(tmax, 1, tmin, 1, FALSE, palette = palette)
#' bounds = out$bounds
#'
#' # Preparing the data
#' out2 <- st_assign_colors(tmax[,,decades[[5]]], 1, tmin[,,decades[[5]]], 1, terciles = FALSE,
#' palette = palette, bounds = bounds)
#' data2 <- out2$data
#' out1 <- st_assign_colors(tmax[,,decades[[1]]], 1, tmin[,,decades[[1]]], 1, terciles = FALSE,
#' palette = palette, bounds = bounds)
#' data1 <- out1$data
#'
#' # Build key
#' colnames(data1) = c("Tmax","Tmin","bothVars","hex_code")
#' colnames(data2) = c("Tmax","Tmin","bothVars","hex_code")
#' key <- build_bkey(data1, palette, terciles = FALSE)
#' key = st_change_key_bounds(key, bounds)
#'
#' bivkey_percent_change(data.primary = data2, data.base = data1, key)
bivkey_percent_change = function(data.primary, data.base, key, factor = bothVars){
  factor = enquo(factor)
  if(!all.equal(levels(data.primary$bothVars), levels(data.base$bothVars))){
    stop("Please ensure the data objects have the same bounds (use st_assign_colors with custom bounds)")
  }

  tiles = data.frame(x = key$tiles$x, y = key$tiles$y, group = key$tiles$group)
  by_group = tiles %>% group_by(group) %>% dplyr::summarise(centroidx = mean(x), centroidy = mean(y))
  by_group$bounds = levels(data.base$bothVars)
  by_group$percOfObs = dplyr::count(data.base, !!factor, .drop = FALSE)$n / nrow(data.base)
  by_group$percChange = (dplyr::count(data.primary, !!factor, .drop = FALSE)$n / nrow(data.primary)) - by_group$percOfObs
  by_group$percChangeString = unlist(lapply(by_group$percChange, generate_percChange_string))
  by_group$percChangeColor = unlist(lapply(by_group$percChange, generate_percChange_colors))

  key.primary = view(key) + geom_label(data=by_group, size=6, aes(x = centroidx, y = centroidy,
                            label = percChangeString, color = percChangeColor, fontface = "bold")) +
    theme(legend.position = "none") + scale_color_identity()

  key.base = view(key) + geom_label(data=by_group, size=6, aes(x = centroidx, y = centroidy,
                                    label = paste0(100*round(percOfObs,4),'%'), fontface = "bold"))

  grid.arrange(key.base, key.primary, ncol=1)
}

generate_percChange_string = function(percChange){
  if(percChange < 0){
    percChange = paste0(100*round(percChange,4),"%")
  }else{
    percChange = paste0("+",100*round(percChange,4),"%")
  }
}

generate_percChange_colors = function(percChange){
  if(percChange < 0){
    color = "red"
  }else{
    color = "green"
  }
}
