#' st_findYMax
#'
#' explanation
#'
#' @param data data
#'
#' @import plyr
#'
#'
st_findYMax <- function(data){
  freqs <- count(data$bothVars)$freq
  ymax <- (max(freqs)/nrow(data)) + 0.05
  ymax
}

