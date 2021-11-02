#' st_bivariate_histogram
#'
#' Plot bivariate spatio-temporal data
#'
#' @param data data frame containing data from two attributes, the binned
#'             information and colour code.
#' @param terciles should terciles be calculated?
#' @param ymax if custom maximum y-axis value is desired, default NULL
#' @param title title of plot
#'
#' @details Assistance for the creation of bar chart levels was found at
#' https://stackoverflow.com/questions/10834382/ggplot2-keep-unused-levels-barplot
#'
#' @import ggplot2
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' # Preparing the data
#' out <- st_assign_colors(tmax, 1, tmin, 1, terciles = FALSE)
#' data <- out$data
#' colors <- out$colors
#' bounds <- out$bounds
#'
#' # generating and plotting the histogram
#' bvh <- st_bivariate_histogram(data = data,
#'             title = "Max/Min temperature distribution")
#' bvh
#'
#' # creating and attaching the key
#' key <- Vizumap::build_bkey(data, colors, terciles = TRUE)
#' Vizumap::view(key)
#' attach_key(bvh, key)
#'
st_bivariate_histogram <- function(data, ymax = NULL, title){

  histobj <- ggplot(data, aes_string(x = 'bothVars', fill = 'hex_code')) +
    geom_bar(aes(y = ..count../sum(..count..)), position = "dodge") +
    scale_fill_identity(drop = FALSE) +
    scale_x_discrete(drop = FALSE) +
    theme(axis.text = element_text(size = 6)) +
    ggtitle(title)

  if(!is.null(ymax)) histobj <- histobj + ylim(c(0,ymax))

  histobj
}

