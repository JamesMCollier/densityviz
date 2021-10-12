#' Plot bivariate spatio-temporal data
#'
#' @param starsObj1 a stars object
#' @param attr1 which attribute of starsObj1 should be plotted?
#' @param starsObj2 a stars object
#' @param attr2 which attribute of starsObj2 should be plotted?
#' @param terciles should terciles be calculated?
#' @param bounds if custom bounds should be plotted, default NULL
#' @param ymax if custom maximum y-axis value is desired, default NULL
#' @param title title of plot
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' st_bivariate_histogram(tmax,1,tmin,1,FALSE, title = "Max/Min temperature distribution")
st_bivariate_histogram = function(starsObj1, attr1, starsObj2, attr2, terciles, bounds = NULL, ymax = NULL, title){
  data = st_assign_colors(starsObj1, attr1, starsObj2, attr2, terciles, bounds)

  # help with bar chart levels found at https://stackoverflow.com/questions/10834382/ggplot2-keep-unused-levels-barplot
  hist = ggplot2::ggplot(data, ggplot2::aes_string(x = 'bothVars', fill = 'hex_code')) +
    ggplot2::geom_bar(ggplot2::aes(y = ..count../sum(..count..)), position = "dodge") +
    ggplot2::scale_fill_identity(drop=FALSE) +
    ggplot2::scale_x_discrete(drop=FALSE) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 6)
    ) + ggplot2::ggtitle(title)

  if(!base::is.null(ymax)) hist = hist + ggplot2::ylim(c(0,ymax))

  hist
}

#' Create multiple plots of bivariate spatio-temporal data along chosen dimension values
#'
#' @param starsObj1 a stars object
#' @param attr1 which attribute of starsObj1 should be plotted?
#' @param starsObj2 a stars object
#' @param attr2 which attribute of starsObj2 should be plotted?
#' @param dim_values the dimension values along which to create plots. Should be a list. Note that the actual indices of the dimension values are needed, see examples
#' @param terciles should terciles be calculated?
#' @param which which dimension is the one of interest? Should take a value between 1 and 3 (inclusive), use aperm() if need be
#' @param titles titles of the plots, should be of the same length as dim_values
#' @param bounds if custom bounds should be plotted, default NULL
#'
#' @return
#' @export
#'
#' @examples
#' time = as.numeric(stars::st_get_dimension_values(tmax,2))
#' decades = list()
#' j=1
#' for(i in list(c(1950:1959),c(1960:1969),c(1970:1979),c(1980:1989),c(1990:1999))){
#'   decades[[j]] = which(time %in% i)
#'   j = j + 1
#' }
#' histograms = st_multiple_histograms(tmax,1,tmin,1,dim_values=decades,which=2,titles=c(1:5))
#' attach_key(gridExtra::arrangeGrob(histograms[[1]],histograms[[5]],ncol=1), key)
st_multiple_histograms = function(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, which = 1, titles, bounds = NULL){
  if(base::length(titles) != base::length(dim_values)){
    stop("Please ensure 'titles' and 'dim_values' are the same length")
  }
  data = st_assign_colors(starsObj1, attr1, starsObj2, attr2, terciles, bounds)
  if(base::is.null(bounds)) bounds = st_findNbounds(data, terciles)
  ymax = st_findYMax(data)

  st_generate_plotsList(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, bounds, ymax, which = which, titles = titles)
}

st_generate_plotsList = function(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, bounds, ymax, which, titles){
  plotsList = base::list()

  if(which == 1){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,j]
      stars2 = starsObj2[,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2, terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }else if(which == 2){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,,j]
      stars2 = starsObj2[,,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2, terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }else if(which == 3){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,,,j]
      stars2 = starsObj2[,,,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2, terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }

  plotsList
}

st_findYMax = function(data){
  freqs = plyr::count(data$bothVars)$freq
  ymax = (base::max(freqs)/base::nrow(data)) + 0.05
  ymax
}

