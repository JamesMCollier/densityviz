st_bivariate_histogram = function(starsObj1, attr1, starsObj2, attr2, terciles, bounds = NULL, ymax = NULL, title){
  data = st_assign_colors(starsObj1, attr1, starsObj2, attr2, terciles, bounds)

  # help with bar chart levels found at https://stackoverflow.com/questions/10834382/ggplot2-keep-unused-levels-barplot
  hist = ggplot(data, aes_string(x = 'bothVars', fill = 'hex_code')) +
    geom_bar(aes(y = ..count../sum(..count..)), position = "dodge") +
    scale_fill_identity(drop=FALSE) +
    scale_x_discrete(drop=FALSE) +
    theme(
      axis.text = element_text(size = 6)
    ) + ggtitle(title)

  if(!is.null(ymax)) hist = hist + ylim(c(0,ymax))

  hist
}

st_multiple_histograms = function(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, which = 1, titles, bounds = NULL){
  if(length(titles) != length(dim_values)){
    stop("Please ensure 'titles' and 'dim_values' are the same length")
  }
  ymax = st_findYMax(starsObj1, attr1, starsObj2, attr2, terciles)
  if(is.null(bounds)) bounds = st_findNbounds(starsObj1, attr1, starsObj2, attr2, terciles)

  st_generate_plotsList(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, bounds, ymax, which = which, titles = titles)
}

st_generate_plotsList = function(starsObj1, attr1, starsObj2, attr2, dim_values, terciles, bounds, ymax, which, titles){
  plotsList = list()

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

st_findYMax = function(starsObj1, attrOf1, starsObj2, attrOf2, terciles){
  data = st_assign_colors(starsObj1, attrOf1, starsObj2, attrOf2, terciles)
  freqs = plyr::count(data$bothVars)$freq
  ymax = (max(freqs)/nrow(data)) + 0.05
  ymax
}

