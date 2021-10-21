st_generate_plotsList <- function(starsObj1, attr1, starsObj2, attr2, dim_values,
                                  terciles, bounds, ymax, which, titles){
  plotsList = list()

  if(which == 1){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,j]
      stars2 = starsObj2[,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2,
                                              terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }else if(which == 2){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,,j]
      stars2 = starsObj2[,,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2,
                                              terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }else if(which == 3){
    i = 1
    for(j in dim_values){
      stars1 = starsObj1[,,,j]
      stars2 = starsObj2[,,,j]
      plotsList[[i]] = st_bivariate_histogram(stars1, attr1, stars2, attr2,
                                              terciles, bounds, ymax, titles[i])
      i = i + 1
    }

  }

  plotsList
}
