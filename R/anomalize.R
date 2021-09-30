#' Anomalize Datasets
#'
#' Redefine data points as how far away they are from the average for that point in space, a useful tool for better visualization and analysis
#'
#' @param starsObj a stars object
#' @param attr attribute of stars object
#' @param space1 spatial dimension (e.g., station, Longitude)
#' @param space2 second spatial dimension, if applicable (e.g., Latitude)
#' @param time time dimension
#' @param baseline optional parameter that chooses which time period to calculate anomalies for (e.g., 1951-1980)
#' @param ... other dimensions not involved in anomalization, but still part of the original stars object
#'
#' @return a stars object with anomalized attribute
#' @export
#'
#' @examples
#' anomalize(GISTEMP,1,1,2,3)
#' anomalize(GISTEMP,1,"Long","Lat","Time", baseline = 1951:1980)
anomalize = function(starsObj, attr, space1, space2 = NULL, time, baseline = NULL, ...){
  x = stars::st_get_dimension_values(starsObj, space1)

  # need to redefine baseline a bit for subsetting
  if(base::is.null(baseline)) baseline = 1:(base::length(stars::st_get_dimension_values(starsObj,time)))
  else{
    z = stars::st_get_dimension_values(starsObj, time)
    baseline = base::which(z %in% baseline)
  }

  # Say the data is on a station grid (i.e., only one spatial dimension)
  if(base::is.null(space2)){
    starsObj = base::aperm(starsObj, c(space1, time, ...))
    anoms = base::rep(0, base::length(x))
    i = 1
    for(i1 in x){
      array = starsObj[attr, i, baseline][[1]]
      avg = base::mean(array, na.rm = TRUE)
      if(!base::is.null(avg)){
        anoms[i] = avg
      }else{
        anoms[i] = NA
      }
      i = i + 1
    }
  }

  i = j = 1
  # Say Long and Lat are provided
  if(!base::is.null(space2)){
    y = stars::st_get_dimension_values(starsObj, space2)
    anoms = base::array(dim = c(
      base::length(x),
      base::length(y)
    ))
    starsObj = base::aperm(starsObj, c(space1,space2,time,...))
    for(i1 in x){
      j = 1
      for(j1 in y){
        array = starsObj[attr, i, j, baseline][[1]]

        anoms[i,j] = base::mean(array, na.rm = TRUE)

        j = j + 1
      }
      i = i + 1
    }
  }

  # we need to rely a bit on R's Vector Recycling Rules
  # is this the best way?
  starsObj[["Anomalized"]] = units:::set_units(starsObj[[attr]],NULL) - base::as.vector(anoms)
  return(starsObj)
}
