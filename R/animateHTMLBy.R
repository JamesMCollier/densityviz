#' Animate Densities by Dimension Values
#'
#' @param starsObj stars object
#' @param attr attribute of interest
#' @param dim grouping dimension
#' @param title plot title
#' @param anim_range if specified, dimension values along which to animate
#' @param ... arguments passed to plotDensityByDimensionValue()
#'
#' @import animation
#' @import rnaturalearth
#' @import stars
#'
#' @export
#'
#' @examples
#' # Animation example of the GISTEMP dataset
#' animateHTMLBy(GISTEMP, 1, 3, "Australia", anim_range = 1981:2020,
#'   baseline = c(1951,1980), sf = ne_countries(country = "Australia", returnclass = "sf"))
animateHTMLBy <- function(starsObj, attr, dim, title, anim_range = NULL, ...){

  ani.options(interval = .4)

  saveHTML(anim_factory(starsObj, attr, dim, title, anim_range, ...),
           autoplay = FALSE,
           loop = FALSE,
           verbose = FALSE,
           outdir = ".",
           htmlfile = "index.html")
}


#' @rdname animateHTMLBy
#' @export
anim_factory <- function(starsObj, attr, dim, title, anim_range = NULL, ...){

  if(!is.null(anim_range)){
    for(i in anim_range){
      print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
    }
  }else{
    for(i in unique(st_get_dimension_values(starsObj, dim))){
        print(plotDensityByDimensionValue(starsObj, attr, dim, i, title, ...))
    }
  }
}

