#' calcbin
#'
#' Code extracted from Vizumap to compute the bins from which
#' the 3x3 grid is produced.
#'
#' Permission granted from authors under GPL
#'
calcbin <- function (terciles, data, x, q, bin, width, min)
{
  estbin <- ifelse(terciles, round(quantile(data[, x], q),
                                   2), round(bin * width + min, 2))
  estbin
}
