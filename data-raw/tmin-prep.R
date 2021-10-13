## code to prepare `tmin-prep` dataset goes here
## data set found at http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.DAILY/.FSOD/
library(dplyr)
tmin = stars::read_stars("data-raw/tmin.nc")
tmin = tmin %>% stars::st_set_dimensions(names = c("Time", "Station"))
tmin = base::aperm(tmin, c(2,1))
tmin = tmin %>% stars::st_set_dimensions(2, values = base::format(
  base::as.Date(base::floor(stars::st_get_dimension_values(tmin,2)), origin = "1949-12-31"), "%Y"))
tmin

usethis::use_data(tmin, overwrite = TRUE)
