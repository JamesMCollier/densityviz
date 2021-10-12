## code to prepare `tmax-prep` dataset goes here
## data set found at http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.DAILY/.FSOD/
library(dplyr)
tmax = stars::read_stars("data-raw/tmax.nc")
tmax = tmax %>% stars::st_set_dimensions(names = c("Time", "Station"))
tmax = base::aperm(tmax, c(2,1))
tmax = tmax %>% stars::st_set_dimensions(2, values = base::format(
  base::as.Date(base::floor(stars::st_get_dimension_values(tmax,2)), origin = "1949-12-31"), "%Y"))
tmax

usethis::use_data(tmax, overwrite = TRUE)
