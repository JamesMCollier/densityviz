## code to prepare `GISTEMP` dataset goes here
library(stars)
library(dplyr)
library(zoo)

z = read_stars("data-raw/gistemp250_GHCNv4.nc")

st_crs(z) = st_crs(rnaturalearth::ne_countries())

setNames(z, "Temperature Anomalies degC") %>%
  st_set_dimensions(names = c("Long", "Lat", "Time")) -> z

z = st_set_dimensions(z,3, values = as.Date(st_get_dimension_values(z,3)))
z
# it would be better for analysis to index Time dimension by year
GISTEMP = st_set_dimensions(z,3,values = as.numeric(format(st_get_dimension_values(z,3), "%Y")))
GISTEMP = filter(GISTEMP, between(Time, 1970, 2020))
usethis::use_data(GISTEMP, overwrite = TRUE)

