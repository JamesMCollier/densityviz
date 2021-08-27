## code to prepare `GISTEMP_Decadal` dataset goes here
library(stars)
library(dplyr)
library(zoo)

round_any = function(x, accuracy, f=round){
  f(x/ accuracy) * accuracy
} # round_any found at https://stackoverflow.com/questions/43627679/round-any-equivalent-for-dplyr/46489816#46489816

z = read_stars("data-raw/gistemp250_GHCNv4.nc")

st_crs(z) = st_crs(rnaturalearth::ne_countries())

setNames(z, "Temperature Anomalies degC") %>%
  st_set_dimensions(names = c("Long", "Lat", "Time")) -> z

z = st_set_dimensions(z,3, values = as.Date(st_get_dimension_values(z,3)))
z

z1 = st_set_dimensions(z,3,values = as.numeric(format(st_get_dimension_values(z,3), "%Y")))
z1 = filter(z1, between(Time, 1971, 2020))

GISTEMP_Decadal = st_set_dimensions(z1,3,values = round_any(st_get_dimension_values(z1,3)-1, 10, f=floor))
st_get_dimension_values(GISTEMP_Decadal,3)
GISTEMP_Decadal

usethis::use_data(GISTEMP_Decadal, overwrite = TRUE)
