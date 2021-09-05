test_that("anomalized data isn't wonky", {
  anoms = anomalize(GISTEMP,1,1,2,3)
  lon = base::sample(1:base::length(stars::st_get_dimension_values(GISTEMP,1)), 1)
  lat = base::sample(1:base::length(stars::st_get_dimension_values(GISTEMP,2)), 1)

  val = GISTEMP[1,lon,lat,][[1]] - base::mean(GISTEMP[1,lon,lat,][[1]], na.rm = TRUE)
  val = units:::set_units(val, NULL)
  anom1 = anoms[1,lon,lat,][[1]]

  expect_equal(val, anom1)
  rm(list = ls())
})
