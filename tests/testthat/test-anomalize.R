test_that("anomalized data isn't wonky", {
  anoms = anomalize(GISTEMP,1,1,2,3)
  lon = base::sample(1:base::length(stars::st_get_dimension_values(GISTEMP,1)), 1)
  lat = base::sample(1:base::length(stars::st_get_dimension_values(GISTEMP,2)), 1)

  val = GISTEMP[1,lon,lat,][[1]] - base::mean(GISTEMP[1,lon,lat,][[1]], na.rm = TRUE)
  val = units:::set_units(val, NULL)
  anom1 = anoms["Anomalized",lon,lat,][[1]]

  expect_equal(val, anom1)


  anoms1 = anomalize(tmax,attr=1,space1=1,time=2)
  station = base::sample(1:base::length(stars::st_get_dimension_values(tmax,1)), 1)

  val1 = tmax[1,station,][[1]] - base::mean(tmax[1,station,][[1]], na.rm = TRUE)
  anom2 = anoms1["Anomalized",station,][[1]]

  expect_equal(val1,anom2)


  rm(list = ls())
})
