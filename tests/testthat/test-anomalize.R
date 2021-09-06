library(densityviz)

test_that("anomalized data isn't wonky", {
  anoms <- anomalize(GISTEMP,1,1,2,3)
  lon <- sample(1:length(st_get_dimension_values(GISTEMP,1)), 1)
  lat <- sample(1:length(st_get_dimension_values(GISTEMP,2)), 1)

  val <- GISTEMP[1,lon,lat,][[1]] - mean(GISTEMP[1,lon,lat,][[1]], na.rm = TRUE)
  val <- set_units(val, NULL)
  anom1 <- anoms[1,lon,lat,][[1]]

  expect_equal(val, anom1)


})
