library(densityviz)


test_that("correct plots are produced", {
  expect_doppelganger("Density Plot", plotDensityByDimensionValue(GISTEMP, 1, 3, 2010, "anomaly density"))

  expect_doppelganger("Another Density Plot", plotDensityByDimensionValue(GISTEMP, 1, 3, 1999, "anomaly density",
                              baseline = c(1940,1970)))
  expect_doppelganger("Last Plot", plotDensityByDimensionValue(GISTEMP, 1, 3, 1980, "Australia anomaly density",
                              sf = ne_countries(country = "Australia", returnclass = "sf")))
})
