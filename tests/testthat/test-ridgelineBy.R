library(densityviz)

test_that("correct ridgeline plots are produced", {
  # use vdiffr to monitor appearance of ridgeline plots produced via
  # testthat snapshots
  # if any changes to the plot are detected, a failure will occur
  expect_doppelganger("Ridgeline", ridgelineBy(GISTEMP_Decadal, 1, 3, "Ridgeline", y_reverse = TRUE,
                                                       breaks = c(1970, 1980, 1990, 2000, 2010)))
  expect_doppelganger("Australia Ridgeline",
                      ridgelineBy(GISTEMP_Decadal[ne_countries(country = "Australia", returnclass = "sf")],
                                  1,3, "Australia Ridgeline", breaks = seq(1970, 2010, 10)))

})
