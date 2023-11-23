test_that("throw errors and warnings", {
  expect_error(TerraclimateR:::get_terraclim_single(lat = "a", lon = 4, clim_var = "tmin"),
               "latitude and longitude must be numeric")
  expect_warning(TerraclimateR:::get_terraclim_single(lat = NA, lon = 4, clim_var = "tmin"),
                 "latitude and/or longitude missing, returning NA")
  expect_error(TerraclimateR:::get_terraclim_single(lat = 1, lon = 4, clim_var = 2),
               "clim_var must to be of mode character")
  expect_error(TerraclimateR:::get_terraclim_single(lat = 1, lon = 4, clim_var = "tmin", nc = 3),
               "is not TRUE")
  expect_warning(TerraclimateR:::get_terraclim_single(lat = 12, lon = 22, clim_var = "tmin"),
                 paste("ambiguous matches for coordinates\n",
                       "longituge :", 22, "\n",
                       "latitude  :", NULL, "\n",
                       "calculating mean of equally distant data points"))
})

test_that("expected results", {
  expect_null(attr(TerraclimateR:::get_terraclim_single(lat = 1.24, lon = 4.21, clim_var = "tmin"),
                   "latitude"))
  expect_equal(sum(TerraclimateR:::get_terraclim_single(lon = 11.960909,
                                                        lat = 51.489191,
                                                        clim_var = "def")[1:42]),
               663.9)
})
