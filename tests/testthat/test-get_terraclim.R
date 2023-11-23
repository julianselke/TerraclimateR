df <- data.frame(id = c("Vilcabamba", "Paracas", "Puerto Viejo", "Trinidad", "Cartagena"),
                 lat = c(-4.260641, -13.833881, 9.656562, 21.808230, 10.172151),
                 lon = c(-79.22274, -76.25046, -82.75600, -79.98102, -75.74658))

test_that("throw errors and warnings", {
  expect_error(TerraclimateR::get_terraclim(df, "id", "lat", "lon", "PDS"),
               "no matching clim_vars provided")
  expect_error(TerraclimateR::get_terraclim(df, "id", "lat", "lon", c("PDSI", "PDS", "PD", "P")),
               "invalid clim_vars provided: PDS, PD, P")
  expect_error(TerraclimateR::get_terraclim(df, "id", "lat", 2, c("PDSI")),
               "all \\*_var arguments must be of type character")
  expect_error(TerraclimateR::get_terraclim(1, "id", "lat", "lon", c("PDSI")),
               "invalid class of df: numeric\nobject of class data.frame or matrix needed")
  expect_error(TerraclimateR::get_terraclim(df, "XXX", "lat", "lon", c("PDSI")),
               "column\\(s\\) XXX not found")
})

nc_df <- TerraclimateR::get_terraclim(df, "id", "lat", "lon", c("PDSI"))

test_that("expected results", {
  expect_true(all(dim(nc_df) == c(3900, 5)))
  expect_true(all(levels(nd_df$month) == month.abb))
  expect_equal(sum(nc_df$value[1:100]), -10.85)
  expect_equal(sum(is.na(nc_df)), 0)
})
