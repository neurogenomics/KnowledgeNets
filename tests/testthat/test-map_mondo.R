test_that("map_mondo works", {
  
  dat <- example_dat()
  dat2 <- map_mondo(dat = dat)
  testthat::expect_true(methods::is(dat2,"data.table"))
})
