test_that("clear_cache works", {
  
  
  f <- 
  testthat::expect_true(file.exists(f))
  clear_cache()
  testthat::expect_false(file.exists(f))
})
