test_that("cache_clear works", {
  
  f <- file.path(cache_dir(),basename(tempfile()))
  data.table::fwrite(data.table::data.table(x=1:10),f)
  testthat::expect_true(file.exists(f))
  cache_clear()
  testthat::expect_false(file.exists(f))
})
