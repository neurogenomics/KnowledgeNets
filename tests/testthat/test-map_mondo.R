skip_if_offline()
test_that("map_mondo works", {

  dat <- example_dat(rm_types="gene")
  dat2 <- map_mondo(dat = dat, map_to = "hpo")
  testthat::expect_true(methods::is(dat2,"data.table"))
  testthat::expect_gte(nrow(dat2),nrow(dat))
  testthat::expect_true(all(dat2$from %in% dat$from))
  testthat::expect_lte(sum(is.na(dat2$mondo_id)),4)
})
