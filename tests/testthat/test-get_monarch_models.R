test_that("get_monarch_models works", {

  models <- get_monarch_models()
  testthat::expect_true(methods::is(models,"data.table"))
  testthat::expect_gte(sum(!is.na(models$disease_id)), 41000)
  testthat::expect_gte(length(unique(models$subject_taxon_label)), 21)
})
