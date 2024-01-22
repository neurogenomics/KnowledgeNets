test_that("map_upheno works", {
  
  run_tests <- function(res){
    testthat::expect_true(methods::is(res$data,"data.table"))
    for(x in res$plots){
      testthat::expect_true(methods::is(x,"gg") ||
                              methods::is(x,"Heatmap"))
    }
  }
  terms <- get_ontology('hp')
  
  res <- map_upheno(force_new = FALSE,
                    terms = terms)
  run_tests(res)
  
  #### Use cached data and filter by HPO terms
  res <- map_upheno(force_new = FALSE,
                    terms = terms)
  run_tests(res)
})
