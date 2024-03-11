test_that("map_ontology_terms works", {

  ont <- get_ontology("hp")
  terms <- c("Focal motor seizure",
              "Focal MotoR SEIzure",
              "HP:0000002","HP:0000003")
  term_names <- map_ontology_terms(ont=ont, terms=terms)
  term_ids <- map_ontology_terms(ont=ont, terms=terms, to="id")
  
  
  testthat::expect_true(
    length(term_names)==length(terms)
  )
  testthat::expect_true(
    length(term_ids)==length(terms)
  )
  testthat::expect_true(
    all(names(term_names)==terms)
  ) 
  testthat::expect_true(
    all(names(term_ids)==terms)
  ) 
})
