test_that("filter_ontology works", {
  
  ont <- get_ontology("hp")
  testthat::expect_gte(ont@n_terms,17000)
  
  ont2 <- filter_ontology(ont,terms=c("HP:0000001","HP:0000002"))
  testthat::expect_gte(ont2@n_terms,4)
  
  ont3 <- filter_ontology(ont,terms=100)
  testthat::expect_gte(ont3@n_terms,500)
  
  ont4 <- filter_ontology(ont,
                          keep_descendants=c("Abnormality of the nervous system"))
  testthat::expect_gte(ont4@n_terms,2600)
  
  ont5 <- filter_ontology(ont,
                          keep_descendants="Mode of inheritance")
  testthat::expect_gte(ont5@n_terms,40)
})
