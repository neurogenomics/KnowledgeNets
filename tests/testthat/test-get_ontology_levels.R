test_that("get_ontology_levels works", {

  hpo <- get_ontology("hp")
  parents <- grep("^HP",unique(hpo@terms), value = TRUE)[seq(2)]
  childs <- simona::dag_children(hpo, term = parents)

  terms1 <- c(parents,childs)
  #### Using only immediate children ####
  lvls1 <- KGExplorer::get_ontology_levels(ont=hpo,
                                           terms = terms1,
                                           absolute = TRUE)
  testthat::expect_length(lvls1, length(terms1))
  testthat::expect_false(diff(range(lvls1)) == 0)

  #### Using all descendants ####
  terms2 <- simona::dag_offspring(hpo,
                                  term= parents[[2]])
  lvls2 <- KGExplorer::get_ontology_levels(ont=hpo,
                                           terms = terms2)
  testthat::expect_length(lvls2, length(terms2))
  testthat::expect_false(diff(range(lvls2)) == 0)

  #### Using relative levels ####
  lvls3 <- get_ontology_levels(ont=hpo,
                               terms = terms1,
                               absolute = FALSE)
  testthat::expect_length(lvls3, length(terms1))
  testthat::expect_false(diff(range(lvls3)) == 0)

  lvls4 <- get_ontology_levels(ont=hpo,
                               terms = terms1,
                               absolute = TRUE,
                               reverse = FALSE)
  testthat::expect_length(lvls4, length(terms1))
  testthat::expect_false(diff(range(lvls4)) == 0)

  #### Visual confirmation of correct hierarchy ####
  # ontologyPlot::onto_plot(ontology = hpo,
  #                         label = terms1,
  #                         terms = terms1)
  # ontologyPlot::onto_plot(ontology = hpo,
  #                         label = terms2,
  #                         terms = terms2)
})
