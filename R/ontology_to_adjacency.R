#' @describeIn convert_ convert_
#' Adjacency matrix
#'
#' Create adjacency matrix of HPO child-parent relationships. 
#' @inheritParams filter_
#' @returns Adjacency matrix
#'
#' @export
#' @examples
#' ont <- get_ontology()
#' adj <- ontology_to_adjacency(ont)
ontology_to_adjacency <- function(ont,
                                  terms = ont@terms,
                                  remove_terms=grep(":",terms,
                                                    invert = TRUE,
                                                    value = TRUE)
                                  ) {
  ont <- filter_ontology(ont,
                         terms = terms,
                         remove_terms = remove_terms)
  g <- ontology_to_graph(ont)
  adj <- igraph::as_adj(g)
  return(adj)
}
