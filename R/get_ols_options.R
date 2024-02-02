#' @describeIn get_ get_
#' Get a complete up=to-date list of ontologies available via the
#' \href{https://www.ebi.ac.uk/ols4}{EBML-EBI Ontology Lookup Service} API.
#' @param ol An \link[rols]{Ontologies} object.
#' @export
get_ols_options <- function(ol = rols::Ontologies()){
  sort(rols::olsNamespace(ol))
}