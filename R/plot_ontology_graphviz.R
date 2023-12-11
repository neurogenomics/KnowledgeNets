#' @describeIn plot_ plot_
#' Plot ontology: graphviz
#' 
#' Make a circular plot of an ontology. 
#' @inheritDotParams simona::dag_graphviz
#' @returns Null
#' 
#' @keywords internal
plot_ontology_graphviz <- function(ont,
                                   ...){
  simona::dag_graphviz(ont,
                       ...) 
}