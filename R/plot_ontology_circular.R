#' @describeIn plot_ plot_
#' @inheritDotParams simona::dag_circular_viz
#' @returns Null
#' @keywords internal
plot_ontology_circular <- function(ont,
                                   ...){
  requireNamespace("DiagrammeR")
  simona::dag_circular_viz(ont,
                           ...) 
}
