#' @describeIn plot_ plot_
#' @inheritDotParams base::plot
#' @returns Null 
#' @keywords internal
plot_ontology_tidygraph <- function(ont,
                                 ...){
  g <- ontology_to_graph(ont)
  plot(g,...)
}
