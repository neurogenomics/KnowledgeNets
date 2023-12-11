#' @describeIn plot_ plot_
#' @inheritDotParams simona::dag_graphviz
#' @returns Null 
#' @keywords internal
plot_ontology_visnetwork <- function(ont,
                                     ...){
  
  g <- ontology_to_tidygraph(ont)
  vn <- visNetwork::visIgraph(g,...) |>
    visNetwork::visNodes(shape = "dot", 
                         label = "name",
                         size = 10,
                         font = list(size = 10)) |>
    visNetwork::visInteraction(hover = TRUE)
  methods::show(vn)
  return(vn)
}
