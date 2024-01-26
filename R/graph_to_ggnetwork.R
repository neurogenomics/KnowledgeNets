#' @describeIn to_ to_
#' 
#' @inheritDotParams ggnetwork::fortify
#' @export
#' @examples
#' g <- igraph::graph.atlas(10)
#' ggn <- graph_to_ggnetwork(g)
graph_to_ggnetwork <- function(g,
                               ...){
  requireNamespace("ggnetwork")
  # n <- intergraph::asNetwork(g)
  messager("Converting graph to ggnetwork.")
  g |>
    igraph::simplify() |>
    ggnetwork::fortify(...)
}
