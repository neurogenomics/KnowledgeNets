#' @describeIn convert_ convert_
#' 
#' @export
#' @examples
#' g <- igraph::graph.atlas(10)
#' ggn <- graph_to_ggnetwork(g)
graph_to_ggnetwork <- function(g){
  g |>
    igraph::simplify() |>
    ggnetwork::fortify()
}
