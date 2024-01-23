#' @describeIn get_ get_
#' Get column names in the nodes and/or edges of a tbl_graph.
#' @inheritParams tidygraph::activate
#' @export
get_graph_colnames <- function(g,
                               what=c("nodes","edges")){
  if(methods::is(g,"data.frame")) return(names(g))
  g <- to_graph(g)
  what <- match.arg(what)
  g |>
    tidygraph::activate(what=!!what) |>
    tidygraph::as_tibble() |>
    colnames()
}