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