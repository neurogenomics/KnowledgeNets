get_graph_colnames <- function(g,
                               what=c("nodes","edges")){
  g <- to_tidygraph(g)
  what <- match.arg(what)
  g |>
    tidygraph::activate(what=!!what) |>
    tidygraph::as_tibble() |>
    colnames()
}