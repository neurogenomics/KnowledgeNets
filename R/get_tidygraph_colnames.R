get_tidygraph_colnames <- function(g,
                                   what=c("nodes","edges")){
  what <- match.arg(what)
  g |>
    tidygraph::activate(what=!!what) |>
    tidygraph::as_tibble() |>
    colnames()
}