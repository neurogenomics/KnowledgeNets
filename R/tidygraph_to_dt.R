#' @describeIn convert_ convert_
tidygraph_to_dt <- function(g,
                            what=c("nodes","edges")){
  if(all(c("nodes","edges") %in% what)){
    as.data.table(g)
  }
  what <- match.arg(what)
  g |> tidygraph::activate(!!what) |>
    data.table::as.data.table()
}