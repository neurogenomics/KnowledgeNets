#' @describeIn filter_ filter_
#' Filter a tbl_graph.
#' @export
filter_graph <- function(g,
                         filters){
  for(nm in names(filters)){
    f <- filters[[nm]]
    if(is.null(f)){
      next
    }
    ## filter just that column in the nodes of the graph
    g <- g|> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(get(eval(nm)) %in% f)
  }
  return(g)
}