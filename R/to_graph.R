#' @describeIn to_ to_
#' 
#' @export
#' @examples
#' g <- igraph::graph.full(10)
#' g2 <- to_graph(g)
to_graph <- function(g,
                     ...){
  if(methods::is(g,"tbl_graph")){
   return(g) 
  }else if(methods::is(g,"igraph")){
    g <- tidygraph::as_tbl_graph(g,...)
  } else if(methods::is(g,"data.table")){
    g <- dt_to_graph(g,...)
  }else{
    stop("Unknown input type.")
  }
}
