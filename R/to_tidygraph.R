#' @describeIn convert_ convert_
#' 
#' @export
#' @examples
#' g <- igraph::graph.full(10)
#' g2 <- to_tidygraph(g)
to_tidygraph <- function(g,
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