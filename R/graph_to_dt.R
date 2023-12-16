#' @describeIn convert_ convert_
#' @export
#' @examples
#' g <- igraph::graph.atlas(10)
#' dat <- graph_to_dt(g)
graph_to_dt <- function(g,
                        what=c("nodes","edges"),
                        id_col=c("id","name")[1]){
  
  if(methods::is(g,"igraph")){
    messager("Converting igraph to tidygraph.")
    g <- tidygraph::as_tbl_graph(g)
  }
  if(all(c("nodes","edges") %in% what)){
    messager("Converting tidygraph to data.table.")
    tmp <- igraph::as_data_frame(g,what="both")  
    #### Convert to character ####
    if(is.numeric(tmp$edges$from)){
      tmp$edges$from <- tmp$vertices[[id_col]][tmp$edges$from]
    }
    if(is.numeric(tmp$edges$to)){
      tmp$edges$to <- tmp$vertices[[id_col]][tmp$edges$to]
    }
    ## merge edges and vertices 
    obj <- merge(tmp$edges, 
                 tmp$vertices,
                 by.x="from", 
                 by.y=id_col) |>
      merge(tmp$vertices,
            by.x="to",
            by.y=id_col,
            suffixes = c(".from",".to")) |> 
      data.table::as.data.table()
    return(obj)
  } else if(length(what)==1){
    g |> 
      tidygraph::activate(!!what) |>
      data.table::as.data.table()
  }
}
