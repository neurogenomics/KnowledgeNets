#' @describeIn convert_ convert_
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' g <- ontology_to(ont, to="tbl_graph")
#' dat <- graph_to_dt(g)
graph_to_dt <- function(g,
                        what=c("nodes","edges"),
                        id_col=c("id","name")){
  #### Conver tot tbl_graph ####
  if(methods::is(g,"igraph")){
    messager("Converting igraph to tbl_graph.")
    g <- tidygraph::as_tbl_graph(g)
  }
  #### Convert to data.table ####
  if(all(c("nodes","edges") %in% what)){
    messager("Converting tidygraph to data.table.")
    opts <- get_graph_colnames(g)
    id_col <- id_col[id_col %in% opts][1]
    if(is.na(id_col)) stopper("Could not find valid option within `id_col`.")
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
