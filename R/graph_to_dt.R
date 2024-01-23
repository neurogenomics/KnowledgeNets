#' @describeIn convert_ convert_
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' g <- ontology_to(ont, to="tbl_graph")
#' dat <- graph_to_dt(g)
graph_to_dt <- function(g,
                        what=c("nodes","edges"),
                        id_col=c("id","name"),
                        prefixes = c("subject","object")){
  if(methods::is(g,"data.frame")) {
    if(!methods::is(g,"data.table")) g <- data.table::as.data.table(g)
    return(g)
  }
  #### Conver tot tbl_graph ####
  if(methods::is(g,"igraph") && !methods::is(g,"tbl_graph")){
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
    edges <- data.table::data.table(tmp$edges)
    data.table::setnames(edges,
                         old = c("from","to"),
                         new = prefixes)
    v1 <- data.table::copy(tmp$vertices) 
    data.table::setnames(v1,
                         gsub("_id$","",paste(prefixes[1],names(v1),sep="_")))
    v1[[prefixes[1]]] <- rownames(v1)
    v2 <- data.table::copy(tmp$vertices) 
    data.table::setnames(v2,
                         gsub("_id$","",paste(prefixes[2],names(v2),sep="_")))
    v2[[prefixes[2]]] <- rownames(v2)
    obj <- merge(edges, 
                 v1,
                 by="subject") |>
      merge(v2,
            by="object") |> 
      data.table::as.data.table()
    return(obj)
  } else if(length(what)==1){
    g |> 
      tidygraph::activate(!!what) |>
      data.table::as.data.table()
  }
}
