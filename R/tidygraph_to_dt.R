#' @describeIn convert_ convert_
tidygraph_to_dt <- function(g,
                            what=c("nodes","edges")){
  if(all(c("nodes","edges") %in% what)){
    tmp <- igraph::as_data_frame(g,what="both")  
    ## merge edges and vertices 
    obj <- merge(tmp$edges, 
                 tmp$vertices,
                 by.x="from", 
                 by.y="name") |>
      merge(tmp$vertices,
            by.x="to",
            by.y="name",
            suffixes = c(".from",".to")) |> 
      data.table::as.data.table()
    return(obj)
  }
  if(all(c("nodes","edges") %in% what)){
    data.table::as.data.table(g)
  }
  what <- match.arg(what)
  g |> tidygraph::activate(!!what) |>
    data.table::as.data.table()
}