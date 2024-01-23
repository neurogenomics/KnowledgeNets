dt_to_graph <- function(dat,
                        from_pattern="subject",
                        to_pattern="object",
                        add_hover=FALSE){
  if(methods::is(dat,"tbl_graph")) {
    messager("Input is already a tbl_graph. Returning input.")
    return(dat)
  }
 make_nodes <- function(dat,
                        pattern){
   cols <- grep(pattern,names(dat), value = TRUE)
   dsub <- dat[,cols,with=FALSE]|> 
     data.table::setnames(pattern,"id")|> 
     unique()
   dsub |>
     data.table::setnames(cols,
                          trimws(gsub(pattern,"",names(dsub)),whitespace = "_"),
                          skip_absent = TRUE)
   dsub <- unique(dsub)
   return(dsub)
 }
 messager("Creating nodes.")
  nodes <- (
    rbind(make_nodes(dat, pattern=from_pattern),
          make_nodes(dat, pattern=to_pattern),
          fill=TRUE
    ) |> unique()
  )[,.SD[1],by="id"]
  #### Add hoverboxes ####
  if(add_hover){
    nodes <- add_hoverboxes_dt(nodes, hoverbox_column = "title")
  }
  #### Construct tidygraph #### 
  edge_cols <- c(
    from_pattern,to_pattern,
    # paste0(c(from_pattern,to_pattern),"_category"),
    grep(paste(c(from_pattern,to_pattern),collapse="|"),
         names(dat), value = TRUE, invert = TRUE)
  )|> unique()
  edge_cols <- edge_cols[edge_cols %in% names(dat)]
  messager("Creating tbl_graph.")
  tidygraph::tbl_graph(nodes = nodes,
                       edges = dat[,edge_cols,with=FALSE] |> 
                         data.table::setnames(c(from_pattern,to_pattern),
                                              c("from","to"))
                       )
}