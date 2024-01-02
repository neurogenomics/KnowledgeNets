link_monarch_graph <- function(files,
                               maps,
                               ...){
  from <- to <- subject_category <- object_category <- NULL;
  #### Download and prepare maps ####
  map_dt <- lapply(seq(length(maps)), function(i){
    m <- maps[[i]]
    messager("Constructing data:",m[1],"<-->",m[2])
    URL <- files[(from==m[1] & to==m[2]) |
                 (from==m[2] & to==m[1])
    ][1,]$url
    d <- data.table::fread(URL)
    d[,subject_category:=m[1]][,object_category:=m[2]]
    g <- dt_to_graph(dat = d)
    g <- add_db(dat = g, 
                input_col = "id", 
                output_col = "db") 
    return(g)
  })
  #### Merge graphs ####
  g <- Reduce(tidygraph::graph_join, map_dt, ...) 
  return(g)
}