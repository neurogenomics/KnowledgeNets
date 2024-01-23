#' @describeIn utils_ utils_
report_filter<- function(g1,
                         g2,
                         cols,
                         suffix="remain after filtering."){
  dt1 <- graph_to_dt(g1,
                     what = "nodes")
  dt2 <- graph_to_dt(g2,
                     what = "nodes")
  for(col in cols){
    if(col %in% names(dt2) && 
       col %in% names(dt1)){
      n1 <- data.table::uniqueN(dt1[[col]])
      n2 <- data.table::uniqueN(dt2[[col]])
      messager(n2,"/",n1,paste0("(",round(n2/n1*100,2),"%)"),
               shQuote(col),suffix)
    } 
  }
}