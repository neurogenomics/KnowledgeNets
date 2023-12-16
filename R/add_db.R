#' @describeIn utils_ utils_
add_db <- function(dat,
                   input_col,
                   output_col=paste0(input_col,"_db"),
                   split=":",
                   keep=1){
  
  if(methods::is(dat,"tbl_graph")){
    dat <- dat |> 
      tidygraph::activate("nodes") |> 
      tidygraph::mutate(!!output_col:=stringr::str_split(get(input_col),
                                                     pattern=split,
                                                     simplify = TRUE)[,keep])
    return(dat)
  }else if (methods::is(dat,"data.table")){
    dat[,(output_col):=data.table::tstrsplit(get(input_col),
                                             split=split,
                                             keep = keep)]
  } else {
    stop("dat must be a tbl_graph or data.table")
  }
}