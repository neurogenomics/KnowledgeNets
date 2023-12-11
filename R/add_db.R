#' @describeIn utils_ utils_
add_db <- function(dat,
                   input_col,
                   output_col=paste0(input_col,"_db"),
                   split=":",
                   keep=1){
  if(!input_col %in% names(dat)){
    messager("Warning: input_col not in dat.") 
  } else{
    dat[,(output_col):=data.table::tstrsplit(get(input_col),
                                             split=split,
                                             keep = keep)]
  }
}