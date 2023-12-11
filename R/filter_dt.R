#' @describeIn utils_ utils_
filter_dt <- function(dat,
                      filters){
  for(f in names(filters)){
    if(!is.null(filters[[f]]) &&
       filters[[f]] %in% names(dat)) {
      n1 <- nrow(dat)
      dat <- dat[get(f) %in% filters[[f]]]
      messager("Filtered",f,":",
               formatC(n1-nrow(dat),big.mark = ","),"/",
               formatC(n1,big.mark = ","),
               "rows dropped.")
    }
  }
  return(dat)
}