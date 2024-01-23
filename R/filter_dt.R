#' @describeIn filter_ filter_
#' Filter a \link[data.table]{data.table}.
#' @export
#' @examples
#' dat <- mtcars 
#' dat2 <- filter_dt(dat, filters=list(cyl=c(4,6)))
filter_dt <- function(dat,
                      filters){
  if(!methods::is(dat,"data.table")){
    dat <- data.table::as.data.table(dat)
  } 
  for(f in names(filters)){
    if(any(!is.null(filters[[f]])) &&
       f %in% names(dat)) {
      n1 <- nrow(dat)
      dat <- dat[get(f) %in% filters[[f]]]
      messager("Filtered",shQuote(f),":",
               formatC(n1-nrow(dat),big.mark = ","),"/",
               formatC(n1,big.mark = ","),
               "rows dropped.")
    }
  }
  return(dat)
}