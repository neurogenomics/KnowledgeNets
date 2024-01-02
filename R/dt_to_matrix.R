#' @describeIn convert_ convert_
#' @export
#' @examples
#' dt <- data.table::as.data.table(mtcars, keep.rownames = TRUE)
#' X <- dt_to_matrix(dt)
dt_to_matrix <- function(dat,
                         omit_cols=NULL,
                         rownames=NULL,
                         as_sparse=TRUE){
  if(is.null(rownames)) {
    rownames <- dat[,1][[1]]
    omit_cols <- c(1,omit_cols)
  }
  if(!is.null(omit_cols)) {
    dat <- dat[,-omit_cols,with=FALSE]
  }
  X <- as.matrix(dat)|> `rownames<-`(rownames)
  if(isTRUE(as_sparse)){
    X <- methods::as(X,'sparseMatrix')
  }
  return(X)
}
