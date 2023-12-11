#' @describeIn query_ query_
#' 
query_monarch <- function(...){
  requireNamespace("monarchr")
  monarchr::biolink_search(...)
}