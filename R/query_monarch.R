#' @describeIn query_ query_
#' 
#' @export
#' @examples
#' cells <- monarchr::biolink_search(phrase_or_id = "T-cell")
query_monarch <- function(...){
  requireNamespace("monarchr")
  monarchr::biolink_search(...)
  
}