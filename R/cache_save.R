#' @describeIn cache_ cache_
#' Save cache.
#' @export
#' @examples
#' cache_save(mtcars, tempfile())
cache_save <- function(obj,
                       path){
  if(!is.null(path)){
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    messager("Caching file -->",path)
    saveRDS(obj, path)
  } 
}