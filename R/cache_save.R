#' @describeIn cache_ cache_
#' Save cache.
#' @export
#' @examples
#' cache_save(mtcars, tempfile())
cache_save <- function(obj,
                       save_path){
  if(!is.null(save_path)){
    dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
    messager("Caching file -->",save_path)
    saveRDS(obj, save_path)
  } 
}