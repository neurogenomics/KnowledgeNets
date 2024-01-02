#' @describeIn cache_ cache_
#' List cache
#'
#' List all data cached by the R package. 
#' @inheritParams base::unlink
#' @inheritDotParams base::unlink
#' @returns Null.
#'
#' @export
#' @examples
#' \dontrun{
#' cache_files()
#' }
cache_files <- function(save_dir=cache_dir(),
                        recursive=TRUE, 
                        ...){
  list.files(save_dir,recursive = recursive,...)
}
