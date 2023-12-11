#' @describeIn cache_ utils_
#' Clear cache
#'
#' Remove all data cached by the R package.
#' @param save_dir Path to cache directory.
#' @inheritParams base::unlink
#' @inheritDotParams base::unlink
#' @returns Null.
#'
#' @export
#' @examples
#' \dontrun{
#' cache_clear()
#' }
cache_clear <- function(save_dir=cache_dir(),
                        recursive=TRUE, 
                        ...
                        ){

  f <- list.files(save_dir,recursive = recursive)
  messager("Clearing",length(f),"cached files from:",save_dir)
  unlink(x = save_dir,recursive = recursive,...)
}
