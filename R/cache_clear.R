#' @describeIn cache_ cache_
#' Clear cache
#'
#' Remove all data cached by the R package.
#' @inheritDotParams base::unlink
#' @export
#' @examples
#' \dontrun{
#' cache_clear()
#' }
cache_clear <- function(save_dir=cache_dir(),
                        recursive=TRUE, 
                        ...
                        ){
  f <- cache_files(save_dir=save_dir,
                   recursive=recursive)
  messager("Clearing",length(f),"cached files from:",save_dir)
  unlink(x = save_dir,recursive = recursive,...)
}
