#' @describeIn plot_ plot_
#' Save a plot using \link[ggplot2]{ggsave}, \pkg{grDevices},
#'  or \link[visNetwork]{visSave}.
#' 
#' @export
plot_save <- function(plt,
                      path,
                      width=NULL,
                      height=width){
 
  if(is.null(path)) return(NULL)
  
  requireNamespace("grDevices")
  messager("Saving plot -->",path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if(methods::is(plt,"gg")||methods::is(plt,"patchwork")){
    if(is.null(width)) width <- NA
    if(is.null(height)) height <- NA
    ggplot2::ggsave(filename = path,
                    plot = plt,
                    width = width,
                    height = height)
  } else if(endsWith(path,".pdf")){ 
    if(is.null(width)) width <- 7
    if(is.null(height)) height <- 7
    {
      grDevices::pdf(file = path,
                     width = width,
                     height = height)
      methods::show(plt)
      grDevices::dev.off()
    }
  } else if(endsWith(path,".png")){
    if(is.null(width)) width <- 480
    if(is.null(height)) height <- 480
    {
      grDevices::png(file = path,
                     width = width,
                     height = height)
      methods::show(plt)
      grDevices::dev.off()
    }
  } else if(endsWith(path,".html")){
    visNetwork::visSave(plt,
                        file = path,
                        selfcontained = TRUE,
                        background = "transparent")
  }
}