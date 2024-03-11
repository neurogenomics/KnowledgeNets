#' @describeIn plot_ plot_
#' Save a plot using \link[ggplot2]{ggsave}, \pkg{grDevices},
#'  or \link[visNetwork]{visSave}.
#' 
#' @export
plot_save <- function(plt,
                      save_path=save_path,
                      width=NULL,
                      height=width){
 
  if(is.null(save_path)) return(NULL)
  
  requireNamespace("grDevices")
  messager("Saving plot -->",save_path)
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  if(methods::is(plt,"gg")||methods::is(plt,"patchwork")){
    if(is.null(width)) width <- NA
    if(is.null(height)) height <- NA
    ggplot2::ggsave(filename = save_path,
                    plot = plt,
                    width = width,
                    height = height)
  } else if(endsWith(save_path,".pdf")){ 
    if(is.null(width)) width <- 7
    if(is.null(height)) height <- 7
    {
      grDevices::pdf(file = save_path,
                     width = width,
                     height = height)
      methods::show(plt)
      grDevices::dev.off()
    }
  } else if(endsWith(save_path,".png")){
    if(is.null(width)) width <- 480
    if(is.null(height)) height <- 480
    {
      grDevices::png(file = save_path,
                     width = width,
                     height = height)
      methods::show(plt)
      grDevices::dev.off()
    }
  } else if(endsWith(save_path,".html")){
    visNetwork::visSave(plt,
                        file = save_path,
                        selfcontained = TRUE,
                        background = "transparent")
  }
}