#' @describeIn plot_ plot_
#' Save a plot using \pkg{grDevices} or \link[visNetwork]{visSave}.
#' 
#' @export
plot_save <- function(plt,
                      path,
                      width=7,
                      height=width){
  requireNamespace("grDevices")
  messager("Saving plot -->",path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  if(is.null(path)) return(NULL)
  
  if(endsWith(path,".pdf")){
    {
      grDevices::pdf(file = path,
                     width = width,
                     height = height)
      methods::show(plt)
      grDevices::dev.off()
    }
  } else if(endsWith(path,".png")){
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