#' @describeIn map_ map_
#' @param columns Names of columns to map colour palettes to. 
#' @param preferred_palettes Preferred palettes to use for each column.
#' @inheritParams tidygraph::activate
#' @import pals
#' @export
#' @examples
#' colors <- map_colors(dat=mtcars, columns=c("cyl","gear"), preferred="viridis")
map_colors <- function(dat,
                       columns=NULL,
                       as=c("vector","dict","name","function"),
                       what="nodes",
                       preferred_palettes=NULL){
  used <- palette <- maxcolors <- NULL;
  as <- match.arg(as) 
  dat <- graph_to_dt(dat, what=what)
  if(is.null(columns)) columns <- names(dat)
  pdt <- pals::pals.maxcolors() |> data.table::data.table()
  #### Get unique palette for each column ####
  pdt[,used:=FALSE]
  lapply(stats::setNames(columns,columns),function(cl){ 
    x <- dat[[cl]]
    vals <- as.character(sort(unique(unlist(x))))
    if(!is.null(preferred_palettes) &&
       which(columns==cl)<=length(preferred_palettes)){
      p <- preferred_palettes[which(columns==cl)]
    } else {
      p <- pdt[maxcolors>=length(vals) & used==FALSE,]$palette[1]
    } 
    messager("Using palette:",p)
    pdt[palette==p,used:=TRUE]
    if(as=="function"){
      return(
        list(utils::getFromNamespace(p, "pals")) |> `names<-`(p)
      )
    } else if (as=="dict"){ 
      return(
        stats::setNames(
          utils::getFromNamespace(p, "pals")(n = length(vals)),
          vals
        )
      )
    } else if (as=="vector"){
      dict <- stats::setNames(
        utils::getFromNamespace(p, "pals")(n = length(vals)),
        vals
      )
      return(
        dict[as.character(x)]
      )
    } else {
      return(p)
    }
  })
}