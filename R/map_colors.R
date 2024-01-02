#' @describeIn map_ map_
#' @param columns Names of columns to map colour palettes to.
#' @import pals
map_colors <- function(dat,
                       columns=NULL,
                       as=c("function","vector","name")){
  used <- palette <- NULL;
  as <- match.arg(as)
  maxcolors <- NULL;
  if(!is.null(columns)) dat <- dat[,columns,with=FALSE] 
  pdt <- pals::pals.maxcolors() |> data.table::data.table()
  #### Get unique palette for each column ####
  pdt[,used:=FALSE]
  lapply(dat,function(x){ 
    vals <- as.character(sort(unique(unlist(x))))
    p <- pdt[maxcolors>=length(vals) & used==FALSE,]$palette[1]
    messager("Using palette:",p)
    pdt[palette==p,used:=TRUE]
    if(as=="function"){
      return(
        list(utils::getFromNamespace(p, "pals")) |> `names<-`(p)
      )
    } else if (as=="vector"){ 
      return(
        stats::setNames(
          utils::getFromNamespace(p, "pals")(n = length(vals)),
          vals
        )
      )
    } else {
      return(p)
    }
  })
}