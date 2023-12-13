#' @describeIn map_ map_
#' @import pals
map_colors <- function(dat,
                       columns=NULL,
                       as=c("function","vector","name")){
  as <- match.arg(as)
  maxcolors <- NULL;
  if(!is.null(columns)) dat <- dat[,columns,with=FALSE]
  #### Get all available palettes ####
  pals_info <- function(){
    maxcolors <- NULL;
    ns <- getNamespaceExports("pals")
    syspals <- utils::getFromNamespace("syspals", "pals")
    pdt <- sapply(names(syspals)[names(syspals) %in% ns], 
                  function(p){
      f <- formals(utils::getFromNamespace(p,"pals"))
      if(!"n" %in% names(f)) return(NA)
      if(rlang::is_missing(f$n)) Inf else eval(f$n)
    }) |> 
      data.table::as.data.table(keep.rownames=TRUE) |>
      `colnames<-`(c("palette","maxcolors"))
    pdt[,is_finite:=is.finite(maxcolors)]
    data.table::setorderv(pdt,"maxcolors",na.last=TRUE)
    return(pdt)
  } 
  pdt <- pals_info()
  # bpi <- data.table::data.table(RColorBrewer::brewer.pal.info, 
  #                               keep.rownames = "palette")
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