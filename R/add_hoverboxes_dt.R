add_hoverboxes_dt <- function(dat,
                              columns=names(dat),
                              hoverbox_column=c("hover",
                                                "title",# For visNetwork
                                                "label"# For plotly
                                                )[1],
                              width = 60,
                              digits = 3,
                              decorators = c("<b>","</b>"),
                              as_html=TRUE,
                              sep = if(isTRUE(as_html)) "<br>" else "\n"){
  messager("Adding hoverboxes to data.table.")
  hover <- name <- NULL;
  #### Check for id col ####
  if(!"name" %in% names(dat)){
    dat[,name:=.I]
  }
  ## Define helper functions
  interleave <- function(a,b,sep){
    idx <- order(c(seq_along(a), seq_along(b)))
    unlist(c(a,paste0(b,sep)))[idx] |> as.list()
  }
  round_if <- function(x,digits,width){
    if(is.numeric(x)) {
      round(x,digits = digits)
    } else {
      stringr::str_wrap(x, width = width)
    }
  }
  #### Create hoverboxes....data.table style! ####
  dat[, (hoverbox_column) := do.call(paste0,
                           interleave(a = paste0(decorators[1],
                                                 columns,
                                                 decorators[2],"= "),
                                      b = lapply(.SD,
                                                 round_if,
                                                 digits = digits,
                                                 width = width),
                                      sep = sep)
  ), 
  .SDcols = columns, by="name"] 
}