link_monarch_dt <- function(files,
                            maps,
                            filters,
                            all=FALSE,
                            allow.cartesian=FALSE){
  from <- to <- NULL;
  #### Download and prepare maps ####
  map_dt <- lapply(maps, function(m){
    messager("Constructing data:",m[1],"<-->",m[2])
    URL <- files[(from==m[1] & to==m[2]) |
                 (from==m[2] & to==m[1])
    ][1,]$url
    d <- data.table::fread(URL)
    add_db(dat = d, 
           input_col = "subject") 
    add_db(dat = d, 
           input_col = "object") 
    data.table::setnames(d,gsub("object",m[2],gsub("subject",m[1],names(d))))
    d
  })
  
  dat <- Reduce(function(x,y,...){
    nms <- Reduce(intersect,lapply(list(x,y),names))
    relation_nms <- c("relation","relation_label","evidence","evidence_label",
                      "source","is_defined_by","qualifier")
    by <- nms[!nms %in% relation_nms]
    tb <- table(unlist(maps))
    suffixes <- paste0(
      ".",unname(unlist(maps))[unname(unlist(maps)) %in% names(tb)[tb==1]]
    )
    merge(x,y,
          by=by,
          suffixes=suffixes,
          all=all,
          allow.cartesian=allow.cartesian,
          ...)
  }, map_dt )
  
  #### Filter ####
  dat <- filter_dt(dat=dat, 
                   filters=filters)
  #### Report ####
  sep <- "----\n"
  lvls <- c(unique(sapply(maps,function(x)x[2])),
            unique(sapply(maps,function(x)x[1]))) |> unique()
  lvls_col <- lapply(lvls,function(x) c(paste(x,c("db","taxon"),sep = "_"),x))
  for(x in lvls_col){
    for(y in x){
      if(y %in% names(dat)){
        messager(paste0(y,"(s)"),":",
                 formatC(data.table::uniqueN(dat[[y]]),big.mark = ","))
      }
    }
    cat(sep)
  }
  return(dat)
}