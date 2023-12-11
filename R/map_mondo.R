#' @describeIn map_ map_
#' Map to/from MONDO IDs
#'
#' @export
#' @examples
#' dat <- example_dat()
#' dat2 <- map_mondo(dat = dat)
map_mondo <- function(dat,
                      input_col = "disease_id",
                      output_col = "MONDO_ID",
                      to="MONDO",
                      map_types = NULL, 
                      map_to = "hpo",
                      top_n = NULL,
                      add_name=TRUE,
                      add_definitions=TRUE, 
                      all.x = TRUE,
                      allow.cartesian = FALSE
                      ){
  mondo_def <- db <- object <- NULL;

  if(output_col %in% names(dat)){
    messager("output_col already exists. Returning original dat.")
    return(dat)
  }
  key <- if(all(to=="MONDO")) "object" else "subject"
  value <- if(key=="object") "subject" else "object"
  map <- get_mondo_maps(map_types = map_types, 
                        map_to = map_to, 
                        top_n = top_n,
                        top_by = value) 
  data.table::setkeyv(map, key) 
  # sort(table(map$db))
  if(all(to!="MONDO")){
    #### Check to option ####
    if(any(!to %in% unique(map$db))){
      stop("to must be one of: ",paste(sort(unique(map$db)),collapse=", "))
    }
    to_list <- to
    map <- map[db %in% to_list]
  } else{
    map <- map[grepl("^MONDO",object)]
  }
  #### Map IDs by merging ####
  cols <- c(key,value)
  if(isTRUE(add_name)){
    cols <- c(cols,"disease_label")
  }
  dat2 <- data.table::merge.data.table(
    dat,
    map[,cols, with=FALSE] |>
      data.table::setnames(c(value),c(output_col)),
    by.x = input_col,
    by.y = key,
    all.x = all.x,
    allow.cartesian = allow.cartesian
  )
  ### Add MONDO definitions ####
  if(all(to=="MONDO")){ 
    if(isTRUE(add_definitions)){
      if(!exists("mondo")) mondo <- get_mondo_ont()
      dat2[,mondo_def:=mondo$def[get(output_col)]]
    }
    report_col <- c(output_col,"MONDO_name","mondo_def")
  } else {
    report_col <- output_col
  }
  ### Report ####
  report_missing(dat = dat2,
                 id_col = input_col,
                 report_col = report_col)
  return(dat2) 
}
