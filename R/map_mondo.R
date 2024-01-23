#' @describeIn map_ map_
#' Map to/from mondo IDs
#'
#' @param to Character vector of database(s) to map IDs to.
#' When not \code{"mondo"}, can supply multiple alternative databases to map to
#'  (e.g. \code{c("OMIM","Orphanet","DECIPHER")}).
#' @export
#' @examples
#' dat <- example_dat(rm_types="gene")
#' dat2 <- map_mondo(dat = dat, map_to="hpo")
map_mondo <- function(dat,
                      input_col = "id",
                      output_col = "mondo_id",
                      to = "mondo",
                      map_types = NULL, 
                      map_to = NULL,
                      top_n = NULL,
                      add_name=TRUE,
                      add_definitions=TRUE, 
                      all.x = TRUE,
                      allow.cartesian = FALSE,
                      save_dir=cache_dir()
                      ){
  object_db <- subject <- NULL;

  if(output_col %in% names(dat)){
    messager("output_col already exists. Returning original dat.")
    return(dat)
  }
  key <- if(all(to=="mondo")) "object" else "subject"
  value <- if(key=="object") "subject" else "object"
  map <- get_mondo_maps(map_types = map_types, 
                        map_to = map_to, 
                        top_n = top_n,
                        top_by = value,
                        save_dir = save_dir) 
  data.table::setkeyv(map, key) 
  # sort(table(map$db))
  if(all(to!="mondo")){
    #### Check to option ####
    if(any(!tolower(to) %in% tolower(unique(map$object_db)))){
      stop("to must be one of: ",paste(sort(unique(map$object_db)),
                                       collapse=", "))
    }
    to_list <- to
    map <- map[object_db %in% to_list]
  } else{
    map <- map[grepl("^MONDO",subject)]
  }
  #### Map IDs by merging ####
  messager("Mapping",input_col,"-->",output_col)
  cols <- c(key,value)
  if(isTRUE(add_name)){
    cols <- c(cols,"subject_label","label")
  }
  dat2 <- data.table::merge.data.table(
    dat,
    map[,unique(cols), with=FALSE] |>
      data.table::setnames(c(value,"subject_label"),
                           c(output_col,"mondo_name")),
    by.x = input_col,
    by.y = key,
    all.x = all.x,
    allow.cartesian = allow.cartesian
  ) |> unique()
  ### Add mondo definitions ####
  if(all(to=="mondo")){ 
    if(isTRUE(add_definitions)){
      add_definitions_mondo(dat2, 
                            input_col = output_col)
    }
    report_col <- c(output_col,"mondo_name","mondo_def")
  } else {
    report_col <- output_col
  }
  ### Report ####
  report_missing(dat = dat2,
                 id_col = input_col,
                 report_col = report_col)
  return(dat2) 
}
