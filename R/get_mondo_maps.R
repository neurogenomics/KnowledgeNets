#' @describeIn get_ get_
#' Get Mondo ID maps
#' 
#' Get mappings between Mondo IDs and IDs in other databases/ontologies.
#' All mappings stored on the official
#'\href{https://github.com/monarch-initiative/mondo/tree/master/src/ontology/mappings}{
#'Mondo GitHub}.
#' @param top_by Grouping columns when selecting \code{top_n} rows per grouping.
#' Can be a character vector of one or more column names. 
#' @returns \link[data.table]{data.table} of mappings.
#' 
#' @export
#' @examples
#' map <- get_mondo_maps("default") 
get_mondo_maps <- function(map_types=c("default",
                                       "broadmatch",
                                       "closematch",
                                       "exactmatch",
                                       "hasdbxref",
                                       "narrowmatch",
                                       "relatedmatch"),
                           map_to = NULL,
                           map_type_order=c("default",
                                            "exactmatch",
                                            "closematch",
                                            "narrowmatch",
                                            "broadmatch",
                                            "relatedmatch",
                                            "hasdbxref"),
                           top_n=NULL,
                           top_by=c("subject","object"),
                           save_dir=cache_dir()
                           ){
  path <- subject_label <- object_label <- disease_label <- map_type <- to <- 
    db <- NULL;
  requireNamespace("downloadR")
  
  if(length(map_types)==1 &&
     map_types=="default"){ 
    path <- downloadR::downloader(
      input_url =  paste0(
        "https://github.com/monarch-initiative/mondo/raw/master/",
        "src/ontology/mappings/mondo.sssom.tsv"),
      output_dir = save_dir,
      download_method = "download.file")
    map <- data.table::fread(path,
                             skip = "subject",
                             tmpdir = save_dir)
    map[,file:=basename(path)]
    map[,map_type:="default"]
    map[,to:=data.table::tstrsplit(basename(file),"_|[.]",keep=3)]
    add_db(dat=map,
           input_col="subject_id",
           output_col="from")
    add_db(dat=map,
           input_col="object_id",
           output_col="db")
    map[,to:=db]
  } else {
    files <- get_mondo_maps_files(save_dir = save_dir,
                                  map_types = map_types,
                                  map_to = map_to)
    #### Import maps #####
    map <- lapply(stats::setNames(files$link_raw,
                                  basename(files$link_raw)),
                  function(x){
      path <- downloadR::downloader(input_url = x,
                                    output_dir = save_dir,
                                    download_method = "download.file")
      data.table::fread(path,
                        skip="subject_id",
                        tmpdir = save_dir)
    }) |> data.table::rbindlist(fill = TRUE, idcol = "file")
    map[,map_type:=data.table::tstrsplit(basename(file),"_",keep=2)]
    map[,to:=data.table::tstrsplit(basename(file),"_|[.]",keep=3)]
    add_db(dat=map,
           input_col="object_id",
           output_col="db")
  }
  ## Fix inconsistency with other Monarch data files: 
  ##  e.g. subject --> subject
  data.table::setnames(map,gsub("_id$","",names(map)))
  #### Sort mappings by confidence #### 
  map[,map_type:=factor(map_type,levels = map_type_order,ordered = TRUE)]
  data.table::setorderv(map,cols = "map_type")
  #### Select the top mapping per ID ####
  if(is.numeric(top_n)){
    n1 <- nrow(map)
    map <- map[,utils::head(.SD, top_n), by=top_by]
    messager(formatC(nrow(map),big.mark = ","),"/",
             formatC(n1,big.mark = ","),
             "rows remain after filtering with",
             paste0("top_n=",top_n,"."))
  } 
  #### Fill in label with any available info #####
  map[,disease_label:=data.table::fcoalesce(object_label,subject_label)]
  return(map) 
}
