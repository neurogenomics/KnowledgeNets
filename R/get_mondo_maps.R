#' @describeIn get_monarch_ get_
#' Get Mondo ID maps
#' 
#' Get mappings between Mondo IDs and IDs in other databases/ontologies.
#' All mappings stored on the official
#'\href{https://github.com/monarch-initiative/mondo/tree/master/src/ontology/mappings}{
#'Mondo GitHub}.
#' @returns \link[data.table]{data.table} of mappings.
#' 
#' @export
#' @examples
#' map <- get_mondo_maps("hasdbxref") 
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
                           top_by=c("subject","object")
                           ){
  path <- subject_label <- object_label <- disease_label <- NULL;
  
  if(length(map_types)==1 &&
     map_types=="default"){
    map <- data.table::fread(
      paste0("https://github.com/monarch-initiative/mondo/raw/master/",
             "src/ontology/mappings/mondo.sssom.tsv"),
      skip = "subject")
    
  } else {
    files <- echogithub::github_files(owner = "monarch-initiative",
                                      repo = "mondo", 
                                      query = "src/ontology/mappings/") 
    #### Filter map types ####
    files[,map_type:=data.table::tstrsplit(basename(path),"_",keep=2)]
    files[is.na(map_type),map_type:="default"]
    if(!is.null(map_types)){
      files <- files[map_type %in% map_types]  
    }
    #### Filter mapping to ####
    files[,to:=data.table::tstrsplit(basename(path),"_|[.]",keep=3)]
    if(!is.null(map_to)) {
      if("hpo" %in% map_to) {
        map_to <- c(map_to[map_to!="hpo"],
                    "omim","decipher","orphanet","hp") |> unique()
      }
      files <- files[to %in% map_to]
    }
    #### Import maps #####
    map <- lapply(stats::setNames(files$link_raw,
                                  basename(files$link_raw)), function(x){
      data.table::fread(x,skip="subject_id")
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
