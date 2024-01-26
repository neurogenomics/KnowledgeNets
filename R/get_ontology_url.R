get_ontology_url <- function(URL,
                             save_dir=cache_dir(),
                             import_func=simona::import_ontology,
                             force_new=FALSE,
                             ...){
  path <- file.path(save_dir,basename(URL))
  if(file.exists(path) && !isTRUE(force_new)){
    messager("Importing cached file:",path)
    ont <- readRDS(path)
    return(ont)
  }
  ont <- import_func(URL, ...)
  saveRDS(ont, path)
  return(ont)
}