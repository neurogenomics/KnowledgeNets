get_ontology_url <- function(URL,
                             save_dir=cache_dir(),
                             import_func=simona::import_ontology,
                             force_new=FALSE,
                             ...){
  save_path <- file.path(save_dir,basename(URL))
  if(file.exists(save_path) && !isTRUE(force_new)){
    messager("Importing cached file:",save_path)
    ont <- readRDS(save_path)
    return(ont)
  }
  ont <- import_func(URL, ...)
  saveRDS(ont, save_path)
  return(ont)
}