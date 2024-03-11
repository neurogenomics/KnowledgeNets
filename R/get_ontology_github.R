#' Get ontology from GitHub and import it via \link[simona]{import_ontology}.
#' @param filetype File type to search for.
#' @inheritParams get_
#' @inheritDotParams simona::import_ontology
#' @inheritParams piggyback::pb_download_url  
#' @returns \link[simona]{ontology_DAG}
#'
#' @keywords internal
get_ontology_github <- function(name,
                                repo,
                                filetype="-base.obo",
                                file=paste0(name,filetype),  
                                tag="latest",
                                save_dir=cache_dir(),
                                force_new=FALSE,
                                ...){
  
  save_path <- file.path(save_dir,file)
  if(!file.exists(save_path) || isTRUE(force_new)){
    #### From Releases #### 
    requireNamespace("piggyback") 
    if(is.null(tag)){
      messager("Identifying latest release for:",repo)
      releases <- piggyback::pb_releases(repo = repo)
      tag <- releases$release_name[1]
    } 
    URL <- piggyback::pb_download_url(file = file, 
                                      repo = repo,
                                      tag = tag)
    messager("Preparing ontology_index object from:",URL)
    ont <- simona::import_ontology(file=URL,
                                   ...) 
    saveRDS(ont,save_path)
  } else {
    messager("Importing cached file:",save_path)
    ont <- readRDS(save_path)
  }
  return(ont)
}
