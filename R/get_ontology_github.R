#' @describeIn get_ get_
#' 
#' Get ontology from GitHub
#' @inheritDotParams simona::import_ontology
#' @inheritParams piggyback::pb_download_url  
#' @returns \link[simona]{ontology_DAG}
#'
#' @export
#' @examples
#' mondo <- get_ontology_github()
get_ontology_github <- function(file="mondo-base.obo", 
                                repo="monarch-initiative/mondo",
                                tag="latest",
                                save_dir=cache_dir(),
                                force_new=FALSE,
                                ...){
  
  path <- file.path(save_dir,file)
  if(!file.exists(path) || isTRUE(force_new)){
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
    saveRDS(ont,path)
  } else {
    messager("Importing cached file:",path)
    ont <- readRDS(path)
  }
  return(ont)
}
