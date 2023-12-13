#' Get ontology
#' 
#' Import an up-to-date ontology directly from from the creators.
#' @param name
#' \itemize{
#' \item{"mondo"}{
#' Import the \href{https://mondo.monarchinitiative.org/}{Mondo} ontology.
#' \href{https://github.com/monarch-initiative/mondo/release}{
#' GitHub.}}.
#' \item{"hpo"}{
#' Import the \href{https://hpo.jax.org/app/}{Human Phenotype Ontology}.
#' \href{https://github.com/obophenotype/human-phenotype-ontology/release}{
#' GitHub.}}
#' }
#' @param add_metadata Add metadata to the resulting ontology object.
#' @inheritDotParams get_ontology_github
#' @returns \link[simona]{ontology_DAG}
#' 
#' @export
#' @examples
#' mondo <- get_ontology(name="mondo")
#' hpo <- get_ontology(name="hpo")
#' upheno <- get_ontology(name="upheno")
get_ontology <- function(name=c("mondo",
                                "hpo",
                                "upheno"),
                         terms=NULL,
                         filetype="-base.obo",
                         method=c("github",
                                  "bioportal"),
                         add_metadata=TRUE,
                         add_ancestors=FALSE,
                         save_dir=cache_dir(),
                         force_new=FALSE,
                         ...){ 
  name <- match.arg(name)
  method <- match.arg(method)
  
  if(method=="github"){
    if(name=="mondo"){
      ont <- get_ontology_github(file=paste0(name,filetype), 
                                 repo="monarch-initiative/mondo",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if(name %in% c("hp","hpo")){
      if(name=="hpo") name <- "hp"
      ont <- get_ontology_github(file=paste0(name,filetype), 
                                 repo="obophenotype/human-phenotype-ontology",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if(name=="upheno"){
      ont <- get_ontology_url(
        "https://github.com/obophenotype/upheno-dev/raw/master/upheno.obo", 
        force_new = force_new, 
        save_dir = save_dir, 
        ...) 
    }
  } 
  #### Add metadata #### 
  if(isTRUE(add_metadata)) {
    ont <- add_ontology_metadata(ont,
                                 add_ancestors=add_ancestors)
  }
  #### Subset ontology ####
  ont <- filter_ontology(ont = ont, 
                         terms = terms)
  #### Return ####
  return(ont)
}