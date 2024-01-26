#' @describeIn get_ get_
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
#' @param filetype File type to search for.
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
                                "upheno",
                                "uberon",
                                "cl"),
                         terms=NULL,
                         filetype="-base.obo",
                         method=c("github",
                                  "bioportal"),
                         add_metadata=TRUE,
                         add_ancestors=2,
                         add_n_edges=TRUE,
                         add_ontology_levels=TRUE,
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
    } else if (name %in% c("cl","cellontology","cell-ontology") ){
      ont <- get_ontology_github(file=paste0(name,filetype), 
                                 repo="obophenotype/cell-ontology",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if(name=="upheno"){
      get_ontology_robot()
      ont <- get_ontology_url(
        # "https://github.com/obophenotype/upheno/raw/master/upheno.owl",
        "https://purl.obolibrary.org/obo/upheno/v2/upheno.owl",
        import_func = simona::import_owl,
        force_new = force_new, 
        save_dir = save_dir, 
        ...) 
    } else if (name %in% c("uberon") ){
      ont <- get_ontology_github(file=paste0(name,filetype), 
                                 repo="obophenotype/uberon",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    }  
  } 
  #### Add metadata #### 
  if(isTRUE(add_metadata)) {
    ont <- add_ontology_metadata(ont,
                                 add_n_edges=add_n_edges,
                                 add_ancestors=add_ancestors, 
                                 add_ontology_levels=add_ontology_levels)
  }
  #### Subset ontology ####
  ont <- filter_ontology(ont = ont, 
                         terms = terms) 
  #### Return ####
  return(ont)
}