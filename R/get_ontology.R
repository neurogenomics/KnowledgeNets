#' @describeIn get_ get_ontology
#' Get ontology
#' 
#' Import an up-to-date ontology directly from from the creators or via the
#' \href{https://www.ebi.ac.uk/ols4}{EBML-EBI Ontology Lookup Service} API.
#' @param name
#' \itemize{
#' \item{<...>}{Any ontology name from \link{get_ols_options}}
#' \item{"mondo"}{
#' Import the \href{https://mondo.monarchinitiative.org/}{Mondo} ontology.
#' \href{https://github.com/monarch-initiative/mondo/release}{
#' GitHub.}}.
#' \item{"hpo"}{
#' Import the \href{https://hpo.jax.org/app/}{Human Phenotype Ontology}.
#' \href{https://github.com/obophenotype/human-phenotype-ontology/release}{
#' GitHub.}}
#' }
#' @param method Whether to import ontology via the \code{rols} package or
#' via the \link{get_ontology_github}/link{get_ontology_url} functions. 
#' @param add_metadata Add metadata to the resulting ontology object.
#' @inheritDotParams get_ontology_github
#' @returns \link[simona]{ontology_DAG}
#' 
#' @export
#' @examples
#' mondo <- get_ontology(name="mondo")
#' \dontrun{
#'   hp <- get_ontology(name="hp")
#'   upheno <- get_ontology(name="upheno")
#' }
get_ontology <- function(name=c("mondo",
                                "hp",
                                "upheno",
                                "uberon",
                                "cl"),
                         ## Safer to use OBO files (via GitHub) 
                         ## rather than OWL files (via rols)
                         method=c("rols",
                                  "github"), 
                         terms=NULL,
                         add_metadata=TRUE,
                         add_ancestors=2,
                         add_n_edges=TRUE,
                         add_ontology_levels=TRUE,
                         save_dir=cache_dir(),
                         force_new=FALSE,
                         ...){ 
  name <- name[1]
  method <- match.arg(method) 
  
  save_path <- file.path(save_dir, paste0(name,".rds"))
  if(isFALSE(force_new) && file.exists(save_path)){
    messager("Loading cached ontology:",save_path)
    ont <- readRDS(save_path)
    return(ont)
  } 
  ol <- rols::Ontologies()
  rols_opts <- get_ols_options(ol=ol)
  if(method=="rols" && 
     !name %in% rols_opts){
    messager("Ontology not found via 'rols.' Trying method='github'.'")
    method <- "github"
  }
  #### via EMBL-EBI Ontology Lookup Service ####
  if(method=="rols"){ 
    ol_ont <- ol[[name]]
    get_ontology_robot()
    ont <- get_ontology_url(
      URL = ol_ont@config$fileLocation,
      force_new = force_new, 
      save_dir = save_dir, 
      ...) 
  #### Via manually coded functions ####
  } else if(method=="github"){
    if(name=="mondo"){
      ont <- get_ontology_github(name=name, 
                                 repo="monarch-initiative/mondo",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if(name %in% c("hp","hpo")){
      if(name=="hpo") name <- "hp"
      ont <- get_ontology_github(name=name, 
                                 repo="obophenotype/human-phenotype-ontology",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if (name %in% c("cl","cellontology","cell-ontology") ){
      ont <- get_ontology_github(name=name, 
                                 repo="obophenotype/cell-ontology",
                                 save_dir=save_dir,
                                 force_new=force_new,
                                 ...)
    } else if(name=="upheno"){
      get_ontology_robot()
      ont <- get_ontology_url(URL = 
        # "https://github.com/obophenotype/upheno/raw/master/upheno.owl",
        "https://purl.obolibrary.org/obo/upheno/v2/upheno.owl",
        # import_func = simona::import_owl,
        force_new = force_new, 
        save_dir = save_dir, 
        ...) 
    } else if (name %in% c("uberon") ){
      ont <- get_ontology_github(name=name, 
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
  #### Cache RDS object ####
  if(!is.null(save_dir)){
    dir.create(save_dir, recursive=TRUE, showWarnings = FALSE)
    messager("Saving ontology -->",save_path)
    saveRDS(ont, save_path)
  }
  #### Return ####
  return(ont)
}