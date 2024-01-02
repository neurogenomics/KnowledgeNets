#' @describeIn add_ add_
#' Add ancestor
#' 
#' For each term, get its ancestor at a given level
#' and add the ID and name of the ancestor to the ontology metadata.
#' @export
#' @import BiocParallel
#' @inheritParams simona::dag_ancestors
#' @examples 
#' ont <- get_ontology("hpo", terms=10)
#' ont2 <- add_ancestor(ont)
add_ancestor <- function(ont,
                         lvl=2, 
                         include_self=TRUE,
                         force_new=FALSE){
  messager("Adding ancestor metadata.")
  #### Check if ancestor metadata already present
  if(all(c("ancestor","ancestor_name") %in% colnames(ont@elementMetadata)) &&
     isFALSE(force_new)){
    messager("Ancestor metadata already present.",
             "Use force_new=TRUE to overwrite.")
    return(ont)
  }
  BPPARAM <- BiocParallel::MulticoreParam(progressbar=TRUE)
  ont@elementMetadata$ancestor <- BiocParallel::bplapply(ont@terms,
                                                         BPPARAM = BPPARAM,
                                                         function(x){
    opts <- simona::dag_ancestors(ont,
                                  term = x,
                                  include_self=include_self)
    ## Find the ancestor at the given level (or closest to it)
    names(sort(abs(simona::dag_depth(ont,opts)-lvl)))[1]
  })|> unlist()
  #### Add ancestor names ####
  name_dict <- stats::setNames(ont@elementMetadata$name,
                               ont@elementMetadata$id)
  ont@elementMetadata$ancestor_name <- name_dict[ont@elementMetadata$ancestor]
  #### Return ####
  return(ont)
}