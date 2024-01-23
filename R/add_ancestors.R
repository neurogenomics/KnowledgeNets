#' @describeIn add_ add_
#' Add ancestor
#' 
#' For each term, get its ancestor at a given level
#' and add the ID and name of the ancestor to the ontology metadata.
#' By default, lower numbers are higher in the ontology
#'  (e.g. level 0 includes the most broad ontology term).
#' When a terms does not have an ancestor at a given level
#'  (e.g. \code{lvl=3} but the term itself is at level 1) the term is assigned
#'   to itself as its own ancestor.
#' When a term has multiple ancestors at a given level, 
#' the first ancestor is arbitrarily chosen.
#' @export
#' @inheritParams simona::dag_offspring
#' @examples 
#' ont <- get_ontology("hpo")
#' ont2 <- add_ancestors(ont)
add_ancestors <- function(ont,
                          lvl=2, 
                          include_self=TRUE,
                          force_new=FALSE){
  term <- ancestor <- NULL;
  
  if(is.null(lvl)) return (ont)
  messager("Adding ancestor metadata.")
  #### Check if ancestor metadata already present
  if(all(c("ancestor","ancestor_name") %in% colnames(ont@elementMetadata)) &&
     isFALSE(force_new)){
    messager("Ancestor metadata already present.",
             "Use force_new=TRUE to overwrite.")
    return(ont)
  } 
  all_lvls <- get_ontology_levels(ont, method = "depth")
  ancestors <- all_lvls[unname(all_lvls)==lvl]|>names()
  messager(length(ancestors),"ancestors found at level",lvl)
  
  ancestors_groups <- lapply(stats::setNames(ancestors,ancestors), function(x){
    data.table::data.table(
      term=simona::dag_offspring(ont, term = x, 
                                 include_self = include_self)
    )
  }) |> data.table::rbindlist(idcol  = "ancestor", fill = TRUE)
  #### Ensure one row per term ####
  ancestors_groups <- ancestors_groups[, .SD[1], keyby = "term"] 
  ancestors_groups <- ancestors_groups[ont@terms][is.na(ancestor),
                                                  ancestor:=term]
  ont@elementMetadata$ancestor <- ancestors_groups$ancestor 
  #### Add ancestor_name col
  ont@elementMetadata$ancestor_name <-  map_ontology_terms(
    ont = ont, 
    terms = ont@elementMetadata$ancestor, 
    to = "name")
  #### Return ####
  return(ont)
}
