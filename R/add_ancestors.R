#' Add ancestors
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
#' @param prefix A prefix for the ancestor column names.
#' @param fill_na If \code{TRUE}, fill ancestor columns with 
#' NA values using the term ID and name.
#' @inheritParams add_
#' @export
#' @inheritParams simona::dag_offspring
#' @examples 
#' ont <- get_ontology("hp")
#' ont2 <- add_ancestors(ont, lvl=4)
add_ancestors <- function(ont,
                          lvl=2, 
                          include_self=TRUE,
                          prefix="ancestor",
                          fill_na=TRUE,
                          i=1,
                          force_new=FALSE){
  term <- NULL;
  
  if(is.null(lvl)) return (ont)
  messager("Adding ancestor metadata.")
  #### Check if ancestor metadata already present
  prefix_name <- paste0(prefix,"_name")
  if(all(c(prefix,prefix_name) %in% colnames(ont@elementMetadata)) &&
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
  }) |> data.table::rbindlist(idcol  = prefix, fill = TRUE)
  #### Ensure one row per term ####
  ancestors_groups <- ancestors_groups[, .SD[i], keyby = "term"] 
  if(isTRUE(fill_na)){
    ancestors_groups <- ancestors_groups[ont@terms][is.na(get(prefix)),
                                                    (prefix):=term]
  } 
  ont@elementMetadata[[prefix]] <- ancestors_groups[[prefix]]
  #### Add ancestor_name col
  ont@elementMetadata[[prefix_name]] <- map_ontology_terms(
    ont = ont, 
    terms = ont@elementMetadata[[prefix]], 
    to = "name")
  #### Return ####
  return(ont)
}
