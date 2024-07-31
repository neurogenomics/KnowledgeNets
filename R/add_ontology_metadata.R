#' Add ontology metadata 
#' 
#' Add per-term metadata to ontology.
#' @inheritParams add_
#' @export
#' @import simona
#' @importFrom Matrix colSums
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' ont2 <- add_ontology_metadata(ont)
add_ontology_metadata <- function(ont,
                                  lvl=2,
                                  force_new = FALSE,
                                  add_n_edges=TRUE,
                                  add_ontology_levels=TRUE){
  messager("Adding term metadata.")
  simona::mcols(ont)$IC <- simona::term_IC(ont)
  simona::mcols(ont)$depth <- simona::dag_depth(ont)
  simona::mcols(ont)$n_children <- simona::n_children(ont)
  simona::mcols(ont)$n_ancestors <- simona::n_ancestors(ont)
  simona::mcols(ont)$n_parents <- simona::n_parents(ont)
  simona::mcols(ont)$n_offspring <- simona::n_offspring(ont)
  simona::mcols(ont)$n_connected_leaves <- simona::n_connected_leaves(ont) 
  ont <- add_ancestors(ont = ont, 
                       force_new = force_new,
                       lvl = lvl)
  if(isTRUE(add_n_edges)){
    adj <- ontology_to(ont = ont,
                       to="adjacency")
    dict <- Matrix::colSums(adj)
    simona::mcols(ont)$n_edges <- dict[ont@terms]
  }
  if(isTRUE(add_ontology_levels)){
    simona::mcols(ont)$ontLvl <- get_ontology_levels(ont)
  }
  return(ont)
}
