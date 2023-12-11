add_ontology_metadata <- function(ont){
  # meta <- simona::mcols(ont) |> data.table::data.table()
  # meta <- add_ancestor(meta)
  simona::mcols(ont)$IC <- simona::term_IC(ont)
  simona::mcols(ont)$depth <- simona::dag_depth(ont)
  simona::mcols(ont)$n_children <- simona::n_children(ont)
  simona::mcols(ont)$n_ancestors <- simona::n_ancestors(ont)
  simona::mcols(ont)$n_parents <- simona::n_parents(ont)
  simona::mcols(ont)$n_offspring <- simona::n_offspring(ont)
  simona::mcols(ont)$n_connected_leaves <- simona::n_connected_leaves(ont) 
  return(ont)
}