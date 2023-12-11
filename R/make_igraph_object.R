#' @describeIn plot_ plot_
#' Make an \link[igraph]{igraph} object
#'
#' This uses the network package to coerce the adjacency matrix into a
#' network object. It also adds the fold change, label,
#' and relative ontology level parameters to each node in the network.
#'
#' It expects there to be a column of HPO IDs in the dat dataframe called
#' hpo_id.
#' @inheritParams ggnetwork::fortify.network
#' @inheritDotParams ggnetwork::ggnetwork
#' @returns A \link[igraph]{igraph} object.
#'
#' @export
#' @examples
#' dat <- get_monarch_models()
#' g <- make_igraph_object(dat = dat)
make_igraph_object <- function(dat,
                               ont = get_ontology("hpo"),
                               adjacency = ontology_to_adjacency(ont),
                               colour_var = "fold_change",
                               add_ont_lvl_absolute = FALSE,
                               cols = list_columns(
                                 extra_cols = c(
                                   colour_var,
                                   grep("_count$|_values$",
                                        names(dat),
                                        value = TRUE)
                                 )
                               ),
                               layout = "fruchtermanreingold",
                               verbose = TRUE,
                               ...
                               ){
  requireNamespace("igraph")
  phenoNet <- make_network_object(dat = dat,
                                  hpo = hpo,
                                  adjacency = adjacency,
                                  colour_var = colour_var,
                                  add_ont_lvl_absolute = add_ont_lvl_absolute,
                                  cols = cols,
                                  ,
                                  ...)
  messager("Creating igraph object.")
  vertices <- unique(
    phenoNet[,!names(phenoNet) %in% c("x","y","vertex.names","xend","yend")]
  )
  rownames(vertices) <- vertices$hpo_id
  g <- igraph::graph_from_adjacency_matrix(adjacency)
  for(n in names(vertices)){
    igraph::vertex_attr(g,name = n) <- vertices[names(igraph::V(g)),n]
  }
  return(g)
}
