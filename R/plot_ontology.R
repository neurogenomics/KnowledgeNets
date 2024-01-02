#' @describeIn plot_ plot_
#' 
#' @export
#' @examples 
#' ont <- get_ontology("hp")
#' res <- plot_ontology(ont, 
#'                      terms=100,
#'                      types="circular",
#'                      partition_by_level=2,
#'                      edge_transparency=.9)
plot_ontology <- function(ont,
                          terms=NULL,
                          types =c("circular",
                                   "graphviz",
                                   "tidygraph",
                                   "visnetwork"),
                          ...){
  requireNamespace("DiagrammeR")
  #### Filter ont ####
  ont <- filter_ontology(ont,
                         terms=terms)
  #### Create each plot ####
  res <- list()
  if("circular" %in% types){
    res[["circular"]] <- plot_ontology_circular(ont,
                                                ...)
  }
  if("graphviz" %in% types){
    res[["graphviz"]] <- plot_ontology_graphviz(ont,
                                                ...)
  } 
  if("tidygraph" %in% types){
    res[["tidygraph"]] <- plot_ontology_tidygraph(ont,
                                                  ...)
  }
  if("visnetwork" %in% types){
    res[["visnetwork"]] <- plot_ontology_visnetwork(ont,
                                                    ...)
  }
  if("graph_3d" %in% types){
    g <- ontology_to(ont,"tbl_graph")
    res[["graph_3d"]] <- plot_graph_3d(g,
                                       ...)
  }
  return(res)
}