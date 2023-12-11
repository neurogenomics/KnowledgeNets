#' @describeIn convert_ convert_
#' @keywords internal
ontology_to_tidygraph <- function(ont,
                                  ...){
  name <- id <- NULL;
  
  g <- simona::dag_as_igraph(ont) |>
    tidygraph::as_tbl_graph(...) 
  ## Add ont@elementMetadata to g
  g <- g |>
    tidygraph::left_join(
      ont@elementMetadata |>
        tidygraph::rename(label=name) |>
        tidygraph::rename(name=id),
      by="name")
  return(g)
}