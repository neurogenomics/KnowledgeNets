#' @describeIn get_ get_
#' Add ancestor
#' 
#' For each term, get its ancestor at a given level
#' and add the ID and name of the ancestor to the ontology metadata.
#' @export
#' @examples 
#' ont <- get_ontology("hpo", terms=10)
#' def <- get_definitions(ont)
get_definitions <- function(ont,
                            from="id",
                            to="definition"){
  get_ontology_dict(ont=ont,
                    from=from,
                    to=to)
}