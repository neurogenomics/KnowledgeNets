#' @describeIn get_ get_
#' 
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' dict <- get_ontology_dict(ont)
get_ontology_dict <- function(ont, 
                              from="id",
                              to="name",
                              include_self=FALSE){
  ## Check from col exists
  if(!from %in% colnames(ont@elementMetadata)){
    stopper("Column",from,"not found in ontology metadata.")
  }
  ## Check to col exists
  if(!to %in% colnames(ont@elementMetadata)){
    stopper("Column",to,"not found in ontology metadata.")
  }
  dict <- stats::setNames(ont@elementMetadata[[to]],
                          ont@elementMetadata[[from]])
  if(isTRUE(include_self)){
    dict <- c(dict,
              stats::setNames(ont@elementMetadata[[to]],
                              ont@elementMetadata[[to]])
              )
  }
  return(dict)
}
