#' @describeIn map_ map_
#' Map ontology terms to an alternative name.
#'
#' Harmonise a mixed vector of term names (e.g. "Focal motor seizure")
#' and term IDs (e.g. c("HP:0000002","HP:0000003")).
#' @param terms A character vector of term names and/or term IDs.
#' @param keep_order Return a named list of the same length and order
#' as \code{terms}.
#' If \code{FALSE}, return a named list of only the unique \code{terms},
#' sometimes in a different order.
#' @param invert Invert the keys/values of the dictionary,
#' such that the key becomes the values (and vice versa).
#' @returns Character vector
#'
#' @export
#' @examples
#' ont <- get_ontology("hp")
#' terms <- c("Focal motor seizure","HP:0000002","HP:0000003")
#' term_names <- map_ontology_terms(ont=ont, terms=terms)
#' term_ids <- map_ontology_terms(ont=ont, terms=terms, to="id")
map_ontology_terms <- function(ont,
                               terms = NULL,
                               to=c("name","id"),
                               keep_order = TRUE,
                               invert = FALSE){
  to <- match.arg(to)
  if(!is.null(terms)) terms <- as.character(terms)
  terms_og <- terms
  terms <- unique(terms)
  #### to IDs ###
  if(to=="id"){
    messager("Translating all terms to HPO IDs.")
    dict <- get_ontology_dict(ont,
                              from="name",
                              to=to,
                              include_self=TRUE)
  } else {
    #### to names ###
    messager("Translating all terms to names.")
    dict <- get_ontology_dict(ont,
                              from="id",
                              to=to,
                              include_self=TRUE)
  }
  out <- dict[terms]
  #### Return ####
  if(isFALSE(keep_order)){
    messager("+ Returning a dictionary of terms",
             "(different order as input).")
  } else {
    messager("+ Returning a vector of terms",
             "(same order as input).")
    out <- out[terms_og]
  }
  #### Invert ####
  if(isTRUE(invert)){
    out <- invert_dict(out)
  }
  return(out)
}
