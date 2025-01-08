#' @describeIn get_ get_
#'
#' @param as_datatable Return as a data.table instead of a named vector.
#' @param include_alternative_terms Include alternative terms in the dictionary.
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' dict <- get_ontology_dict(ont)
get_ontology_dict <- function(ont,
                              from="short_id",
                              to=c("name","label","term"),
                              include_self=FALSE,
                              include_alternative_terms=FALSE,
                              as_datatable=FALSE){
  to <- intersect(to,colnames(ont@elementMetadata))[1]

  if(from=="id") from <- "short_id"
  if(to=="id") to <- "short_id"

  ## Check from col exists
  if(!from %in% colnames(ont@elementMetadata)){
    stopper("Column",from,"not found in ontology metadata.")
  }
  ## Check to col exists
  if(!to %in% colnames(ont@elementMetadata)){
    stopper("Column",to,"not found in ontology metadata.")
  }

  if(isTRUE(as_datatable)){
    #### As data.table ####
    dict <- data.table::as.data.table(
      ont@elementMetadata
      )[,from:=get(from)][,to:=get(to)][,c("from","to")]
    if(isTRUE(include_alternative_terms) &&
       "alternative_terms" %in% methods::slotNames(ont) &&
       length(ont@alternative_terms)>0){
      data.table::setkeyv(dict, c("from")) 
      tmp <- data.table::data.table(
        from=gsub("_",":",basename(names(ont@alternative_terms))),
        to=dict[unname(ont@alternative_terms)]$to)
      dict <- rbind(dict,tmp)
    }
    if(isTRUE(include_self)){
      dict <- rbind(dict,
                    data.table::as.data.table(
                      ont@elementMetadata
                      )[,from:=get(to)][,to:=get(to)][,c("from","to")])
    }
    dict <- unique(dict)
    data.table::setkeyv(dict, c("from"))
  } else {
    #### As named vector ####
    dict <- stats::setNames(ont@elementMetadata[[to]],
                            ont@elementMetadata[[from]])
    if(isTRUE(include_self)){
      dict <- c(dict,
                stats::setNames(ont@elementMetadata[[to]],
                                ont@elementMetadata[[to]])
      )
    }
  }
  return(dict)
}
