#' @describeIn get_ get_
#' 
#' @param as_datatable Return as a data.table instead of a named vector.
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' dict <- get_ontology_dict(ont)
get_ontology_dict <- function(ont, 
                              from="id",
                              to=c("name","label","term"),
                              include_self=FALSE,
                              as_datatable=FALSE){
  
  to <- to[to %in% colnames(ont@elementMetadata)][1]
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
    dict <- data.table::data.table(ont@elementMetadata)[,from:=get(from)][,to:=get(to)][,c("from","to")]
    if(isTRUE(include_self)){
      dict <- rbind(dict,
                    data.table::data.table(ont@elementMetadata)[,from:=get(to)][,to:=get(to)][,c("from","to")])
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
