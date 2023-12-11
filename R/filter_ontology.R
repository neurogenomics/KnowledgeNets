#' @describeIn filter_ filter_
#' Filter ontology
#'
#' Filter ontology by terms.
#' @export
#' @examples
#' ont <- get_ontology("hp")
#' ont2 <- filter_ontology(ont,terms=c("HP:0000001","HP:0000002"))
#' ont3 <- filter_ontology(ont,terms=100)
filter_ontology <- function(ont,
                            terms=NULL,
                            remove_terms=NULL,
                            use_simona=FALSE,
                            ...){
  #### Check remove_terms #### 
  terms <- terms[!terms %in% remove_terms]
  #### Use simona ####
  if(isTRUE(use_simona)){
    ont <- simona::dag_filter(ont, terms=terms, ...)
    return(ont)
  }
  #### Use custom filtering methods ####
  if(!is.null(terms)){
    ## Characters 
    if(is.character(terms)){
      terms <- terms[terms %in% ont@terms] |> unique()
      if(length(terms)==0) {
        stopper("None of the supplied terms found in the ontology.")
      } 
      ont <- ont[,terms]
      
    } else if (is.numeric(terms)){
      messager("Randomly sampling",terms,"term(s).")
      if(terms>length(ont@terms)){
        messager(
          "Number of terms requested exceeds number of terms in the ontology.",
          "Returning original ontology object without filtering.")
        return(ont) 
      } 
      if(terms==0) stopper("Terms must be >0 if numeric.")
      term_ids <- sample(ont@terms,terms, replace = FALSE)
      ont <- ont[,term_ids]
    }  
  }
  return(ont)
}