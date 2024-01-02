#' @describeIn get_ get_
#' Get prevalence
#' 
#' Get epidemiological disease and phenotype prevalence data.
#' @param include_mondo Include MONDO IDs in the output.
#' @export
#' @examples
#' \dontrun{
#' get_prevalence()
#' }
get_prevalence <- function(method=c("orphanet","oard"),
                           agg_by=c("mondo_id","id","Name"),
                           include_mondo=TRUE,
                           ...){
  method <- match.arg(method)
  if(method=="orphanet"){
    dprev <- get_prevalence_orphanet(agg_by=agg_by, 
                                     include_mondo=include_mondo)
  } else if(method=="oard"){
    dprev <- get_prevalence_oard(...)
  }
}