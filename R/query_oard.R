#' @describeIn query_ query_
#' 
#' Query the Open Real-world-based Annotation for Rare Disease (OARD).
#' See \href{https://rare.cohd.io/}{here} for details.
#' @source \href{https://smart-api.info/ui/25d608913f0e16a87dd29dabe3f52f78}{
#' OARD API docs} 
#' @source \href{https://www.cell.com/ajhg/fulltext/S0002-9297(22)00319-6}{
#' Publication} 
#' @param concept_prefix Concept prefix.
#' @param domain OARD domain URL.
#' @param dataset_id Dataset ID.
#' @param endpoint OARD API endpoint to use. 
#' See the \href{https://smart-api.info/ui/25d608913f0e16a87dd29dabe3f52f78}{
#' OARD API docs} for details.
#' @export
query_oard <- function(ids,
                       concept_prefix="q=",
                       domain="https://rare.cohd.io/api/",
                       dataset_id=NULL,
                       endpoint="vocabulary/findConceptByAny",
                       batch_size=100
                       ){
  requireNamespace("BiocParallel")
  requireNamespace("htmltools")
  requireNamespace("httr")
  requireNamespace("jsonlite")

  ids <- stats::na.omit(unique(ids))
  messager("Querying OARD API for",formatC(length(ids),big.mark = ","),
           "IDs.")
  batches <- split(ids, ceiling(seq_along(ids)/batch_size))
  oard_query_api_i <- function(batch,
                               ...){
    URL <- paste0(paste0(domain,endpoint),"?",
                  ifelse(is.null(dataset_id),
                         "",
                         paste0("dataset_id=",dataset_id,"&")
                  ),
                  if(!is.null(batch)){
                    paste0(concept_prefix,
                           htmltools::urlEncodePath(
                             paste(batch,collapse = ";")
                           )
                    )
                  } else {
                    ""
                  }
    )
    # message(URL)
    res <- httr::GET(URL)
    cont <- httr::content(res, as = "text", encoding = "UTF-8")
    js <- jsonlite::fromJSON(cont, simplifyDataFrame = TRUE)
    dat <- js$results
    if(length(dat)==0) return(dat)
    if(nrow(dat)==length(js$parameter$q)) dat$query <- js$parameter$q
    return(dat)
  }
  RES <- BiocParallel::bplapply(batches,
                                oard_query_api_i,
                                match.call()) |>
    data.table::rbindlist(idcol = "batch", fill = TRUE)
  return(RES)
}
