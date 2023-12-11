#' @describeIn query_ query_
query_oard <- function(ids,
                       concept_prefix="q=",
                       domain="https://rare.cohd.io/api/",
                       dataset_id=NULL,
                       endpoint="vocabulary/findConceptByAny",
                       batch_size=100,
                       workers=1
                       ){
  requireNamespace("BiocParallel")
  requireNamespace("htmltools")

  ids <- na.omit(unique(ids))
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
  BiocParallel::register(BiocParallel::MulticoreParam(workers=workers,
                                                      progressbar = TRUE))
  RES <- BiocParallel::bplapply(batches,
                                oard_query_api_i,
                                match.call()) |>
    data.table::rbindlist(idcol = "batch", fill = TRUE)
  return(RES)
}
