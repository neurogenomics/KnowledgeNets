#' @describeIn link_ link_
#' Link Monarch
#'
#' Construct a knowledge graph by iteratively linking together pairs of concepts
#'  across  multiple Monarch datasets.
#'
#' @export
#' @examples
#' dat <- link_monarch(maps = list(c("gene","disease")))
link_monarch <- function(maps = list(
                                    c("variant","disease"),
                                    c("variant","phenotype"),
                                    c("variant","gene")
                         ),
                         queries=NULL,
                         filters=
                           list(
                             phenotype=NULL,
                             db=NULL,#c("HP"),
                             gene=NULL
                           ),
                         as_graph=TRUE,
                         domain="https://data.monarchinitiative.org",
                         subdir="latest/tsv/all_associations/",
                         ...
                         ){
  
  files <- get_monarch_files(maps = maps,
                             queries = queries,
                             domain = domain,
                             subdir = subdir) 
  g <- link_monarch_graph(files=files,
                          maps=maps,
                          ...)
  for(nm in names(filters)){
    f <- filters[[nm]]
    if(is.null(f)){
      next
    }
    ## filter just that column in the nodes of the graph
    g|> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(get(eval(nm)) %in% f)
  }
  if(isFALSE(as_graph)){
   dat <- graph_to_dt(g)
   return(dat)
  } else {
    return(g)
  }
}
