#' @describeIn get_monarch_ get_
#' Get Monarch models
#'
#' Get disease-to-model mappings for multiple model species.
#' Additionally maps MONDO IDs to OMIM and Orphanet IDs.  
#' NOTE, adding additional \code{maps} 
#' will drastically reduce the number of results.
#' @export
#' @examples
#' models <- get_monarch_models()
get_monarch_models <- function(maps = list(
                                m2d=c("model","disease")
                                # m2g=c("model","gene")
                                # m2v=c("model","variant")
                               ),
                               filters=
                                 list(
                                   disease=NULL,
                                   gene=NULL,
                                   variant=NULL
                               ),
                               to=c("OMIM","Orphanet","DECIPHER"),
                               map_orthologs=TRUE,
                               ...){
  dat <- link_monarch(maps = maps,
                      filters = filters,
                      ...) 
  messager("Model species:",data.table::uniqueN(dat$subject_taxon_label))
  #### Map disease IDs ####
  if(length(to)>0 &&
     "disease" %in% names(dat)){
    dat <- map_mondo(dat = dat[,-c("disease_label")],
                     input_col="disease",
                     output_col="disease_id",
                     to=to)
  }
  #### Map gene IDs ####
  if(isTRUE(map_orthologs) &&
     "gene" %in% names(dat)){
    dat <- map_monarch_genes(dat = dat,
                             gene_col ="gene")
  }
  return(dat)
}
