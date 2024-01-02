#' @describeIn get_ get_
#' Load disease genes
#'
#' Load gene lists associated with each disease phenotype from:
#' \itemize{
#' \item{OMIM}
#' \item{Orphanet}
#' \item{DECIPHER}
#' }
#' @inheritDotParams link_monarch
#' @returns data.table
#'
#' @export
#' @import data.table
#' @examples
#' genes <- get_genes_disease()
get_genes_disease <- function(maps = list(c("gene","disease")),
                              ...){
  disease_id <- disease <- mondo_id <- NULL;
  
  #### Import data ####
  genes <- link_monarch(maps = maps,
                        ...
                        )
  # length(unique(genes$disease)) # 6244
  # genes2 <- get_monarch(queries = "gene_disease")[[1]]
  # length(unique(genes2$object))
  genes <- map_mondo(dat = genes[,-c("disease_label")],
                     input_col = "disease",
                     output_col = "disease_id",
                     to = c("OMIM","Orphanet","DECIPHER"))
  genes[,disease_id:=data.table::fcoalesce(disease_id,disease)] 
  data.table::setnames(genes,"disease","mondo_id")
  genes <- genes[grepl("^mondo",mondo_id)]
  add_db(genes,
         input_col = "disease_id", 
         output_col = "disease_db")
  messager("disease(s):",data.table::uniqueN(genes$disease_id))
  messager("genes(s):",data.table::uniqueN(genes$gene))  
  return(genes)
}
