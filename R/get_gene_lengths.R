#' @describeIn get_ get_
#' @param genes A character vector of gene symbols
#' @param ensembl_version Which Ensembl database version to use.
#' @export
get_gene_lengths <- function(genes,
                             keep_chr = c(seq(22),"X","Y"),
                             ensembl_version = 75){

  requireNamespace("ensembldb")
  requireNamespace("AnnotationFilter")

  genes <- unique(genes)
  messager("Gathering metadata for",length(genes),"unique genes.")
  if(is.null(keep_chr)){
    keep_chr <- eval(formals(get_gene_lengths)$keep_chr)
  }
  #### Standardise seqnames ####
  keep_chr <- unique(
    c(tolower(keep_chr),
      paste0("chr",keep_chr),
      keep_chr)
  )
  #### Get gene reference database ####
  if(ensembl_version==75){
    #### Outdated ref ####
    requireNamespace("EnsDb.Hsapiens.v75")
    txdb <- EnsDb.Hsapiens.v75::EnsDb.Hsapiens.v75
  } else {
    #### Updated ref ####
    requireNamespace("AnnotationHub")
    hub <- AnnotationHub::AnnotationHub()
    q <- AnnotationHub::query(hub, c("EnsDb",ensembl_version,"sapiens"))
    txdb <- q[[length(q)]]
  }
  #### Set
  tx_gr <- ensembldb::genes(
    txdb,
    columns = c(ensembldb::listColumns(txdb, "gene"), "entrezid"),
    filter=AnnotationFilter::AnnotationFilterList(
      AnnotationFilter::SymbolFilter(value = genes),
      AnnotationFilter::SeqNameFilter(value = keep_chr)
    ))
  return(tx_gr)
}
