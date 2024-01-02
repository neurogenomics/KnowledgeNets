#' @describeIn filter_ filter_
#' Remove non-standard chromosomes
#' 
#' Remove non-standard chromosomes from a list of 
#' \link[GenomicRanges]{GRanges} objects.
#' @returns Named list of \link[GenomicRanges]{GRanges} objects.
#' @keywords internal
filter_chromosomes <- function(grlist,
                               keep_chr = paste0(
                                 "chr",c(seq_len(22),"X","Y")
                               )){
  requireNamespace("GenomeInfoDb")
  requireNamespace("BiocGenerics")
  #### Check if keep_chr is NULL ####
  if(is.null(keep_chr)) return(grlist)
  messager("Removing non-standard chromosomes.")
  #### Check if it's a single element ####
  tmp_list <- FALSE
  grlist <- lapply(grlist, function(gr){ 
    suppressMessages(suppressWarnings(
      GenomeInfoDb::seqlevelsStyle(gr) <- "UCSC"
    ))
    gr <- gr[BiocGenerics::`%in%`(GenomeInfoDb::seqnames(gr),
                                  keep_chr)]
    GenomeInfoDb::seqlevels(gr) <- GenomeInfoDb::sortSeqlevels(
      GenomeInfoDb::seqlevelsInUse(gr)
    )
    #update seq lengths
    GenomeInfoDb::seqlevels(
      gr, pruning.mode="coarse") <- keep_chr
    return(gr)
  })
  if(tmp_list) grlist <- grlist[[1]]
  return(grlist)
}
