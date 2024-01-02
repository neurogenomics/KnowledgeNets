#' @describeIn map_ map_
#' @export
#' @examples
#' if(interactive()){
#' gr <- GenomicRanges::GRanges("1:100-10000")
#' hits <- map_variants(gr)
#' }
map_variants <- function(gr,
                         build=c("GRCh37","GRCh38"),
                         upstream = 2000L,
                         downstream = 200L,
                         keep_chr = paste0(
                           "chr",c(seq_len(22),"X","Y")
                         ),
                         ignore.strand = TRUE){
  requireNamespace("VariantAnnotation")
  requireNamespace("GenomeInfoDb")
  requireNamespace("GenomicRanges")
  gr <- filter_chromosomes(list(gr), 
                                 keep_chr=keep_chr)[[1]]
  GenomeInfoDb::seqlevelsStyle(gr) <- "UCSC"
  build <- match.arg(build)
  txdb <- select_txdb_build(build)
 
  regions <- list(
    intergenic = VariantAnnotation::IntergenicVariants(),
    promoter = VariantAnnotation::PromoterVariants(upstream = upstream,
                                                   downstream = downstream),
    coding = VariantAnnotation::CodingVariants(),
    intron = VariantAnnotation::IntronVariants(),
    threeUTR = VariantAnnotation::ThreeUTRVariants(),
    fiveUTR  = VariantAnnotation::FiveUTRVariants(),
    splicesite = VariantAnnotation::SpliceSiteVariants()
  ) 
  hits <- lapply(stats::setNames(names(regions),
                                 names(regions)), 
                       function(x) {
                         messager("Querying regions: ", x, parallel = TRUE)
                         VariantAnnotation::locateVariants(
                           query = gr,
                           subject = txdb,
                           region = regions[[x]],
                           ignore.strand = ignore.strand
                         )
                       }) |>
    GenomicRanges::GRangesList() |>
    unlist()
  GenomicRanges::mcols(hits)$region <- names(hits)
  # sort(table(hits$region))/ length(hits)*100
  return(hits)
}
