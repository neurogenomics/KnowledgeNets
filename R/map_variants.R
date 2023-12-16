#' @describeIn map_ map_
#' @export
map_variants <- function(gr,
                         build=c("GRCh37","GRCh38"),
                         promoter_upstream = 2000L,
                         promoter_downstream = 200L,
                         rm_nonstandard_chrom = TRUE){
  
  if(rm_nonstandard_chrom){
    gr <- remove_nonstandard_chrom(list(gr))[[1]]
  }
  GenomeInfoDb::seqlevelsStyle(gr) <- "UCSC"
  build <- match.arg(build)
  txdb <- select_txdb_build(build)
 
  regions <- list(
    intergenic = VariantAnnotation::IntergenicVariants(),
    promoter = VariantAnnotation::PromoterVariants(
      upstream = promoter_upstream,
      downstream = promoter_downstream),
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
                           region = regions[[x]]
                         )
                       }) |>
    GenomicRanges::GRangesList() |>
    unlist()
  GenomicRanges::mcols(hits)$region <- names(hits)
  
  # sort(table(hits$region))/ length(hits)*100
  return(hits)
}