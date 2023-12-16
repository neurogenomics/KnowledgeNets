select_txdb_build <- function(ref_genome) {
  if (toupper(ref_genome) %in% c("HG19", "GRCH37")) {
    messager("Selecting TxDb.Hsapiens.UCSC.hg19.knownGene.")
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene::TxDb.Hsapiens.UCSC.hg19.knownGene
  } else if (toupper(ref_genome) == "GRCH38") {
    messager("Selecting TxDb.Hsapiens.UCSC.hg38.knownGene")
    txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene::TxDb.Hsapiens.UCSC.hg38.knownGene
  } else {
    stopper("ref_genome must be 'GRCh37' or 'GRCh38'")
  }
  return(txdb)
}
