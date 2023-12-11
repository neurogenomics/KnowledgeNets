#' @describeIn map_ map_
#' Map Monarch genes
#'
#' Map Monarch gene IDs to HGNC gene symbols, within or across species.
#' @param dat \link[data.table]{data.table} with genes.
#' @param gene_col Name of the gene column in \code{dat}.
#' @inheritParams data.table::merge.data.table
#' @returns Mapped \code{dat}
#'
#' @export
#' @examples
#' dat <- example_dat("gene")
#' dt2 <- map_monarch_genes(dat=dat, gene_col="gene")
map_monarch_genes <- function(dat,
                              gene_col,
                              all.x=FALSE){
  subject <- hgnc <- hgnc_label <- gene <- gene_label <- NULL;

  #### Prepare orthology map ####
  {
    homol <- get_monarch("gene_homolog")[[1]]
    messager("Unique species with orthologs:",
             data.table::uniqueN(homol$subject_taxon_label))
    ## Subset to only convert human --> non-human
    homol <- homol[grep("^HGNC",subject),] |>
      data.table::setnames(
        c("subject","subject_label","object","object_label"),
        c("hgnc","hgnc_label","gene","gene_label"))
    ## Add human-to-human back into map
    hhomol <- (homol[subject_taxon_label=="Homo sapiens",
                     c("hgnc","hgnc_label",
                        "subject_taxon","subject_taxon_label")] |> unique()
    )[,gene:=hgnc][,gene_label:=hgnc_label]
    homol <- data.table::rbindlist(list(homol,hhomol),fill=TRUE)
    ## Add HGNC IDs
    hhomol[,gene:=hgnc_label]
    homol <- data.table::rbindlist(list(homol,hhomol),fill=TRUE)
    ## Make unique
    homol <- unique(homol)
    messager("Unique orthologs:",
             formatC(data.table::uniqueN(homol$gene),big.mark = ","))
  }
  {
    dt_homol <- data.table::merge.data.table(
      dat,
      homol[,c("hgnc","hgnc_label","gene")],
      by.x=gene_col,
      by.y="gene",
      all.x = all.x)
    messager(formatC(nrow(dt_homol),big.mark = ","),"/",
             formatC(nrow(dat),big.mark = ","),
             "rows remain after gene orthology mapping.")
  }
  return(dt_homol)
}
## Orthogene
# else if(gene_map_method=="orthogene"){
#   requireNamespace("orthogene")
#   homol <- lapply(stats::setNames(unique(genes$subject_taxon_label),
#                                   unique(genes$subject_taxon_label)),
#                   function(x){
#
#                     input_species <- if(x=="Xenopus laevis"){
#                       "Xenopus"
#                     } else if(x=="Caenorhabditis elegans"){
#                       "celegans"
#                     } else {
#                       x
#                     }
#                     orthogene::convert_orthologs(
#                       gene_df = genes[subject_taxon_label==x,]$subject_label|>
#                         unique(),
#                       # gene_input = "subject_label",
#                       gene_output = "columns",
#                       input_species = input_species,
#                       non121_strategy = "kbs",
#                       output_species = "human")
#                   }) |>
#     data.table::rbindlist(fill=TRUE, idcol = "subject_taxon_label") |>
#     data.table::setkeyv(c("subject_taxon_label","input_gene"))
#   data.table::setnames(homol,"input_gene","subject_label")
#   message("Unique orthologs: ",
#           data.table::uniqueN(
#             homol[subject_taxon_label!="Homo sapiens"]$subject_label))
# }
