#' @describeIn map_ map_
#' Map Monarch genes
#'
#' Map Monarch gene IDs to HGNC gene symbols, within or across species.
#' @param dat \link[data.table]{data.table} with genes.
#' @param gene_col Name of the gene column in \code{dat}.
#' @param map_by_merge Map orthologs by merging the node data such that the 
#' orthologous genes will appear as a new column (\code{TRUE}). 
#' Otherwise, the orthologs will be added as new nodes to the graph 
#' (\code{FALSE}).
#' @inheritParams data.table::merge.data.table
#' @returns Mapped \code{dat}
#'
#' @export
#' @examples
#' dat <- example_dat("gene")
#' dt2 <- map_genes_monarch(dat=dat, gene_col="gene")
map_genes_monarch <- function(dat,
                              gene_col,
                              as_graph=methods::is(dat,"tbl_graph"),
                              map_by_merge=FALSE,
                              all.x=FALSE){
  #### Prepare orthology map ####
  homol <- get_monarch_homol(as_graph = as_graph)
  
  if(!is.null(dat)){
    if(isTRUE(as_graph)){ 
      if(map_by_merge){
        dat_homol <- data.table::merge.data.table(
          graph_to_dt(dat, what = "nodes"),
          graph_to_dt(homol),
          by.x="id",
          by.y="object"
        )
      } else {
        dat_homol <- tidygraph::graph_join(dat,homol)
      }
      
      return(dat_homol)
    } else{
      dat_homol <- data.table::merge.data.table(
      dat,
      homol[,c("hgnc","hgnc_label","gene")],
      by.x=gene_col,
      by.y="gene",
      all.x = all.x)
    messager(formatC(nrow(dat_homol),big.mark = ","),"/",
             formatC(nrow(dat),big.mark = ","),
             "rows remain after gene orthology mapping.")
    return(dat_homol)
    }
  } else {
    return(homol)
  }
  
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