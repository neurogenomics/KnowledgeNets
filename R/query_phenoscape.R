#' Query Phenoscape
#' 
#' Query \href{https://phenoscape.org/}{Phenoscape}.
#' @source \href{https://github.com/phenoscape/rphenoscape/}{rphenoscape}
query_phenoscape <- function(){
  requireNamespace("rphenoscape")
  
  #### Gather all phenotype IDs ####
  # p2g <- get_monarch("gene_phenotype")[[1]]
  # phenos <- data.table::fread("https://kb.phenoscape.org/api/v2-beta/phenotype/query?historical_homologs=false&serial_homologs=false&offset=0&limit=0&parts=false")
  terms <- rphenoscape::find_term(query = NA,
                                  limit = NA)
  terms$db <- stringr::str_split(basename(terms$id),"_",simplify = TRUE)[,1]
  # sort(table(terms$db))
  
  # nex <- rphenoscape::get_term_iri(text = "hippocampus",
  #                                  as = "UBERON")   
  
  # #### List desired taxa ####
  # species <- orthogene::all_species(method = "homologene")$scientific_name 
  # taxon <- rphenoscape::taxon_info(term = species)
  # taxon <- taxon[!is.na(taxon$id),]
  # nexl <- lapply(taxon$id, function(tx){
  #   message('Querying:',tx)
  #   nex <- rphenoscape::get_phenotypes(taxon  = tx)
  #   message("ids returned: ",length(nex$id))
  #   nex
  # })
  # ana <- rphenoscape::anatomy_term_info(term = "hippocampus")
  # iri <- rphenoscape::get_term_iri(text = "hippocampus", 
  #                                  as = "uberon")
  
  # nex <- rphenoscape::get_ontotrace_data(taxon = c("Ictalurus", "Ameiurus"), 
  #                                        entity = "fin spine")
  # nex <- rphenoscape::get_ontotrace_data(taxon = tax$label, 
  #                                        entity =  terms$id[1:10],
  #                                        strict = TRUE)
  # rphenoscape::jaccard_similarity()
  # length(nex@otus[[1]]@otu)
  # m <- rphenoscape::get_char_matrix(nex)
  # meta <- rphenoscape::get_char_matrix_meta(nex)  

 
  #### Species-Species similarity matrix ####
  # Xs <- rphenoscape::jaccard_similarity(terms = stats::na.omit(tax$id),
  #                                       .colnames = "ID")
  #### Phenotype-Phenotype similarity matrix ####
  Xp <- rphenoscape::jaccard_similarity(terms = stats::na.omit(terms$id),
                                        .colnames = "ID")
  Xp <- methods::as(Xp,"sparseMatrix")
  # obj <- scKirby::process_seurat(obj = Xp)
  # saveRDS(Xp,"~/Downloads/phenoscape_Xp.rds")
  
}