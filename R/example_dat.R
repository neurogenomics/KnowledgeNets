#' @describeIn utils_ utils_
#' Example data
#' 
#' Construct an example \link[data.table]{data.table} containing diseases, 
#' phenotype, and/or genes.
#' @param types Types of IDs to include in the data.
#' @returns \link[data.table]{data.table}
#' 
#' @export
#' @examples
#' dat <- example_dat()
example_dat <- function(types=c("omim_id",
                                "orphanet_id",
                                "decipher_id",
                                "hpo_id",
                                "gene")){
  # p2g <- HPOExplorer::load_phenotype_to_genes()
  # add_db(dat = p2g, input_col = "disease_id", output_col = "db")
  # cat(paste(paste0(shQuote(sample(p2g[db=="OMIM"]$disease_id,5))),collapse = ","))
  idl <- c() 
  if("omim_id" %in% types){
    idl <- c(idl,c('OMIM:607326','OMIM:105210','OMIM:616728',
                   'OMIM:612260','OMIM:300978'))
  }
  if("orphanet_id" %in% types){
    idl <- c(idl,c('ORPHA:293181','ORPHA:64280','ORPHA:110',
                   'ORPHA:522077','ORPHA:508533'))
  }
  if("decipher_id" %in% types){
    idl <- c(idl,c("DECIPHER:48","DECIPHER:4","DECIPHER:54",
                   "DECIPHER:29","DECIPHER:58"))
  }
  if("hpo_id" %in% types){
    idl <- c(idl,c("HP:0025115","HP:0025114","HP:0025112","HP:0002948"))
  }
  if("gene" %in% types){
    idl <- c(idl,c('MYCN','IL15','HGNC:25933','YRDC','KCNJ8','ODAD2'))
  }
  id_col <- if(length(types)>1) 'id' else types
  dat <- data.table::data.table(idl) |> `colnames<-`(id_col)
  
  add_db(dat = dat,
         input_col = id_col, 
         output_col = "db")
  return(dat)
}