#' @describeIn get_ get_
#' Get knowledge graph: Monarch
#' 
#' Imports the entire Monarch knowledge graph containing >500,000 nodes 
#' and >10,000,000 edges across many categories 
#' (e.g. Disease, Phenotypes, Cell Types, etc.).
#' 
#' Option 1: Use the \href{https://api.monarchinitiative.org/api/}{biolink API}
#'  to efficiently extract specific subset of data
#' from the Monarch server.
#' Option 2: Import the entire knowledge graph from the
#' \href{https://data.monarchinitiative.org/monarch-kg/latest/}{Monarch server}.
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/37707514/}{BioThings Explorer}
#' @source \href{https://rdrr.io/github/frequena/rbiolink/}{rbiolink}
#' @source \href{https://github.com/biolink/kgx}{KGX}
#' @inheritDotParams data.table::fread
#' @export
#' @examples
#' \dontrun{
#' g <- get_monarch_kg(save_dir=tempdir(), nrows=100)
#' }
get_monarch_kg <- function(as_graph=TRUE,
                           save_dir=cache_dir(),
                           force_new=FALSE,
                           ...){
  files <- get_monarch_files(subdir = "monarch-kg/latest/",
                             queries = "\\.tsv\\.gz")
  save_path <- file.save_path(
    save_dir, 
    paste0(gsub("\\.tsv\\.gz","",basename(files$url[1])),".rds"))
  if(file.exists(save_path) & 
     isFALSE(force_new)){
    messager("Importing",save_path)
    g <- readRDS(save_path)
    return(g)
  } else {
    d <- data.table::fread(files$url[1],
                           tmpdir = save_dir,
                           ...)
    if(isFALSE(as_graph))  return(d)
    g <- dt_to_kg(d)
    cache_save(g,save_path)
  }
  return(g)
}

#### Query neo4j graph ####
# con <- neo4r::neo4j_api$new(
#   url = "http://localhost:7474", 
#   user = "neo4j", 
#   password = "plop"
# )
# jsonlite::read_json("https://api.monarchinitiative.org/api/bioentity/mondo:0012990")
# neo2R::graphRequest(graph = ,
#                     endpoint = "/bioentity/",
#                     postText = "mondo:0012990")
# neo <- neo2R::graphRequest("https://data.monarchinitiative.org/monarch-kg-dev/latest/monarch-kg.neo4j.dump",
#                            endpoint = "/bioentity/",
#                            postText = "mondo:0012990")

