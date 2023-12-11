#' Query ubergraph
#' 
#' Query Ubergraph: an integrated multi-ontology knowledge graph.
#' @source \href{https://github.com/INCATools/ubergraph}{ubergraph}
#' @source \href{https://github.com/INCATools/ubergraph/tree/master/sparql}{
#' ubergraoh sparql query functions}
#' @source \href{https://github.com/phenoscape/ubeRsim}{ubeRsim}
#' @source \href{https://github.com/dhimmel/obonet}{obonet}
query_ubergraph <- function(){
  requireNamespace("sparklyr")
  
  #### Install system deps: Spark ####
  # v <- rev(unlist(
  #   sparklyr::spark_available_versions()
  # ))[[1]]
  # sparklyr::spark_install(version = v)
  # sparklyr::spark_disconnect(sc)
  sc <- sparklyr::spark_connect(master = "local")
  sc <- sparklyr::spark_connect(master = "https://ubergraph.apps.renci.org/sparql")
  
  sc <- sparklyr::spark_connect(master = "spark:/https://ubergraph.apps.renci.org/sparql:7077")
  
}