#' @describeIn convert_ convert_
#' Convert ontology
#'
#' Convert an  \link[simona]{ontology_DAG} to
#' a number of other useful formats. 
#' @export 
#' @importFrom stats as.dist hclust cutree
#' @examples
#' ont <- get_ontology()
#' obj <- ontology_to(ont=ont, to="dendrogram")
ontology_to <- function(ont,
                        terms=ont@terms,
                        remove_terms=grep(":",terms,
                                          invert = TRUE, value = TRUE),
                        to=c("adjacency",
                             "adjacency_dist",
                             "adjacency_dist_hclust",
                             "adjacency_dist_hclust_dendrogram",
                             "adjacency_dist_hclust_clusters",
                             "similarity",
                             "dendrogram",
                             "igraph",
                             "dot",
                             "adjacency_igraph",
                             "igraph_dist",
                             "igraph_dist_hclust",
                             "igraph_dist_hclust_dendrogram",
                             "tidygraph",
                             "data.frame",
                             "data.table",
                             "list"),
                        as_sparse=FALSE,
                        ...){
  to <- match.arg(to)
  ont <- filter_ontology(ont,
                         terms = terms,
                         remove_terms = remove_terms)
  if(to=="adjacency"){
    obj <- ontology_to_adjacency(ont)
  } else if(to=="adjacency_dist"){
    adj <- ontology_to(ont, to="adjacency")
    # obj <- stats::dist(adj) ### seems to take forever
    obj <- stats::as.dist(abs(adj-max(adj)))
  } else if(to=="adjacency_dist_hclust"){
    d <- ontology_to(ont, to="dist")
    obj <- stats::hclust(d)
  } else if(to=="adjacency_dist_hclust_dendrogram"){
    dh <- ontology_to(ont, to="dist_hclust")
    obj <- stats::as.dendrogram(dh)
  } else if(to=="dendrogram"){
    ont2 <- simona:::dag_treelize(ont)
    obj <- simona::dag_as_dendrogram(ont2)
  } else if(to=="dot"){
    obj <- simona::dag_as_DOT(ont, ...)
  } else if(to=="similarity"){
    obj <- simona::term_sim(ont, terms=ont@terms, ...)
  } else if(to=="adjacency_dist_hclust_clusters"){ 
    hc <- ontology_to(ont, to="adjacency_dist_hclust")
    obj <- stats::cutree(hc, ...)
  } else if(to=="igraph"){
    obj <- simona::dag_as_igraph(ont)
  } else if(to=="adjacency_igraph"){
    adj <- ontology_to(ont, to="adjacency")
    obj <- igraph::graph_from_adjacency_matrix(adj, ...)
  } else if (to=="igraph_dist"){
    g <- ontology_to(ont, to="igraph")
    obj <- igraph::distances(g, ...)
  } else if(to=="igraph_dist_hclust"){
    gd <- ontology_to(ont, to="igraph_dist")
    if(any(is.infinite(gd))) gd[is.infinite(gd)] <- max(gd[!is.infinite(gd)])
    obj <- stats::hclust(stats::as.dist(gd), ...)
  } else if(to=="igraph_dist_hclust_dendrogram"){
    gdh <- ontology_to(ont, to="igraph_dist_hclust")
    obj <- stats::as.dendrogram(gdh)
  } else if(to=="tidygraph"){ 
    obj <- ontology_to_tidygraph(ont, ...)
  } else if(to=="data.frame"){
    g <- ontology_to_tidygraph(ont)
    obj <- tidygraph_to_dt(g)
  } else if(to=="data.table"){
    df <- ontology_to(ont, to="data.frame")
    obj <- data.table::as.data.table(df)
  } else if(to=="list") {
    obj <- list(
      similarity=ontology_to(ont, to = "similarity"), 
      adjacency=ontology_to(ont, to = "adjacency"),
      elementMetadata=data.table::data.table(ont@elementMetadata),
      annotation=ont@annotation,
      terms=ont@terms
    )
  } else {
    stop("Unknown conversion type.")
  }
  #### Convert to sparse ####
  if(isTRUE(as_sparse)){
    if(methods::is(obj,"matrix")){
      obj <- Matrix::Matrix(adj, sparse=TRUE)
    }
  }
  ## Report
  messager("Converted ontology to:",to,
           if(as_sparse) paste0("(sparse)") else NULL
             )
  return(obj)
}
