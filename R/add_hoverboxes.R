#' Make hoverboxes
#'
#' A hoverbox is a box of text that shows up when the cursor
#'  hovers over something.
#' These can be useful when making interactive network plots
#' of the HPO phenotypes because we can include a hoverbox that gives
#' information and data associated with each phenotype.
#' @param force_new Force new hoverbox creation even when the "hover" column 
#' already exists.
#' @inheritParams base::round
#' @inheritParams stringr::str_wrap
#' @returns tidygraph with additional metadata column named "hover".
#'
#' @export
#' @importFrom stringr str_wrap
#' @examples
#' ont <- get_ont("hpo")
#' g <- ontology_to_graph(ont)
#' g2 <- add_hoverboxes(g)
add_hoverboxes <- function(g,
                           columns = get_graph_colnames(g),
                           hoverbox_column=c("hover",
                                             "title",# For visNetwork
                                             "label"# For plotly
                           ),
                           width = 60,
                           digits = 3,
                           decorators = c("<b>","</b>"),
                           as_html = TRUE,
                           force_new = FALSE) {
  hoverbox_column <- match.arg(hoverbox_column)
  #### Check for hover col ####
  if(hoverbox_column %in% names(igraph::vertex.attributes(g)) && 
     isFALSE(force_new)){
    messager(sQuote(hoverbox_column),
             "column already exists. Skipping hoverbox creation.")
    return(g)
  } 
  #### Check columns ####
  col_opts <- get_graph_colnames(g)
  columns <- columns[unname(columns) %in% col_opts]
  ##### Proceed ####
  if(length(columns)==0){
    messager("No columns found. Skipping hoverbox creation.")
  } else {
    messager("Making hoverboxes from:",paste(shQuote(columns),collapse = ", "))
    nodes <- graph_to_dt(g)
    nodes <- add_hoverboxes_dt(dat = nodes,
                               columns = columns,
                               hoverbox_column = hoverbox_column,
                               width = width,
                               digits = digits,
                               decorators = decorators,
                               as_html = as_html)
    #### Add hover back into graph ####
    igraph::vertex_attr(g, hoverbox_column) <- 
      nodes[[hoverbox_column]][match(igraph::V(g)$name,nodes$name)]
  } 
  return(g)
}
