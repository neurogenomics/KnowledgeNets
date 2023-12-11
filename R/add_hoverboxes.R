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
#' g <- ontology_to_tidygraph(ont)
#' g2 <- add_hoverboxes(g)
add_hoverboxes <- function(g,
                           columns = get_tidygraph_colnames(g),
                           interactive = TRUE,
                           width = 60,
                           digits = 3,
                           decorators = c("<b>","</b>"),
                           force_new = FALSE) {
  hover <- name <- NULL;
  #### Check for hover col ####
  if("hover" %in% names(igraph::vertex.attributes(g)) && 
     isFALSE(force_new)){
    messager("'hover' column already exists. Skipping hoverbox creation.")
    return(g)
  } 
  #### Select sep ####
  sep <- if(isTRUE(interactive)) "<br>" else "\n"
  #### Check columns ####
  col_opts <- get_tidygraph_colnames(g)
  columns <- columns[unname(columns) %in% col_opts]
  ##### Proceed ####
  if(length(columns)==0){
    messager("No columns found. Skipping hoverbox creation.")
  } else {
    messager("Making hoverboxes from:",paste(shQuote(columns),collapse = ", "))
    nodes <- tidygraph_to_dt(g)
    #### Check for id col ####
    if(!"name" %in% names(nodes)){
      nodes[,name:=.I]
    }
    ## Define helper functions
    interleave <- function(a,b,sep){
      idx <- order(c(seq_along(a), seq_along(b)))
      unlist(c(a,paste0(b,sep)))[idx] |> as.list()
    }
    round_if <- function(x,digits,width){
      if(is.numeric(x)) {
        round(x,digits = digits)
      } else {
        stringr::str_wrap(x, width = width)
      }
    }
    #### Create hoverboxes....data.table style! ####
    nodes[, hover := do.call(paste0,
                             interleave(a = paste0(decorators[1],
                                                   columns,
                                                   decorators[2],"= "),
                                        b = lapply(.SD,
                                                   round_if,
                                                   digits = digits,
                                                   width = width),
                                        sep = sep)
                             ), 
          .SDcols = columns, by="name"] 
    # cat(nodes$hover[1])
    #### Add hover back into graph ####
    igraph::V(g)$hover <- nodes$hover[match(igraph::V(g)$name,nodes$name)]
  } 
  return(g)
}
