#' @describeIn filter_ filter_
#' 
#' Filter the monarch knowledge graph to only include edges between specific 
#' types of nodes (e.g. Disease <--> Cell).
#' @export
#' @examples
#' g <- get_monarch_kg()
#' g2 <- filter_kg(g)
filter_kg <- function(g,
                      from_categories = paste0("biolink:",
                                                  c("Disease",
                                                    "PhenotypicFeature",
                                                    "GrossAnatomicalStructure",
                                                    "AnatomicalEntity",
                                                    "Cell")
                                                  ),
                              to_categories = from_categories,
                              edge_categories = NULL,
                              dbs=NULL,# c("mondo","HP","CL")
                              rm_isolated=TRUE,
                              as_dt=FALSE){
  category <- db <- NULL;
  len1 <- length(g)
  nodes <- g|> tidygraph::activate("nodes")|>data.table::as.data.table()
  # edges <- g|> tidygraph::activate("edges")|>data.table::as.data.table()
  # sort(table(nodes$category))
  # sort(table(edges$category))
 
  if(!is.null(from_categories) && !is.null(to_categories)){
    messager("Filtering between from/to edges.")
    g2 <- g |>
      tidygraph::activate("edges") |>
      tidygraph::filter(
        tidygraph::edge_is_between(
          from=nodes[,.I[category %in% from_categories]],
          to=nodes[,.I[category %in% to_categories]],
          ignore_dir=TRUE)
        )
  } else if(!is.null(from_categories)){
    messager("Filtering from edges.")
    g2 <- g |>
      tidygraph::activate("edges") |>
      tidygraph::filter(
        tidygraph::edge_is_from(
          from=nodes[,.I[category %in% from_categories]])
        )
  } else if(!is.null(to_categories)){
    messager("Filtering to edges.")
    g2 <- g |>
      tidygraph::activate("edges") |>
      tidygraph::filter(
        tidygraph::edge_is_to(
          to=nodes[,.I[category %in% to_categories]])
        )
  } else {
    g2 <- g
  }
  
  if(!is.null(edge_categories)){
    messager("Filtering edge_categories.")
    g2 <- g2 |> 
      tidygraph::activate("edges") |>
      tidygraph::filter(category %in% edge_categories)
  }
  if(!is.null(dbs)){
    messager("Filtering node dbs.")
    g2 <- g2 |> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(db %in% dbs) 
  }
  if(isTRUE(rm_isolated)){
    messager("Removing isolated nodes.")
    g2 <- g2 |> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(!tidygraph::node_is_isolated())
  }
  messager(formatC(length(g2),big.mark=","),"/",
           formatC(len1,big.mark=","),
           paste0("(",round(100*length(g2)/len1,1),"%)"),
           "edges remain after filtering.")
    
  # tidygraph::activate(nodes) |>
  # tidygraph::sample_n(1000) |>
  # add_hoverboxes(hoverbox_column = "title")
  if(as_dt){
    obj <- graph_to_dt(g2)
    return(obj)
    # dim(obj)
    # sort(table(obj$db.from)) 
    # table(obj$link_category)
  } else {
    return(g2)
  } 
}
