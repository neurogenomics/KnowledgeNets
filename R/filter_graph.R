#' @describeIn filter_ filter_
#' Filter a \link[tidygraph]{tbl_graph}.
#' @export
filter_graph <- function(g,
                         node_filters=list(),
                         edge_filters=list(),
                         rm_isolated=TRUE,
                         size=NULL){
  ##### Filter nodes #####
  for(nm in names(node_filters)){
    f <- node_filters[[nm]]
    if(is.null(f)){
      next
    }
    g <- g|> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(get(eval(nm)) %in% f)
  }
  ##### Filter edges #####
  for(nm in names(edge_filters)){
    f <- edge_filters[[nm]]
    if(is.null(f)){
      next
    }
    g <- g|> 
      tidygraph::activate("edges") |>
      tidygraph::filter(get(eval(nm)) %in% f)
  }
  #### Sample graph ####
  if(!is.null(size)){
    g <- tidygraph::sample_n(g,size = min(size,length(g)))
  }
  #### Remove isolated nodes ####
  if(isTRUE(rm_isolated)){
    messager("Removing isolated nodes.")
    g <- g |> 
      tidygraph::activate("nodes") |>
      tidygraph::filter(!tidygraph::node_is_isolated())
  }
  return(g)
}