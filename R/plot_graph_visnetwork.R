#' @describeIn plot_ plot_
#' Plot graph using visNetwork.
#' @param add_visExport Add PDF download button.
#' @inheritParams map_colors
#' @inheritParams visNetwork::visIgraph
#' @inheritParams visNetwork::visIgraphLayout
#' @inheritParams visNetwork::visPhysics
#' @inheritParams visNetwork::visNodes
#' @inheritParams visNetwork::visEdges
#' @inheritParams visNetwork::visOptions
#' @inheritParams visNetwork::visNetwork
#' @export
#' @examples
#' ont <- get_ontology("hp", terms=10)
#' g <- ontology_to(ont, to="tbl_graph")
#' out <- plot_graph_visnetwork(g)
plot_graph_visnetwork <- function(g,
                                  label_var = "name",
                                  size_var = "degree",
                                  colour_var = size_var,
                                  invert_colour_var = TRUE,
                                  columns = get_graph_colnames(g),
                                  preferred_palettes = NULL,
                                  selectedBy = label_var,
                                  show_plot = TRUE,
                                  layout = "layout_with_kk",
                                  solver = "forceAtlas2Based",
                                  physics = FALSE,
                                  forceAtlas2Based = list(
                                    avoidOverlap = 0.5, 
                                    gravitationalConstant = -50),
                                  scaling = NULL,
                                  arrows = "from",
                                  smooth = list(enabled = TRUE, 
                                                type = "cubicBezier", 
                                                roundness = 0.5),
                                  add_visExport = FALSE,
                                  degree = 1,
                                  width = "100%",
                                  height = "90vh",
                                  highlight_color="#00FFFFCF",
                                  randomSeed=2024,
                                  main=NULL,
                                  submain=NULL,
                                  save_path=tempfile(
                                    fileext = "_visnetwork.html")){
  requireNamespace("visNetwork") 
  requireNamespace("tidygraph")
  requireNamespace("igraph")
  . <- NULL;
  
  messager("Creating visNetwork plot.")
  g <- to_graph(g)
  #### Set metadata columns ####
  g <- add_hoverboxes(g,
                      columns = columns,
                      hoverbox_column = "title")
  igraph::vertex_attr(g,"degree") <- igraph::degree(g)
  if(size_var %in% get_graph_colnames(g)){
    igraph::vertex_attr(g,"value") <- igraph::vertex_attr(g,size_var)
    if(invert_colour_var){
      igraph::vertex_attr(g,"value") <- max(igraph::vertex_attr(g,"value")) -
        igraph::vertex_attr(g,"value")+1  
    }
  } 
  igraph::vertex_attr(g,"name") <- igraph::vertex_attr(g,label_var)
  igraph::vertex_attr(g,"color") <- map_colors(
    dat = g,
    columns = colour_var,
    preferred_palettes = preferred_palettes,
    as="vector")[[1]] |> unname()
  
  #### Create plot #### 
  # g |>
  #   visNetwork::visIgraph() |>
  #   visNetwork::visNodes(...) |>
  #   visNetwork::visInteraction(hover = TRUE)
  visnet <- visNetwork::toVisNetworkData(g) %>%
    {
      do.call(visNetwork::visNetwork,
              c(., list(main = main,
                        height = height,
                        width = width,
                        submain = submain,
                        background = "transparent")
              )
      )
    } |>
    # visNetwork::visIgraph(g,
    #                               randomSeed = randomSeed) |>
    visNetwork::visIgraphLayout(layout = layout,
                                type = "full",
                                randomSeed = randomSeed,
                                physics = physics) |>
    visNetwork::visPhysics(solver=solver,
                           forceAtlas2Based=forceAtlas2Based,
                           enabled = physics) |>
    visNetwork::visNodes(font = list(color="#F0FFFF",
                                     strokeWidth=2,
                                     strokeColor="rgba(0,0,0,1)"
    ),
    shadow = list(enabled=TRUE,
                  size = 10),
    opacity = 0.75,
    borderWidth=3,
    borderWidthSelected=6,
    color = list(hover = list(background="rgba(0,0,0,.5)"),
                 highlight = list(background=highlight_color,
                                  border=highlight_color)
    ),
    scaling = scaling
    ) |>
    visNetwork::visEdges(shadow = list(enabled=FALSE),
                         smooth = smooth,
                         arrows = arrows,
                         color = list(opacity = 0.5)) |>
    # visNetwork::visLegend() |>
    # visNetwork::visClusteringByConnection(nodes = unique(top_targets[[group_var]])) |>
    # visNetwork::visGroups(groupname = unique(igraph::vertex_attr(g,"group"))[[2]],
    #                       color="green")
    # visNetwork::visClusteringByGroup(groups = igraph::vertex_attr(g,"group"))
    visNetwork::visInteraction(hover = TRUE) |>
    visNetwork::visOptions(
      selectedBy = selectedBy,
      highlightNearest = list(enabled=TRUE,
                              degree=degree)) |>
    visNetwork::visEvents(type = "on",
                          doubleClick = "function(){ this.fit()}")
  
  if(isTRUE(add_visExport)){
    visnet <-  visnet |> visNetwork::visExport(type = "pdf")
  }
  #### Save network ####
  plot_save(plt=visnet, 
            save_path=save_path) 
  return(list(
    data=g,
    plot=visnet
  ))
}
