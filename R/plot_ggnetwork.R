#' @describeIn plot_ plot_
#' Plot a ggnetwork.
#' @param g ggnetwork object 
#' (or an igraph/tbl_graph to be converted to ggnetwork format).
#' @inheritDotParams ggplot2::aes
#' @export
#' @examples
#' ont <- get_ontology(terms=10)
#' g <- ontology_to(ont, to="tbl_graph")
#' g <- add_hoverboxes(g)
#' out <- plot_ggnetwork(g, label_var="label")
plot_ggnetwork <- function(g,
                           colour_var = "x",
                           size_var = colour_var,
                           label_var="name", 
                           hoverbox_column = "hover",
                           interactive=TRUE,
                           show_plot=TRUE,
                           ...){

  requireNamespace("ggplot2")
  requireNamespace("ggnetwork")
  messager("Creating ggnetwork plot.")
  x <- y <- xend <- yend <- NULL;
  if(methods::is(g,"igraph")) g <- graph_to_ggnetwork(g)
  g$label <- g[[hoverbox_column]]
  plt <- ggplot2::ggplot(g,
                         ggplot2::aes(x = x,
                                      y = y,
                                      xend = xend,
                                      yend = yend,
                                      text = !!ggplot2::sym(colour_var),
                                      ...
                         )
  ) +
    ggplot2::geom_point(ggplot2::aes(
      colour = !!ggplot2::sym(colour_var),
      size = !!ggplot2::sym(size_var))
    ) +
    ggnetwork::geom_edges(color = "black",alpha=.5) +
    ggplot2::geom_text(ggplot2::aes(label = !!ggplot2::sym(label_var)),
                       color = "black") +
    ggplot2::scale_color_viridis_c(option = "magma") +
    ggplot2::scale_size(trans = "exp") +
    ggplot2::guides(size = "none") +
    ggplot2::theme_void() 

  if(isTRUE(interactive)) {
    plt <- plotly::ggplotly(plt,
                            tooltip = hoverbox_column) |>
      plotly::layout(hoverlabel = list(align = "left"))
  }
  if(isTRUE(show_plot)) methods::show(plt) 
  return(list(
    data=g,
    plot=plt
  ))
}
