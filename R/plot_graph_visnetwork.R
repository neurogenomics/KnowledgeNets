#' @describeIn plot_ plot_
#' @export
plot_graph_visnetwork <- function(g){
  g |> 
    tidygraph::activate("nodes") |>
    # tidygraph::sample_n(10000) |>
    visNetwork::visIgraph(layout = "layout_with_kk",
                          physics = TRUE, 
                          smooth = TRUE) |>
    visNetwork::visNodes(shape = "category") |>
    visNetwork::visInteraction(hover = TRUE)
}