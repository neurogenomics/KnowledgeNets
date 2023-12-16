#' @describeIn plot_ plot_
#' 
#' @export
plot_tidygraph <- function(g){
  
  g |> 
    tidygraph::activate("nodes") |>
    # tidygraph::sample_n(10000) |>
    visNetwork::visIgraph(layout = "layout_with_kk",
                          physics = TRUE, smooth = TRUE) |>
    visNetwork::visNodes(shape = "category") |>
    visNetwork::visInteraction(hover = TRUE)
}