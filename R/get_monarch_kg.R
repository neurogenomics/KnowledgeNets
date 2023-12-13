#' @describeIn get_monarch_ get_
#' Get knowledge graph: Monarch
#' 
#' Option 1: Use the \href{https://api.monarchinitiative.org/api/}{biolink API}
#'  to efficiently extract specific subset of data
#' from the Monarch server.
#' Option 2: Import the entire knowledge graph from the
#' \href{https://data.monarchinitiative.org/monarch-kg/latest/}{Monarch server}.
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/37707514/}{BioThings Explorer}
#' @source \href{https://rdrr.io/github/frequena/rbiolink/}{rbiolink}
#' @source \href{https://github.com/biolink/kgx}{KGX}
get_monarch_kg <- function(){
  ### Option 1 ####
  #
  # con <- neo4r::neo4j_api$new(
  #   url = "http://localhost:7474", 
  #   user = "neo4j", 
  #   password = "plop"
  # )
  # jsonlite::read_json("https://api.monarchinitiative.org/api/bioentity/MONDO:0012990")
  # neo2R::graphRequest(graph = ,
  #                     endpoint = "/bioentity/",
  #                     postText = "MONDO:0012990")
  # neo <- neo2R::graphRequest("https://data.monarchinitiative.org/monarch-kg-dev/latest/monarch-kg.neo4j.dump",
  #                            endpoint = "/bioentity/",
  #                            postText = "MONDO:0012990")

  files <- get_monarch_files(subdir = "monarch-kg/latest/",
                             queries = "\\.tsv\\.gz")
  ### Option 2 ####
  d <- data.table::fread(files$url[1])
  cells <- monarchr::biolink_search("biolink:Cell")
  # return(d)
  
  query_link <- function(d,
                         c1,
                         c2=c1,
                         as_tidygraph = TRUE,
                         add_hover = FALSE){
    d_sub <- d[(subject_category %in% c1 & object_category %in% c2)|
               (subject_category %in% c2 & object_category %in% c1),]
    message("object_label examples:")
    print(head(d_sub$object_label))
    message("subject_label examples:")
    print(head(d_sub$subject_label))
    if(as_tidygraph){
      d_sub <- dt_to_graph(d_sub,
                           add_hover=add_hover)
    }
    d_sub
  } 
  categories <- paste0("biolink:",c("Disease",
                                    "PhenotypicFeature",
                                    "GrossAnatomicalStructure",
                                    "AnatomicalEntity",
                                    "Cell"))
  g <- query_link(d,c1=categories) 
  nodes <- g|> tidygraph::activate(nodes)|>data.table::as.data.table()
  edges <- g|> tidygraph::activate(edges)|>data.table::as.data.table()
  sort(table(nodes$category))
  sort(table(edges$link_category))
  
  # g <- Reduce(function(x,y){tidygraph::full_join(x,y,copy = TRUE)},list(a2c,c2c,d2a,d2d,a2a))
  g2 <- g |>
    tidygraph::activate(edges) |>
    tidygraph::filter(
      tidygraph::edge_is_between(from=nodes[,.I[category %in% c("biolink:Disease","biolink:PhenotypicFeature")]],
                                 to=nodes[,.I[category %in% c("biolink:Cell")]],
                                 ignore_dir=TRUE)) |>
    tidygraph::activate(nodes) |>
    tidygraph::filter(db %in% c("MONDO","HP","CL")) |>
    tidygraph::filter(!tidygraph::node_is_isolated())
    # tidygraph::activate(nodes) |>
    # tidygraph::sample_n(1000) |>
    # add_hoverboxes(hoverbox_column = "title")
  obj <- tidygraph_to_dt(g2) 
  dim(obj)
  sort(table(obj$db.from)) 
  table(obj$link_category)
  
  g2 |> 
    tidygraph::activate(nodes) |>
    # tidygraph::sample_n(10000) |>
  visNetwork::visIgraph(layout = "layout_with_kk",
                        physics = TRUE, smooth = TRUE) |>
    visNetwork::visNodes(shape = "category") |>
    visNetwork::visInteraction(hover = TRUE)
  # #### Use evidence_count as connection weights ####
  # round(table(d$evidence_count,useNA = "always")/nrow(d)*100,6)
}
