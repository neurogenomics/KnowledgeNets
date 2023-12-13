dt_to_graph <- function(dat,
                        add_hover=TRUE){
  nodes <- (
    rbind(
      dat[,.(id=subject,
             name=subject,
             label=subject_label,
             category=subject_category,
             taxon=subject_taxon,
             taxon_label=subject_taxon_label)],
      dat[,.(id=object,
             name=object,
             label=object_label,
             category=object_category,
             taxon=object_taxon,
             taxon_label=object_taxon_label)]
    ) |> unique()
  )[,.SD[1],by="id"]
  #### Add db ####
  add_db(nodes, 
         input_col = "id",
         output_col = "db") 
  #### Add hoverboxes ####
  if(add_hover){
    nodes <- add_hoverboxes_dt(nodes, hoverbox_column = "title")
  }
  #### Add hoverboxes ####
  tidygraph::tbl_graph(
    nodes = nodes,
    edges = dat[,.(from=subject, 
                   to=object,
                   subject_category,
                   object_category,
                   predicate, 
                   evidence_count,
                   aggregator_knowledge_source,
                   primary_knowledge_source,
                   publications,
                   provided_by)][,link_category:=paste0(subject_category,"_",object_category)],
  )
}