dt_to_kg <- function(d,
                     c1=NULL,
                     c2=c1,
                     as_tidygraph = TRUE,
                     add_hover = FALSE){
  subject_category <- object_category <- NULL;
  if(!all(is.null(c(c1,c2)))){
    d_sub <- d[(subject_category %in% c1 & object_category %in% c2)|
               (subject_category %in% c2 & object_category %in% c1),]
  } else {
    d_sub <- d
  }
  message("object_label examples:")
  methods::show(unique(utils::head(d_sub$object_label,100))[seq(5)])
  message("subject_label examples:")
  methods::show(unique(utils::head(d_sub$subhect_label,100))[seq(5)])
  if(as_tidygraph){
    g <- dt_to_graph(d_sub,
                     add_hover=add_hover)
    igraph::vertex_attr(g,"name") <- igraph::vertex_attr(g,"id")
    return(g)
  }
  return(d_sub)
} 