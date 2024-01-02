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
  methods::show(utils::head(d_sub$object_label))
  message("subject_label examples:")
  methods::show(utils::head(d_sub$subject_label))
  if(as_tidygraph){
    d_sub <- dt_to_graph(d_sub,
                         add_hover=add_hover)
  }
  d_sub
} 