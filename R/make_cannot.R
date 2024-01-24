make_cannot <- function(annot,
                        col_side_vars){
  annot <- data.table::copy(annot)
  col <- map_colors(annot,
                    columns = col_side_vars,
                    as = "function")
  ha_list <- lapply(stats::setNames(col_side_vars,
                                    col_side_vars),
                    function(x){
    ComplexHeatmap::anno_barplot(which = "column",
                                 x = annot[[x]] ,
                                 gp = grid::gpar(fill = col[[x]][[1]](1)),
                                 add_numbers = FALSE)
  } )
  ha <- do.call(ComplexHeatmap::columnAnnotation, ha_list )
  # ComplexHeatmap::draw(ha)
  return(ha)
}
