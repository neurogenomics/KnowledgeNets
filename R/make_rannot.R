make_rannot <- function(annot,
                        row_side_vars){
  annot_i <- data.table::copy(annot)[,row_side_vars,with=FALSE]
  col <- map_colors(annot_i,
                    as = "vector") 
  ra <- ComplexHeatmap::HeatmapAnnotation(
    df = annot_i,
    which = "row",
    col =  col)
  return(ra)
}
