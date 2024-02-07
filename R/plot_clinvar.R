#' @describeIn plot_ plot_
#' Plot mapped variant annotations.
#' @export
plot_clinvar <- function(hits,
                         x="region",
                         y="N",
                         fill="Type",
                         rows="ClinSigSimple",
                         by=c("build",x,fill,rows)
                         ){
  
  requireNamespace("ggplot2")
  requireNamespace("forcats")
  
  plot_dat <- hits[,.N, by=by]
  gg <- ggplot2::ggplot(plot_dat, 
                        ggplot2::aes(
                          x=forcats::fct_reorder(!!ggplot2::sym(x),
                                                 !!ggplot2::sym(y)), 
                          y=!!ggplot2::sym(y),
                          fill=!!ggplot2::sym(fill))) +
    ggplot2::geom_bar(stat="identity",position="stack") +
    ggplot2::facet_grid(rows = rows,
               scales = "free_y") +
    ggplot2::labs(x=x)+
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1)) 
  return(list(
    plot=gg,
    data=plot_dat
  ))
}
