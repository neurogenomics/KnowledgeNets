plot_ttd <- function(dat_sub){
  requireNamespace("ggplot2")
  requireNamespace("scales")
  HIGHEST_STATUS<- prioritised <- n_drugs <- NULL;

  dat_sub[,n_drugs:=.N, by=c("HIGHEST_STATUS")]
  ggplot2::ggplot(dat_sub, 
                  ggplot2::aes(x=HIGHEST_STATUS,
                               fill=prioritised,
                               label=n_drugs)) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::geom_label(
      data = unique(dat_sub[,c("HIGHEST_STATUS","prioritised","n_drugs")]),
      ggplot2::aes(x=HIGHEST_STATUS, fill=prioritised, label=n_drugs, y=1),
      fill="white", alpha=.75, inherit.aes = FALSE) +
    ggplot2::labs(x="Approval stage") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
