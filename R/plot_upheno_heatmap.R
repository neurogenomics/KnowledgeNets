#' @describeIn plot_ plot_
plot_upheno_heatmap <- function(plot_dat,
                               ont,
                               hpo_ids=NULL,
                               value.var=c("phenotype_genotype_score",
                                           "prop_intersect",
                                           "equivalence_score",
                                           "subclass_score"),
                               min_rowsums=NULL,
                               cluster_from_ontology=FALSE,
                               save_dir=tempdir(),
                               height = 15,
                               width = 10){
  requireNamespace("ComplexHeatmap")
  subject <- hpo_id <- label1 <- NULL;
  set.seed(2023)
  value.var <- match.arg(value.var)
  # hpo_ids <- MultiEWCE::example_targets$top_targets$hpo_id

  ### Subset phenotypes
  if(!is.null(hpo_ids)) plot_dat <- plot_dat[subject %in% unique(hpo_ids)]
  plot_dat[,hpo_id:=subject][,label1:=gsub(" (HPO)","",label1,fixed = TRUE)]
  plot_dat <- add_ancestors(plot_dat)
  data.table::setkeyv(plot_dat,"label1")

  ### Plot
  X <- data.table::dcast.data.table(plot_dat,
                                    formula = label1 ~ subject_taxon_label2,
                                    fill = 0,
                                    value.var = value.var,
                                    fun.aggregate = mean,
                                    na.rm=TRUE)|>
    dt_to_matrix()
  #### Filter by min_rowsums ####
  if(!is.null(min_rowsums)){
    X <- X[Matrix::rowSums(X, na.rm = TRUE)>=min_rowsums,]
  }
  if(nrow(X)==0) stop("No rows remain after filtering by `min_rowsums`.")

  #### Get clusters from ontology ####
  if(isTRUE(cluster_from_ontology)){
    ids <- map_ontology_terms(ont = ont,
                              terms = plot_dat$subject,
                              keep_order = FALSE)
    ids <- ids[ids %in% rownames(X)]
    ## best to do this on the entire HPO, then subset
    cluster_rows <- ontology_to(to="igraph_dist_hclust",
                                terms = names(ids))
    # leaves <- dendextend::get_leaves_attr(cluster_rows,"label")
    # c2 <- dendextend::prune(cluster_rows,
    #                         leaves[!leaves %in% names(ids)],
    #                         keep_branches = FALSE)
    ## subset to only those in the heatmap
    X <- X[ids,]
  } else {
    cluster_rows <- TRUE
  }
  #### Add annotation ####
  annot_dat <- plot_dat[rownames(X),
                        c("label1","n_genes_db1",
                          "ancestor_name")] |> unique()
  # col_fun <- colorRamp2::colorRamp2(
  #   seq(min(annot_dat$n_genes_db1),
  #       max(annot_dat$n_genes_db1),
  #       length.out=4),
  #   pals::gnuplot(4))
  la <- ComplexHeatmap::rowAnnotation(
    df=data.frame(annot_dat[,-c("label1")],
                  row.names = annot_dat$label1),
    # ?ComplexHeatmap::Legend
    show_legend = c(TRUE, FALSE),
    annotation_legend_param = list(
      n_genes_db1= list(title = "HPO genes"))
  )
  row_split <- if(isFALSE(cluster_from_ontology)) annot_dat$ancestor_name
  #### make heatmap ####
  ch <- ComplexHeatmap::Heatmap(matrix = as.matrix(X),
                                name = value.var,
                                cluster_rows = cluster_rows,
                                row_title_rot = 0,
                                row_names_gp = grid::gpar(fontsize = 7),
                                row_title_gp = grid::gpar(fontsize = 9),
                                row_split = row_split,
                                col = pals::viridis(n = 20),
                                cluster_columns = FALSE,
                                left_annotation = la)
  # methods::show(ch)
  #### Save ####
  if(!is.null(save_dir)){
    grDevices::pdf(file = file.path(save_dir,
                                    paste0("plot_upheno_heatmap-",
                                           value.var,".pdf")),
                   height = height,
                   width = width)
    methods::show(ch)
    grDevices::dev.off()
  }
  return(ch)
}
