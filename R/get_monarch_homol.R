get_monarch_homol <- function(filters=list(subject_db="HGNC"),
                              as_graph=FALSE){
  gene_label <- hgnc_label <- gene <- hgnc <- 
    subject_category <- object_categpry <- 
    object_category <- subject_taxon_label <- NULL;
  
  homol <- get_monarch("gene_homolog", 
                       rbind=TRUE)
  messager("Unique species with orthologs:",
           data.table::uniqueN(homol$subject_taxon_label))
  homol[,subject_category:="gene"][,object_category:="gene"]
  add_db(homol,
         input_col = "subject")
  add_db(homol,
         input_col = "object")
  ### Filter
  homol <- filter_dt(homol,
                     filters = filters)
  if(as_graph){
    homol <- dt_to_graph(dat = homol)
    return(homol)
  } else {
    ## Subset to only convert human --> non-human
    data.table::setnames(homol,
                         c("subject","subject_label","object","object_label"),
                         c("hgnc","hgnc_label","gene","gene_label"))
    ## Add human-to-human back into map
    hhomol <- (homol[subject_taxon_label=="Homo sapiens",
                     c("hgnc","hgnc_label",
                       "subject_taxon","subject_taxon_label")] |> unique()
    )[,gene:=hgnc][,gene_label:=hgnc_label]
    homol <- data.table::rbindlist(list(homol,hhomol),fill=TRUE)
    ## Add HGNC IDs
    hhomol[,gene:=hgnc_label]
    homol <- data.table::rbindlist(list(homol,hhomol),fill=TRUE)
    ## Make unique
    homol <- unique(homol)
    messager("Unique orthologs:",
             formatC(data.table::uniqueN(homol$gene),big.mark = ","))
    return(homol)
  }
}