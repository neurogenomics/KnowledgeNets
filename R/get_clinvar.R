#' @describeIn get_ get_
#' Get ClinVar variant data
#'
#' ClinSigSimple          integer, 0 = no current value of Likely pathogenic; Pathogenic; Likely pathogenic, low penetrance;
#' Pathogenic, low penetrance; Likely risk allele; or Risk allele
#' 1 = at least one current record submitted with an interpretation of Likely pathogenic; Pathogenic;
#' Likely pathogenic, low penetrance; Pathogenic, low penetrance; Likely risk allele; 
#' or Risk allele (independent of whether that record includes assertion criteria and evidence).
#' -1 = no values for clinical significance at all for this variant or set of variants; used for
#' the "included" variants that are only in ClinVar because they are included in a
#' haplotype or genotype with an interpretation
#' NOTE: Now that the aggregate values of clinical significance give precedence to records with
#' assertion criteria and evidence, the values in this column may appear to be in conflict with the
#' value reported in ClinicalSignificance.  In other words, if a submission without assertion criteria and
#' evidence interpreted an allele as pathogenic, and those with assertion criteria and evidence interpreted
#' as benign, then ClinicalSignificance would be reported as Benign and ClinSigSimple as 1.
#' @source \href{https://ftp.ncbi.nlm.nih.gov/pub/clinvar/}{ClinVar server}
#' @source \href{https://ftp.ncbi.nlm.nih.gov/pub/clinvar/README.txt}{
#' ClinVar server README}
#' 
#' @export
get_clinvar <- function(as_granges=FALSE,
                        annotate=FALSE){
  cv <- data.table::fread("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz")
  if(as_granges) return(cv)           
  grl <- list(
    "GRCh37"=GenomicRanges::makeGRangesFromDataFrame(
      cv[Start<Stop & Assembly=="GRCh37"],
      start.field = "Start",
      end.field = "Stop",
      seqnames.field = "Chromosome",
      keep.extra.columns = TRUE
    ),
    "GRCh38"=GenomicRanges::makeGRangesFromDataFrame(
      cv[Start<Stop & Assembly=="GRCh38"],
      start.field = "Start",
      end.field = "Stop",
      seqnames.field = "Chromosome",
      keep.extra.columns = TRUE
    )
  )
  
  if(annotate){
    hits <- lapply(stats::setNames(names(grl),names(grl)),function(x){
      map_variants(grl[[x]], build = x)
    })
    # hits_dt <- lapply(hits, function(h){
    #   data.table::data.table(
    #     region=sort(table(h$region))/ length(h)*100
    #   )
    # }) |> data.table::rbindlist(idcol = "build") |>
    #   data.table::setnames(c("build","region","percent"))
    # hits_dt[,list(percent=mean(percent)),by="region"]
    hits_merged <- lapply(hits, function(h){
      hdt <- data.table::as.data.table(h)
      hdt[,c("PRECEDEID","FOLLOWID","CDSID"):=NULL]
      merge(
        x = cv[,id:=.I],
        y = unique(hdt),
        by.x = "id",
        by.y = "QUERYID",
        sort = FALSE,
        all = TRUE
      )
    }) |> data.table::rbindlist(idcol = "build")
    remove(hits)
    plot_dat <- hits_merged[,.N, by=c("build","region","Type","ClinSigSimple")]
    plot_dat[,ClinSigSimple:=factor(ClinSigSimple, levels=c(-1,0,1), labels=c("unknown","benign","pathogenic"))]
    ggplot(plot_dat, aes(x=forcats::fct_reorder(region, N), 
                         y=N,
                         fill=Type)) +
      geom_bar(stat="identity",position="stack") +
      facet_grid(ClinSigSimple~.,  scales = "free_y") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x="Region", y="Count", fill="Type") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) 
    
    # sort(table(cv$Type))
    return(hits_merged)
  } else {
    return(grl)
  }
  
}
