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
#' @param annotate Add variant annotations with \link{map_variants}.
#' @source \href{https://ftp.ncbi.nlm.nih.gov/pub/clinvar/}{ClinVar server}
#' @source \href{https://ftp.ncbi.nlm.nih.gov/pub/clinvar/README.txt}{
#' ClinVar server README}
#' 
#' @export
get_clinvar <- function(as_granges=FALSE,
                        annotate=FALSE){
  ClinSigSimple_label <- ClinSigSimple <- index <- Start <- Stop <- Assembly <- 
    id <- NULL;
  cv <- data.table::fread(
    "https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz"
    )
  cv[,index:=.I]
  ### Count unique diseases ####
  PhenotypeList <- unique(cv$PhenotypeList)
  messager(formatC(length(PhenotypeList),big.mark = ","),
           "diseases/phenotypes extracted from ClinVar.")
  #### Get disease metadata ####
  # diseases <- data.table::fread("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/disease_names") |>
  #   data.table::setnames(c("#DiseaseName"),c("DiseaseName"))
  # diseases <- diseases[DiseaseName %in% PhenotypeList]
  # #### Filter IDs #####
  # PhenotypeIDS <- unlist(stringr::str_split(cv$PhenotypeIDS,",|;|[|]"))|>
  #   unique()|>
  #   gsub(pattern="mondo:mondo:",replacement="mondo:")|>
  #   gsub(pattern="Human Phenotype Ontology:",replacement="") |> unique()
  # annot <- HPOExplorer::load_phenotype_to_genes(3)[,disease_id:=gsub(
  #   "ORPHA:","Orphanet:",disease_id)]
  # #### Search for disease IDs in cv #### 
  # cv <- cv[PhenotypeList %in% diseases[SourceID!="" & is.na(DiseaseMIM)]$DiseaseName]
  # cv <- cv_og[grepl("OMIM:|Orphanet:",PhenotypeIDS),]
  
  
  if(isFALSE(as_granges) && isFALSE(annotate)) return(cv)      
  #### Convert to GRanges ####
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
  if(isTRUE(annotate)){
    hits <- lapply(stats::setNames(names(grl),names(grl)),function(x){
      map_variants(grl[[x]], build = x)
    })
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
    remove(cv)
    remove(hits)
    hits_merged[,ClinSigSimple_label:=factor(
      ClinSigSimple,
      levels=c(-1,0,1),
      labels=c("unknown","benign","pathogenic"))] 
    #### Plot ####
    plt <- plot_clinvar(
      hits=hits_merged, 
      x="region",
      y="N",
      fill="Type",
      by=c("build","region","Type","ClinSigSimple_label"),
      rows="ClinSigSimple_label")
    return(list(
      data=hits_merged,
      plot=plt
    ))
  } else {
    return(grl)
  }
  
}
