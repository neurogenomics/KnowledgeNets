get_prevalence_oard <- function(ids){

  # ids <- unique(phenos$hpo_id)[1:100]
  res <- query_oard(ids=ids)
  res2 <- query_oard(ids=res$results$concept_id,
                         dataset_id=1,
                         endpoint="frequencies/singleConceptFreq",
                         concept_prefix="concept_id=")
  return(res2)
}
