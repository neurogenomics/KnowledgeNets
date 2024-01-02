#' @describeIn get_ get_
#' Get AlphaMissense predictions
#'
#' Get gene-level
#' \href{https://console.cloud.google.com/storage/browser/dm_alphamissense}{
#' AlphaMissense} predictions for all canonical and non-canonical
#' protein-coding gene transcripts.
#' @param types A character vector of types to return.
#' @param agg_fun A function to aggregate multiple transcripts per gene.
#' @param force_new If TRUE, force a new download.
#' @return A named list of data.tables of AlphaMissense predictions.
#' 
#' @export
#' @importFrom orthogene map_genes
#' @examples
#' \dontrun{
#' am <- get_alphamissense()
#' }
get_alphamissense <- function(types=c("canonical",
                                      "non_canonical",
                                      "merged"),
                              agg_fun=mean,
                              save_dir=cache_dir(),
                              force_new=FALSE){
  name <- NULL;
  types <- match.arg(types)
  save_path <- file.path(
    save_dir,paste0("alphamissense_",
                    deparse(substitute(agg_fun)),"_",
                    paste0(types,collapse = "_"),".rds")
  )
  if(file.exists(save_path) &&
     isFALSE(force_new)){
    messager("Loading cached alphamissense data:",shQuote(save_path))
    return(readRDS(save_path))
  }
  get_alphamissense_i <- function(URL,
                                  agg_fun=mean,
                                  agg_col=c("am_pathogenicity",
                                            "mean_am_pathogenicity")){
    am_pathogenicity <- input <- transcript_id <- NULL;
    am <- data.table::fread(URL)
    am$enst_id <- stringr::str_split(am$transcript_id,"\\.", simplify = TRUE)[,1]
    map <- orthogene::map_genes(genes = unique(am$enst_id),
                                target = "ENST",
                                species="human",
                                drop_na = FALSE,
                                mthreshold = Inf)
    am_mapped <- unique(map[,c("input","name")]) |>
      data.table::data.table(key = "input") |>
      data.table::merge.data.table(am, by.x = "input", by.y = "enst_id")
    if(!is.null(agg_fun)){
      agg_col <- agg_col[agg_col %in% names(am_mapped)]
      agg <- am_mapped[,list(n_transcripts=data.table::uniqueN(transcript_id),
                             input=input[1],
                             transcript_id=transcript_id[1],
                             mean_am_pathogenicity=agg_fun(get(agg_col))),
                       by="name"]
      return(agg)
    } else {
      return(am_mapped)
    }
  }

  res <- list()
  base_url <- "https://storage.googleapis.com/dm_alphamissense/"
  if("canonical" %in% types || "merged" %in% types){
    messager("Getting AlphaMissense data for canonical transcripts.")
    res[["canonical"]] <- get_alphamissense_i(
      URL=paste0(base_url,"AlphaMissense_gene_hg38.tsv.gz"),
      agg_fun=agg_fun
    )
  }
  if("non_canonical" %in% types || "merged" %in% types){
    messager("Getting AlphaMissense data for non-canonical transcripts.")
    res[["non_canonical"]] <- get_alphamissense_i(
      URL=paste0(base_url,"AlphaMissense_isoforms_hg38.tsv.gz"),
      agg_fun=agg_fun
    )
  }
  if("merged" %in% types){
    messager("Merging data.")
    res[["merged"]] <- data.table::rbindlist(
      list(
        canonical=res[["canonical"]][!is.na(name)],
        non_canonical=res[["non_canonical"]][!is.na(name)][
          !name %in% res[["canonical"]]$name
          ]
      ),idcol = "type"
    )
  }
  #### Save ####
  if(!is.null(save_dir)){
    messager("Saving alphamissense data ==>",shQuote(save_path))
    dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(res,save_path)
  }
  #### Return ####
  return(res)
}
