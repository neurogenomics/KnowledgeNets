#' Get Open Targets
#' 
#' Get Open Targets disease-gene associations data
#' NOTE: If you get an error like " Cannot call parquet...", 
#' try running \code{arrow::install_arrow},
#'  restarting the R session, and trying again.
#' @source \href{https://community.opentargets.org/t/r-script-for-graphql-query-query-targetdiseaseevidence/662/5}{OpenTargets GraphQL queries in R}
#' @param release Open Targets release version.
#' @param data_type Type of data to download.
#' @param server Open Targets server.
#' @param subdir Open Targets subdirectory.
#' @param subdir2 Open Targets sub-subdirectory.
#' @param ftp The final Open Targets FTP URL.
#' @inheritParams get_
#' @import rvest
#' @export
#' @examples
#' d <- get_opentargets()
get_opentargets <- function(release="latest",
                            data_type=c("associationByDatasourceDirect",
                                        "associationByDatasourceIndirect",
                                        
                                        "associationByDatatypeDirect",
                                        "associationByDatatypeIndirect",
                                        
                                        "associationByOverallDirect",
                                        "associationByOverallIndirect"
                            )[1],
                            server="https://ftp.ebi.ac.uk/pub/databases/opentargets/",
                            subdir=c("platform/","genetics/")[1],
                            subdir2=c("/output/etl/parquet/","/")[1],
                            ftp = paste0(server,
                                         subdir,
                                         release,
                                         subdir2,
                                         data_type[1],"/"),
                            save_dir=cache_dir(),
                            force_new=FALSE){  
  
  ## Variant and gene level data merged for all genome-wide summary statistics:
  # ftp="https://ftp.ebi.ac.uk/pub/databases/opentargets/genetics/latest/d2v2g_scored/"
  Name <- NULL;
  save_path <- file.path(save_dir,
                         paste0("opentargets_",data_type,".rds"))
  #### Import cached data ####
  if(file.exists(save_path) && 
     isFALSE(force_new)){
    messager("Loading cached file -->",save_path)
    return(readRDS(save_path))
  }
  #### Get new data ####
  requireNamespace("arrow")
  #### Scrape FTP to get file names ####
  tbl <- (
    rvest::read_html(ftp)|>
      rvest::html_table()
  )[[1]]|>
    subset(endsWith(Name,".parquet")|endsWith(Name,".json")) 
  tbl$Date <- stringr::str_split(tbl$`Last modified`," ",simplify = TRUE)[,1]
  if(nrow(tbl)==0) stopper("No data files found at", ftp)
  BPPARAM <- set_cores()
  d <- BiocParallel::bplapply(stats::setNames(tbl$Name,
                                              tbl$Name),
                              BPPARAM = BPPARAM,
                              function(f){ 
                                if(endsWith(f,".json")){
                                  j <- jsonlite::fromJSON(paste0(ftp,"/",f))
                                  jsonlite::fromJSON(j$serialisedSchema) 
                                } else if (endsWith(f,".parquet")){
                                  arrow::read_parquet(paste0(ftp,f)) |>
                                    data.table::data.table()
                                }
                              }) |> data.table::rbindlist(idcol = "file")
  attr(d,"version") <- tbl$Date[[1]] 
  #### Report ####
  messager("OpenTargets data loaded with:", 
           "\n-",formatC(nrow(d), big.mark = ","),"rows", 
           if("targetId" %in% names(d)){
             paste("\n-",formatC(length(unique(d$targetId)), big.mark = ","),
                   "unique targets across"     )
           },
           if("diseaseId" %in% names(d)){
             paste("\n-",formatC(length(unique(d$diseaseId)), big.mark = ","), 
                   "unique diseases across"     )
           },
           v = TRUE)
  cache_save(d, save_path)
  return(d)
}