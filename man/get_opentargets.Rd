% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_opentargets.R
\name{get_opentargets}
\alias{get_opentargets}
\title{Get Open Targets}
\source{
\href{https://community.opentargets.org/t/r-script-for-graphql-query-query-targetdiseaseevidence/662/5}{OpenTargets GraphQL queries in R}
}
\usage{
get_opentargets(
  release = "latest",
  data_type = c("associationByDatasourceDirect", "associationByDatasourceIndirect",
    "associationByDatatypeDirect", "associationByDatatypeIndirect",
    "associationByOverallDirect", "associationByOverallIndirect", "l2g")[1],
  server = "https://ftp.ebi.ac.uk/pub/databases/opentargets/",
  subdir = c("platform/", "genetics/")[1],
  subdir2 = c("/output/etl/parquet/", "/")[1],
  ftp = paste0(server, subdir, release, subdir2, data_type[1], "/"),
  save_dir = cache_dir(),
  max_files = NULL,
  workers = 1,
  force_new = FALSE
)
}
\arguments{
\item{release}{Open Targets release version.}

\item{data_type}{Type of data to download.}

\item{server}{Open Targets server.}

\item{subdir}{Open Targets subdirectory.}

\item{subdir2}{Open Targets sub-subdirectory.}

\item{ftp}{The final Open Targets FTP URL.}

\item{save_dir}{Directory to save a file to.}

\item{max_files}{Limit the number of files to import}

\item{workers}{Number (>1) or proportion (<1) of worker cores to use.}

\item{force_new}{If TRUE, force a new download.}
}
\description{
Get Open Targets disease-gene associations data
NOTE: If you get an error like " Cannot call parquet...", 
try running \code{arrow::install_arrow},
 restarting the R session, and trying again.
}
\examples{
\dontrun{
d <- get_opentargets(max_files=2, save_dir=tempdir())
}
}
