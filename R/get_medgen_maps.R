#' @describeIn get_ get_
#' Get MedGen maps.
get_medgen_maps <- function(){
  URL <- "https://ftp.ncbi.nlm.nih.gov/pub/medgen/"
  def <- data.table::fread(paste0(URL,"csv/MGDEF.csv.gz"), fill = TRUE)
  map <- data.table::fread(paste0(URL,"MedGenIDMappings.txt.gz"),
                           header = TRUE,
                           key = "source_id")
  data.table::setnames(map,"#CUI","CUI")
  return(list(map=map,
              def=def))
}