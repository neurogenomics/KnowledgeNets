get_prevalence_orphanet <- function(agg_by=c("mondo_id","id","Name"),
                                    include_mondo=TRUE){
  prevalence_numerator <- prevalence <- prevalence_mean <- Prevalence.ValMoy <-
    prevalence_denominator <- OrphaCode <- disease_id <- NULL;
  ## Open in Excel and convert to CSV first ##
  path <- system.file("extdata", "orphanet_epidemiology.csv.gz",
                      package = "KGExplorer")
  d <- data.table::fread(path)
  #### Fix column names ####
  names(d) <- trimws(whitespace = "[.]",
                     gsub("[.]+",".",
                          gsub("/|#",".",
                               gsub(paste("/DisorderList",
                                          "/Disorder",
                                          "/PrevalenceList",
                                          sep = "|"),
                                    "",
                                    names(d)
                               )
                          )
                     )
  ) |> `names<-`(names(d))
  #### Prepare numeric prevalence data ####
  d[,prevalence_numerator:=`Prevalence.ValMoy`]
  d$prevalence_denominator <- gsub(
    " +","",
    stringr::str_split(d$Prevalence.PrevalenceClass.Name," / ",
                       simplify = TRUE)[,2]
  ) |> as.numeric()
  d[,prevalence:=prevalence_numerator/prevalence_denominator*100]
  names(d) <- make.unique(names(d))
  d[,disease_id:=paste0("Orphanet:",OrphaCode)]
  #### Add mondo ID ####
  if(isTRUE(include_mondo)){
    d <- map_mondo(dat = d, 
                   input_col = "disease_id", 
                   map_to = "orphanet") 
   } 
  if(!is.null(agg_by)){
    ## Compute mean prevalence
    dprev <- d[,list(n=.N, 
                     prevalence_numerator_mean=mean(prevalence_numerator, na.rm=TRUE),
                     prevalence_denominator_mean=mean(prevalence_denominator, na.rm=TRUE),
                     prevalence_mean=mean(prevalence, na.rm=TRUE)),
               by=agg_by #"Prevalence.PrevalenceType.Name"
    ][order(-prevalence_mean)]
    return(dprev)
  } else {
    return(d)
  }
}

#### Alternative approach: parse XML files ####
# https://www.orphadata.com/epidemiology/
# tmp <- tempfile(fileext = ".xml")
# download.file("https://www.orphadata.com/data/xml/en_product9_prev.xml",
#               destfile = tmp)
# xml <- rvest::read_html(tmp)

# xml <- xml2::read_xml(tmp)
# xmlconvert::xml_to_df(xml, records.xpath = "/html")
#   xmls <- xml |>
#     rvest::html_nodes("Disorder") |>
#     # xml2::xml_find_all(".//Disorder") |>
#     xml2::as_list()# |>
#     tibble::as_tibble(xmls[[1]]) |>
#     tidyr::unnest_longer(col=c(OrphaCode,ExpertLink,
#                                Name,DisorderType,DisorderGroup), simplify = TRUE) |>
#     tidyr::unnest_wider(col=PrevalenceList) |>
#     tidyr::unnest_longer(col = c(Source,PrevalenceType,PrevalenceQualification,ValMoy))
#
#
#   nodes <- xml2::xml_find_all(xml, ".//Disorder")
#   ## Preview structure
#   nodes[1]|> xml2::html_structure()
#     # xml2::xml_path()|>
#     # xml2::xml_text()
#
#   ### Convert each node to a flattened data.table
#   xml2::xml_find_all(nodes[1:10], ".//Prevalence") |>
#     xml2::as_list() |>
#     purrr::flatten() |>
#     data.table::as.data.table()# |>
# data.table::transpose()
