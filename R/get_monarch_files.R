#' @describeIn get_monarch_ get_
#' Monarch files
#' 
#' Find files 
#' \href{https://monarchinitiative.org/}{Monarch Initiative}
#' \href{https://data.monarchinitiative.org}{server}.
#' 
#' @export
#' @import rvest
#' @examples
#' files <- get_monarch_files() 
get_monarch_files <- function(maps=NULL,
                              queries=NULL,
                              domain="https://data.monarchinitiative.org",
                              subdir="latest/tsv/all_associations/",
                              omit=c("...","md5sums","index.html")){
  name <- from <- to <- NULL;

  html <- rvest::read_html(paste(domain,subdir,sep="/"))
  links <- rvest::html_elements(html,"a")
  files <- data.table::data.table(
    name=gsub("\\.tsv|\\.gz","",rvest::html_text(links)),
    file=rvest::html_text(links),
    url=rvest::html_attr(links,"href")
  )
  #### Remove extra rows ####
  files <- files[!name %in% omit,]
  #### Steps specific to associations files ####
  if(grepl("_associations",subdir)){
    #### Add from/to cols ####
    files[,c("from","to"):=data.table::tstrsplit(gsub("\\..*","",name),"_")]
    #### Subset using maps ####
    if(!is.null(maps)){
      files <- lapply(maps, function(m){
        if(!is.null(m)){
          files[(from==m[1] & to==m[2]) | 
                   (from==m[2] & to==m[1])]
        } 
      }) |> data.table::rbindlist() |> unique()
    } 
  }  
  #### Subset using queries ####
  if(!is.null(queries)){
    bool <- lapply(queries, function(q){
      if(!is.null(q)){
        regex <- paste(paste0("*.",q,".*"),collapse = "&") 
        files[, Reduce(`|`,lapply(.SD, function(x){grepl(regex,x)}))] 
      } 
    }) |> data.table::as.data.table()
    files <- files[Matrix::rowSums(bool)>0,]
  }
  #### Report ####
  messager("Files found:",nrow(files))
  #### Return ####
  return(files)
}
