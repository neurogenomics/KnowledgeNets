#' @describeIn get_ get_
get_mondo_maps_files <- function(map_types,
                                 map_to,
                                 save_dir){
  requireNamespace("echogithub")
  path <- map_type <- to <- NULL;
  
  files_path <- file.path(save_dir,"mondo_maps.csv.gz")
  if(!file.exists(files_path)){
    files <- echogithub::github_files(owner = "monarch-initiative",
                                      repo = "mondo", 
                                      query = "src/ontology/mappings/") 
    data.table::fwrite(files,files_path)
  } else {
    files <- data.table::fread(files_path)
  }
  #### Filter map types ####
  files[,map_type:=data.table::tstrsplit(basename(path),"_",keep=2)]
  files[is.na(map_type),map_type:="default"]
  if(!is.null(map_types)){
    files <- files[map_type %in% map_types]  
  }
  #### Filter mapping to ####
  files[,to:=data.table::tstrsplit(basename(path),"_|[.]",keep=3)]
  if(!is.null(map_to)) {
    if("hpo" %in% map_to) {
      map_to <- c(map_to[map_to!="hpo"],
                  "omim","decipher","orphanet","hp") |> unique()
    }
    files <- files[to %in% map_to]
  }
  return(files)
}