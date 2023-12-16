get_monarch_colkey <- function(){
  txt <- readLines("https://data.monarchinitiative.org/README.txt")
  
  txt2 <- txt[seq(grep("TSV Glossary of Terms",txt)+1,length(txt))]
  txt2 <- txt2[txt2!=""]
  cat(txt2, sep = "\n")
}