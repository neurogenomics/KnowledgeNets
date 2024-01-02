add_definitions_mondo <- function(dat,
                                  input_col="mondo_id",
                                  output_col="mondo_def"){
  if(!exists("mondo")) mondo <- get_ontology("mondo")
  dict <- get_definitions(mondo)
  dat[,(output_col):=dict[get(input_col)]]
}