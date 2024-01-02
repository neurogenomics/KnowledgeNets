#' @describeIn utils_ utils_
get_data_package <- function(name,
                             package = "KGExplorer"){
  utils::data(list=name, package = package)
  get(eval(name))
}
