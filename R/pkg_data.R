#' @describeIn utils_ utils_
pkg_data <- function(name,
                     package = "KGExplorer"){
  utils::data(list=name, package = package)
  get(eval(name))
}
