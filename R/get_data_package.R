#' @describeIn get_ get_
#' @inheritParams utils::data
#' @export
#' @importFrom utils data
get_data_package <- function(name,
                             package = "KGExplorer"){
  utils::data(list=name, package = package)
  get(eval(name))
}
