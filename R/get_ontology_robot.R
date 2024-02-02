#' Get ontology robot
#' 
#' Download the ontology \href{https://github.com/ontodev/robot}{robot} tool
#'  and set its path so that it's accessible by \pkg{simona}.
#' @inheritDotParams get_data
#' @keywords internal 
get_ontology_robot <- function(...){
  simona_opt <- utils::getFromNamespace(x = "simona_opt",
                                        ns = "simona")
  robot <- get_data(file = "robot.jar",
                    repo = "ontodev/robot",
                    ...)
  simona_opt$robot_jar <- robot
}