
#' Title
#'
#' @param file
#'
#' @return
#'
#' @examples
read_settings <- function(con){
  settings <- yaml::read_yaml(con)
  .VICtools$settings <- append(settings,.VICtools$settings)
}

