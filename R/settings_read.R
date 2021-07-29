
#' Read Settings
#'
#' @param file
#' @export
#'
#' @return
settings.read <- function(con){
  settings <- yaml::read_yaml(con)
  .VICvalicaliR$settings <- append(settings,.VICvalicaliR$settings)
}

