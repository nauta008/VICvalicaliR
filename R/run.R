

#' Run Tool
#'
#' @param config
#'
#' @return
#' @export
#'
#' @examples
run_tool <- function(config){
  settings.read(config)
  logger.init()
  if(.VICvalicaliR$settings$mode == "validation"){
    validation <- as.validation(.VICvalicaliR$settings$validation)
    run(validation)
  }
}
