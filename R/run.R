

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
  # create working directory
  dir.create(.VICvalicaliR$settings$output, recursive = T ,showWarnings = F)
  logger.init()
  if(.VICvalicaliR$settings$mode == "validation"){
    validation <- as.validation(.VICvalicaliR$settings$validation)
    run(validation)
  }
  log_info("VICvaliCaliR finished")
}
