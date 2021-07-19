

#' Run Tool
#'
#' @param config
#'
#' @return
#' @export
#' @name run_tool
#' @examples
#' # example of a validation
#' con <- system.file("extdata","validation_config.yml", package="VICvalicaliR")
#' run_tool(con)
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
