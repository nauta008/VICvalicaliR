

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
  start_dt <- lubridate::now()
  settings.read(config)
  # create working directory
  dir.create(.VICvalicaliR$settings$output, recursive = T ,showWarnings = F)
  # add log messages to console and file
  logger.init()
  log_info(sprintf("Start VICvalicaliR at %s", as.character(lubridate::now())))
  # init domain
  domain.init(.VICvalicaliR$settings$domain$file)
  if(.VICvalicaliR$settings$mode == "validation"){
    validation <- as.validation(.VICvalicaliR$settings$validation)
    run(validation)
  }
  end_dt <- lubridate::now()
  timing <- round(lubridate::as.period(lubridate::as.interval(start_dt,end_dt) ), digits = 2)
  log_success(sprintf("VICvalicalR finished in %s.", as.character(timing)))
}
