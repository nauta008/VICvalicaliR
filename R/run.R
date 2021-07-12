

#' Run Tool
#'
#' @param config
#'
#' @return
#' @export
#'
#' @examples
run_tool <- function(config){
  read_settings(config)
  logger.init()
  if(.VICtools$settings$mode == "validation"){
    validation <- new("Validation")
    set(validation)
    run(validation)
  }
}
