
#' init logger
#'
#' @return
#'
#' @examples
logger.init <- function(){
  # log to console
  log_format <- layout_glue_generator(format = '{level} {time}: {msg}')
  log_layout(log_format, index = 1)
  log_appender(appender_console)
  .VICvalicaliR$settings$log_level <- toupper(.VICvalicaliR$settings$log_level)
  # set log threshold
  if(.VICvalicaliR$settings$log_level=="INFO"){
    log_threshold(INFO)
  }
  else if(.VICvalicaliR$settings$log_level=="DEBUG"){
    log_threshold(DEBUG)
  }
  else if(.VICvalicaliR$settings$log_level=="WARNING"){
    log_threshold(WARN)
  }
  else if(.VICvalicaliR$settings$log_level=="ERROR"){
    log_threshold(ERROR)
  }
  # create log file
  if(!is.null(.VICvalicaliR$settings$log)){
    file.create(.VICvalicaliR$settings$log)
    log_appender(appender_file(.VICvalicaliR$settings$log), index = 2)
    log_info(sprintf("Initialized log file %s",.VICvalicaliR$settings$log))
  }
}

