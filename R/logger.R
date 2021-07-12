
#' init logger
#'
#' @return
#'
#' @examples
logger.init <- function(){
  # log to console
  log_format <- layout_glue_generator(format = '{level} {time} {fn}: {msg}')
  log_layout(log_format, index = 1)
  log_appender(appender_console)
  .VICtools$settings$log_level <- toupper(.VICtools$settings$log_level)
  # set log threshold
  if(.VICtools$settings$log_level=="INFO"){
    log_threshold(INFO)
  }
  else if(.VICtools$settings$log_level=="DEBUG"){
    log_threshold(DEBUG)
  }
  else if(.VICtools$settings$log_level=="WARNING"){
    log_threshold(WARN)
  }
  else if(.VICtools$settings$log_level=="ERROR"){
    log_threshold(ERROR)
  }
  # create log file
  if(!is.null(.VICtools$settings$log)){
    file.create(.VICtools$settings$log)
    log_appender(appender_file(.VICtools$settings$log), index = 2)
    log_info(sprintf("Initialized log file %s",.VICtools$settings$log))
  }
}

