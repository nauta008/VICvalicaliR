
#' @name set
setMethod("set", signature("Validation") , function(x){
  x@start <- lubridate::parse_date_time(.VICtools$settings$validation$start, "ymd")
  x@end <- lubridate::parse_date_time(.VICtools$settings$validation$start, "ymd")
})

#' @name run
setMethod("run", signature("Validation"), function(x){
  log_debug("Start validation run.")
})
