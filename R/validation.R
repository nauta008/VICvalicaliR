
as.validation <- function(settings){
  validation <- new("Validation")
  validation@start <- lubridate::parse_date_time(settings$start, "ymd")
  validation@end <- lubridate::parse_date_time(settings$end, "ymd")
  for(veri_settings in settings$verification){
    veri_settings$start <- validation@start
    veri_settings$end <- validation@end
    new_verification <- as.verification(veri_settings)
    validation@verifications <- append(validation@verifications, new_verification)
  }
  validation@output <- settings$output
  return(validation)
}

#' @name run
setMethod("run", signature("Validation"), function(x){
  log_debug("Start validation run.")
  # create output file
  ncdf.create(x@output)
  start(x)
  log_info("Validation run finished.")
})

setMethod("start", signature("Validation"), function(x){
  log_debug("Start validation")
  for(verification in x@verifications){
    result_st <- start(verification)
    ncdf.write.data(x@output,result_st)
  }
})
