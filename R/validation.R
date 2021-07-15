
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
  for(timeseries_settings in settings$timeseries){
    ts <- as.timeseries(timeseries_settings)
    ts@start <- validation@start
    ts@end <- validation@end
    validation@timeseries <- append(validation@timeseries,ts)
  }
  return(validation)
}

#' @name run
setMethod("run", signature("Validation"), function(x){
  log_debug("Start validation run.")
  # create output directory
  output <- file.path(.VICvalicaliR$settings$output,"validation")
  dir.create(output, showWarnings = F, recursive = T)
  x@netcdf <- file.path(output,"netcdf","verification_results.ncdf")
  x@ts_plots <- file.path(output,"plots")
  # create netcdf output (directory)
  dir.create(dirname(x@netcdf),showWarnings = F)
  dir.create(dirname(x@ts_plots), showWarnings = F)
  # create output file
  ncdf.create(x@netcdf)
  # TODO: validate x before running?
  start(x)
  log_info("Validation run finished.")
})

setMethod("start", signature("Validation"), function(x){
  log_debug("Start validation")
  # evaluate model performance
  for(verification in x@verifications){
    result_st <- start(verification)
    if(!is.null(result_st) && !is.null(x@netcdf)){
      ncdf.write.data(x@netcdf,result_st)
    }
  }
  # generate timeseries
  for(ts in x@timeseries){
    series_sf <- create(ts)
    out_file <- file.path(x@ts_plots,sprintf("timeseries_%s.pdf",ts@var))
    timeseries.plot(series_sf,out_file)
  }
})
