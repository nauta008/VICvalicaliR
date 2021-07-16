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
  dir.create(x@ts_plots, showWarnings = F)
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
    browser()
    series_sf <- data.get(ts, sdt=x@start, edt=x@end)
    out_file <- file.path(x@ts_plots,sprintf("timeseries_%s.pdf",ts@dataset@var))
    timeseries.plot(series_sf,out_file)
  }
})
