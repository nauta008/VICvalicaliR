#' @name run
setMethod("run", signature("Validation"), function(x){
  log_debug("Start validation run.")
  # create output directory
  output <- file.path(.VICvalicaliR$settings$output,"validation")
  dir.create(output, showWarnings = F, recursive = T)
  x@netcdf <- file.path(output,"netcdf")
  x@ts_plots <- file.path(output,"plots")
  # create netcdf output (directory)
  dir.create(x@netcdf,showWarnings = F)
  dir.create(x@ts_plots, showWarnings = F)
  # TODO: validate x before running?
  start(x)
  log_info("Validation run finished.")
})

setMethod("start", signature("Validation"), function(x){
  log_debug("Start validation")
  # evaluate model performance
  for(verification in x@verifications){
    result_st <- start(verification)
    if(!is.null(result_st)){
      # create output file
      prefix <- sprintf("verification_%s", verification@method)
      ncdf_file <- file.path(x@netcdf,get.file(verification@dataset, file_format="ncdf", prefix=prefix))
      ncdf.create(ncdf_file)
      ncdf.write.data(ncdf_file,result_st)
    }
  }
  # generate timeseries
  for(ts in x@timeseries){
    series_sf <- data.get(ts, sdt=x@start, edt=x@end)
    out_file <- file.path(x@ts_plots,get.file(ts@dataset, file_format="pdf", prefix="timeseries"))
    timeseries.plot(series_sf,out_file)
  }
})
