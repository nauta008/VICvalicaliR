
as.timeseries <- function(settings){
  timeseries <- new("Timeseries")
  # dataset start and end inheritance from time series start end
  if(is.null(settings$data$start)){
    settings$data$start <- settings$start
  }
  if(is.null(settings$data$end)){
    settings$data$end <- settings$end
  }
  # REQUIRED slots
  timeseries@dataset <- as.dataset(settings$data)
  # OPTIONAL slots
  return(timeseries)
}
