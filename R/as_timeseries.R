
as.timeseries <- function(settings){
  timeseries <- new("Timeseries")
  # REQUIRED slots
  timeseries@dataset <- as.dataset(settings$data)
  return(timeseries)
}
