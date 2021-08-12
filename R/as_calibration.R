
as.calibration <- function(settings){

  if(is.null(settings$start) || is.null(settings$end) || is.null(settings$objective)){
    log_error("Missing one of: start, end, objective in calibration settings")
    stop()
  }

  sdt <- lubridate::parse_date_time(settings$start, "ymd")
  edt <- lubridate::parse_date_time(settings$end, "ymd")
  # settings$verification$start <- settings$start
  # settings$verification$end <- settings$end
  # veri <- as.verification(settings$verification)
  settings$objective$data$start <- settings$start
  settings$objective$date$end <- settings$end
  objective <- as.objective(settings$objective)

  cal <- new("Calibration", start=sdt, end=edt, objective=objective)

  return(cal)

}
