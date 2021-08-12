
as.verification <- function(settings){
  # set properties of verification class from settings list
  verification <- new("Verification")
  verification@method <- toupper(settings$method)
  verification@start <- lubridate::parse_date_time(settings$start,"ymd")
  verification@end <- lubridate::parse_date_time(settings$end,"ymd")

  if(is.null(settings$data$start)){
    settings$data$start <- settings$start
  }
  if(is.null(settings$data$end)){
    settings$data$end <- settings$end
  }

  if(!is.null(settings$obs_file)){
    verification@obs_file <- settings$obs_file
  }
  if(!is.null(settings$sim_file)){
    verification@sim_file <- settings$sim_file
  }

  verification@dataset <- as.dataset(settings$data)

  return(verification)
}

