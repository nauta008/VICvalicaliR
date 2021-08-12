

as.objective <- function(settings){

  if(is.null(settings$data)){
    log_error("Missing one of 'data' in objective settings")
    stop()
  }
  obj <- new("Objective")
  if(!is.null(settings$method)){
    obj$method <- settings$method
  }
  obj$data <- as.dataset(settings$data)
  return(obj)

}
