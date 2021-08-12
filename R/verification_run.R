#' @importFrom dplyr slice pull %>%
setMethod("start", signature("Verification") ,function(x){
  x@state <- "RUNNING"
  log_info(sprintf("Start model verification on %s and method %s", x@dataset@var, x@method))
  # get variable name in ncdf file
  data <- data.get(x)
  sim <- data$sim
  obs <- data$obs
  res_st <- NULL
  if(x@method=="NSE"){
    res_st <- nse(sim,obs)
  }
  else if(x@method=="KGE"){
    res_st <- kge(sim,obs)
  }
  else{
    log_warn(sprintf("Skipping method %s. Method %s not implemented. Choose NSE or KGE."))
    x@state <- "ERROR"
    return(res_st)
  }

  x@state <- "SUCCESS"
  return(res_st)
})
