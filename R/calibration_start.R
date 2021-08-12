
setMethod("start", signature("Calibration"), function(x){

  log_info("Start Calibration")

  # currently only implementation for MPR
  optimizer.sa(gamma_params)


})


