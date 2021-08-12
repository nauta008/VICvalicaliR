

setMethod("eval.model", signature("Calibration"), function(x,params){

  gamma_params <- params
  beta_params <- mpr.get.beta(gamma_params)
  # TODO:
  # 1: vic.write.params(file, beta_params)
  # 2: vic.run()
  # 3 veri_results <- run(x@verification) Note, simulation file differs compared to validation mode
  # 4 aggregate results in space and time to get one value.

})
