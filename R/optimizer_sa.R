
optimizer.sa <- function(param_list, calibration, reducer=0.95){
  max_iter <- calibration@max_iter
  n <- 0
  min_tau <- 0.01
  tau <- 100
  solution <- cal.params.default(param_list)

  # run eval model
  score <- eval_model(params)
  optimum <- score
  param_names <- names(param_list)

  while(tau > min_tau && n <  max_iter){
    # randomly select a param to modify
    param_name <- sample(param_names, 1)
    # copy solution
    proposed_solution <- solution
    # randomly modify a calibration param
    proposed_solution[param_name] <- runif(1, min=param_list[[param_name]]$lower, max=param_list[[param_name]]$upper)


    n <- n+1
  }

  return(list(n=n,solution=solution,optimum=optimum))

}
