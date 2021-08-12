
cal.params.sample <- function(params, n){

  samples <- array(NA, dim = c(n, length(params)))

  for(i in 1:length(params)){
    param <- params[[i]]
    samples[,i] <- runif(n, min=param$lower, param$upper)
  }

  samples <- as.data.frame(samples) %>% setNames(names(params))
  return(samples)
}


cal.params.default <- function(params){

  default <- array(NA, dim = c(1, length(params)))
  for(i in 1:length(params)){
    param <- params[[i]]
    default[1,i] <- param$default
  }
  default <- as.data.frame(default) %>% setNames(names(params))

  return(default)

}
