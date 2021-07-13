
nse <- function(sim,obs){
  log_debug("Calculate Nash–Sutcliffe efficiency")
  data <- c(sim=sim, obs=obs)
  names(data) <- c("sim","obs")
  data <- merge(data, name="attributes")
  res <- stars::st_apply(data, MARGIN=c("x","y","stations"), FUN=function(data_series){
    bias <- data_series[,1] - data_series[,2]
    a <- sum(bias^2, na.rm=T)
    c <- sum((data_series[,2] - mean(data_series[,2], na.rm = T))^2, na.rm = T)
    #b <- apply(data_series[,2], FUN=function(obs_series){ (obs_series - mean(obs_series, na.rm=T))^2})
    #c <- sum(b, na.rm=T)
    val <- 1 - a/c
    return(val)
  })
  browser()
  return(res)
}



