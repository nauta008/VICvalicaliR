
nse <- function(sim,obs){
  log_debug("Calculate Nash–Sutcliffe efficiency")
  data <- sim
  data$obs <- obs %>% pull()
  data <- merge(data, attributes=c("sim","obs"))
  res <- stars::st_apply(data, MARGIN=c("x","y","stations"), FUN=function(data_series){
    bias <- data_series[,1] - data_series[,2]
    a <- sum(bias^2, na.rm=T)
    c <- sum((data_series[,2] - mean(data_series[,2], na.rm = T))^2, na.rm = T)
    #b <- apply(data_series[,2], FUN=function(obs_series){ (obs_series - mean(obs_series, na.rm=T))^2})
    #c <- sum(b, na.rm=T)
    val <- 1 - a/c
    return(val)
  })
  names(res) <- "NSE"
  return(res)
}

kge <- function(sim,obs, use_gamma=T){
  log_debug("Calculate Kling-Gupta Efficiency")
  #data <- c(sim=sim, obs=obs)
  #names(data) <- c("sim","obs")
  data <- sim
  data$obs <- obs %>% pull()
  data <- merge(data, attributes=c("sim","obs"))
  kge_components <- function(data_series){
    r_pearson <- cor(data_series[,1], data_series[,2], method="pearson", use="pairwise.complete.obs")
    # means
    mean_sim <- mean(data_series[,1], na.rm=T)
    mean_obs <- mean(data_series[,2], nar.rm=T)
    if(!is.na(mean_obs) && mean_obs == 0){
      mean_obs <- NA
    }
    # standard deviation
    sd_sim <- sd(data_series[,1], na.rm=T)
    sd_obs <- sd(data_series[,2], na.rm=T)
    if(!is.na(sd_obs) && sd_obs == 0){
      sd_obs <- NA
    }
    # relative variability
    alpha <- sd_sim/sd_obs
    # ratio of means
    beta <- mean_sim/mean_obs
    # variation coefficient
    cv_sim <- sd_sim/mean_sim
    cv_obs <- sd_obs/mean_obs
    # variability ratio
    gamma <- cv_sim/cv_obs

    vr <- alpha
    if(use_gamma){
      vr <- gamma
    }

    kge <- 1 - sqrt((1-r_pearson)^2 + (1-alpha)^2 + (1-vr)^2)
    val <- c(kge,r_pearson,alpha,beta,gamma)
    return(val)
  }
  res <- stars::st_apply(data, MARGIN=c("x","y","stations"), kge_components )
  res <- split(res,"kge_components")
  names(res) <- c("kge","corr","rat_sd","rat_mean","rat_cv")
  return(res)
}

