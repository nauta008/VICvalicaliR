
as.validation <- function(settings){
  validation <- new("Validation")
  validation@start <- lubridate::parse_date_time(settings$start, "ymd")
  validation@end <- lubridate::parse_date_time(settings$end, "ymd")
  for(veri_settings in settings$verification){
    veri_settings$start <- settings$start
    veri_settings$end <- settings$end
    new_verification <- as.verification(veri_settings)
    validation@verifications <- append(validation@verifications, new_verification)
  }
  for(timeseries_settings in settings$timeseries){
    if(is.null(timeseries_settings$data$start)){
      timeseries_settings$data$start <- settings$start
    }
    if(is.null(timeseries_settings$data$end)){
      timeseries_settings$data$end <- settings$end
    }
    ts <- as.timeseries(timeseries_settings)
    validation@timeseries <- append(validation@timeseries,ts)
  }
  return(validation)
}


.validation.data.get <- function(dataset){
  obs_var_name <- .VICvalicaliR$settings$observation[[dataset@var]]
  sim_var_name <- "OUT_DISCHARGE"
  # read ncdfs
  log_debug(sprintf("Read %s from simulation data.", sim_var_name))
  #sim_data_st <- ncdf.data.read(.VICvalicaliR$settings$simulation$file,sim_var_name,...)
  sim_data_st <- data.get(dataset, sim_var_name, conn=.VICvalicaliR$settings$simulation$file)
  log_debug(sprintf("Read %s from observation data.", obs_var_name))
  #obs_data_st <- ncdf.data.read(.VICvalicaliR$settings$simulation$file, obs_var_name,...)
  obs_data_st <- data.get(dataset, obs_var_name, conn=.VICvalicaliR$settings$observation$file)
  # check for specific discharge
  if(dataset@var == "specdis"){
    log_debug("Calculate specifc discharge")
    upstream_area_var <- .VICvalicaliR$settings$routing[["upstream_area"]]
    upstream_area <- ncdf.data.read(.VICvalicaliR$settings$routing$file, upstream_area_var)
    # specific discharge in mm/d
    sim_data_st <- sim_data_st / upstream_area  * (3600 * 24  * 1000) # seconds -> day & meters -> mm
  }
  # create mask for obs
  mask_obs <- stars::st_apply(obs_data_st, MARGIN=c("x","y","stations"), FUN=function(x){
    val <- 1
    # if all values in time series are NULL set mask value to NA.
    if(all(is.na(x))){
      val <- NA
    }
    return(val)
  })
  obs_dims <- dim(obs_data_st)
  sim_dims <- dim(sim_data_st)
  names(mask_obs) <- "mask"
  if(!all(obs_dims == sim_dims)){
    # set simulation to equal 4th dimension
    sim_data_st <- stars::st_as_stars(replicate(obs_dims[4],sim_data_st), dimensions=stars::st_dimensions(sim_data_st))
    sim_data_st <- merge(sim_data_st) %>% stars::st_set_dimensions(4,values = stars::st_get_dimension_values(obs_data_st,which = "stations"), names="stations")
  }
  # apply observations mask over simulation data
  sim_data_st <- stars::st_apply(sim_data_st,MARGIN=c("time"),FUN=function(y){
    y * mask_obs$mask
  }) %>% aperm(perm = dimnames(obs_data_st))

  return(list(obs=obs_data_st,sim=sim_data_st))
}
