
as.verification <- function(settings){
  # set properties of verification class from settings list
  verification <- new("Verification")
  verification@method <- toupper(settings$method)
  verification@var <- settings$var
  verification@start <- settings$start
  verification@end <- settings$end
  return(verification)
}

#' @importFrom dplyr slice pull %>%
setMethod("start", signature("Verification") ,function(x){
  log_info(sprintf("Start model verification on %s and method %s", x@var, x@method))
  # load time series from observations
  nc_obs <- ncdf4::nc_open(.VICvalicaliR$settings$observation$file)
  nc_sim <- ncdf4::nc_open(.VICvalicaliR$settings$simulation$file)
  obs_time <- ncdf4.helpers::nc.get.time.series(nc_obs)
  sim_time <- ncdf4.helpers::nc.get.time.series(nc_sim)
  nx <- nc_sim$dim$x$len
  ny <- nc_sim$dim$y$len
  nstations <- nc_obs$dim$stations$len
  ncdf4::nc_close(nc_obs)
  ncdf4::nc_close(nc_sim)
  # find indexes for obs
  obs_sdt_idx <- which(as.character(obs_time)==as.character(x@start))
  obs_edt_idx <- which(as.character(obs_time)==as.character(x@end))
  # find indexes for sim
  sim_sdt_idx <- which(as.character(sim_time)==as.character(x@start))
  sim_edt_idx <- which(as.character(sim_time)==as.character(x@end))
  # get variable name in ncdf file
  obs_varname <- .VICvalicaliR$settings$observation[[x@var]]

  sim_ncsub <- cbind(start=c(1,1,sim_sdt_idx), count=c(nx,ny,sim_edt_idx-sim_sdt_idx+1))
  obs_ncsub <- cbind(start=c(1,1,obs_sdt_idx,1), count=c(nx,ny,obs_edt_idx-obs_sdt_idx+1,nstations))
  log_debug("Read simulation data.")
  # different routines for verification variables
  if(x@var == "dis"){
    sim <- stars::read_ncdf(.VICvalicaliR$settings$simulation$file, var = "OUT_DISCHARGE", ncsub = sim_ncsub, make_units = F )
  }
  # calc specdis from simulation discharge and model upstream area
  if(x@var == "specdis"){
    sim_dis <- stars::read_ncdf(.VICvalicaliR$settings$simulation$file, var = "OUT_DISCHARGE", ncsub = sim_ncsub ,make_units = F)
    upstream_area_var <- .VICvalicaliR$settings$routing[["upstream_area"]]
    upstream_area <- stars::read_ncdf(.VICvalicaliR$settings$routing$file, var = upstream_area_var)
    # specific discharge in mm/d
    sim <- sim_dis / upstream_area  * (3600 * 24  / 1000)
  }
  log_debug("Read observation data.")
  obs <- stars::read_ncdf(.VICvalicaliR$settings$observation$file, var = obs_varname, ncsub = obs_ncsub, make_units = F)
  browser()
  sim <- stars::st_as_stars(replicate(sim %>% pull(),n = dim(obs)[4]), dimensions=stars::st_dimensions(obs))
  res_st <- NULL
  if(x@method=="NSE"){
    res_st <- nse(sim,obs)
  }
  names(res_st) <- sprintf("%s.%s",x@method,x@var)
  return(res_st)
})
