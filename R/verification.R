
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
  x@state <- "RUNNING"
  log_info(sprintf("Start model verification on %s and method %s", x@var, x@method))
  # get variable name in ncdf file
  obs_varname <- .VICvalicaliR$settings$observation[[x@var]]
  log_debug("Read simulation data.")
  # different routines for verification variables
  if(x@var == "dis"){
    sim <- ncdf.data.read(.VICvalicaliR$settings$simulation$file, "OUT_DISCHARGE", x@start, x@end)
  }
  # calc specdis from simulation discharge and model upstream area
  if(x@var == "specdis"){
    sim_dis <- ncdf.data.read(.VICvalicaliR$settings$simulation$file, "OUT_DISCHARGE", x@start, x@end)
    upstream_area_var <- .VICvalicaliR$settings$routing[["upstream_area"]]
    upstream_area <- ncdf.data.read(.VICvalicaliR$settings$routing$file, upstream_area_var)
    # specific discharge in mm/d
    sim <- sim_dis / upstream_area  * (3600 * 24  * 1000) # seconds -> day & meters -> mm
  }
  log_debug("Read observation data.")
  # load obs data
  obs <- ncdf.data.read(.VICvalicaliR$settings$observation$file, obs_varname, x@start,x@end)
  # set simulation to equal 4th dimension
  sim <- stars::st_as_stars(replicate(sim %>% pull(),n = dim(obs)[4]), dimensions=stars::st_dimensions(obs))
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

  # set attributes names
  if(length(res_st)==1){
    names(res_st) <- sprintf("%s_%s",x@var,x@method)
  }
  # for verifcations with sub components. e.g. KGE
  else if(length(res_st)>1){
    names(res_st) <- sapply(names(res_st), FUN=function(attr_name){
      new_name <- sprintf("%s_%s_%s",x@var,x@method,attr_name)
    })
  }
  x@state <- "SUCCESS"
  return(res_st)
})
