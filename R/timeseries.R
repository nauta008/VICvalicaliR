
as.timeseries <- function(settings){
  timeseries <- new("Timeseries")
  timeseries@var <- settings$var
  if(!is.null(settings$filter)){
    # for(filter_setting in settings$filter){
    #   # init filter
    # }
  }
  for(agg_settings in settings$aggregation){
    # init aggregation
    agg <- as.aggregation(agg_settings)
    timeseries@aggregation <- append(timeseries@aggregation, agg)
  }

  return(timeseries)
}

setMethod("create",signature("Timeseries"), function(x){
  # load data from obs and sim
  obs_var <- .VICvalicaliR$settings$observation[[x@var]]
  obs_st <- ncdf.data.read(.VICvalicaliR$settings$observation$file,obs_var, x@start, x@end)
  sim_st <- ncdf.data.read(.VICvalicaliR$settings$simulation$file,"OUT_DISCHARGE", x@start, x@end)
  for(aggregation in x@aggregation){
    obs_st <- aggregate(aggregation,obs_st)
    sim_st <- aggregate(aggregation,sim_st)
  }
  obs_dims <- dim(obs_st)
  mask_obs <- stars::st_apply(obs_st, MARGIN=c("x","y","stations"), FUN=function(x){
    val <- 1
    # if all values in time series are NULL set mask value to NA.
    if(all(is.na(x))){
      val <- NA
    }
    return(val)
  })
  browser()
  names(mask_obs) <- "mask"
  # replicate along stations dimension
  sim_st <- stars::st_as_stars(replicate(obs_dims[4],sim_st), dimensions=stars::st_dimensions(sim_st))
  sim_st <- merge(sim_st) %>% stars::st_set_dimensions(4,values = st_get_dimension_values(obs_st,which = "stations"), names="stations")
  # apply observations mask over simulation data
  sim_st <- stars::st_apply(sim_st,MARGIN=c("time"),FUN=function(y){
    y * mask_obs$mask
  }) %>% aperm(perm = dimnames(obs_st))

  # convert to point geometries
  obs_sf <- stars::st_xy2sfc(obs_st, as_points = T)
  sim_sf <- stars::st_xy2sfc(sim_st, as_points = T)

  obs_xts <- xts::as.xts(obs_sf)
  sim_xts <- xts::as.xts(sim_sf)

  data_sf <- merge(st_as_stars(list(obs=obs_sf$dis,sim=sim_sf$X))) %>% stars::st_set_dimensions(4,names = "set",values = c("obs","sim"))
  # ggplot(data=df, aes(x=time,y=discharge, group=set, linetype=set, colour=set)) + geom_line(size=1) + theme_bw() + scale_linetype_manual(values = c("solid","dashed")) + scale_color_manual(values=c("black","blue"))

  return(data_sf)

})
