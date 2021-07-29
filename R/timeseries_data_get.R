
setMethod("data.get",signature("Timeseries"), function(x,...){

  data_st <- .validation.data.get(x@dataset)

  # convert to point geometries
  obs_sf <- stars::st_xy2sfc(data_st$obs, as_points = T)
  sim_sf <- stars::st_xy2sfc(data_st$sim, as_points = T)

  # obs_xts <- xts::as.xts(obs_sf)
  # sim_xts <- xts::as.xts(sim_sf)

  # original dimensions are lost after merging
  data_sf <- merge(stars::st_as_stars(list(obs=obs_sf %>% pull(),sim=sim_sf %>% pull()))) %>%
    setNames(x@dataset@var) %>% stars::st_set_dimensions(4,names = "set",values = c("obs","sim")) %>%
    stars::st_set_dimensions(which = "geometry", values = stars::st_get_dimension_values(obs_sf,which = "geometry"), names = "geometry") %>%
    stars::st_set_dimensions(which = "time", values = stars::st_get_dimension_values(obs_sf, which = "time"), names = "time") %>%
    stars::st_set_dimensions(which = "stations", values = stars::st_get_dimension_values(obs_sf,which = "stations"),names = "stations")

  return(data_sf)
})
