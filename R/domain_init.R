
domain.init <- function(conn){

  mask <- stars::read_ncdf(conn, var="mask")

  crs_domain <- sf::st_crs(mask)
  if(!is.na(crs_domain)){
    .VICvalicaliR$domain$proj4 <- crs_domain$proj4string
  }
  else if(!is.null(.VICvalicaliR$settings$proj4)){
    .VICvalicaliR$domain$proj4 <- .VICvalicaliR$settings$proj4
  }
  else{
    log_warn("Could not determine Coordinate Reference System from domain file of config. Please add CRS to NetCDF or proj4 property to config.")
  }
  log_info(sprintf("Using proj4 %s", .VICvalicaliR$domain$proj4))

  .VICvalicaliR$domain$bbox <- sf::st_bbox(mask)
  # assumes first dimension to be x or lon, second dimension to be y or lat
  .VICvalicaliR$domain$xvals <- stars::st_get_dimension_values(mask,which = 1, center = T)
  .VICvalicaliR$domain$yvals <- stars::st_get_dimension_values(mask,which = 2, center = T)
}

.set.coords <- function(st_data){
  st_data <- stars::st_set_dimensions(st_data, which = 1, values=.VICvalicaliR$domain$xvals)
  st_data <- stars::st_set_dimensions(st_data, which = 2, values=.VICvalicaliR$domain$yvals)
  st_data <- sf::st_set_crs(st_data, value=.VICvalicaliR$domain$proj4)
  return(st_data)
}
