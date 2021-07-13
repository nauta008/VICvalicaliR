
ncdf.create <- function(file){
  # remove old file
  file.remove(file)
  sys_info <- Sys.info()
  nc_vars <- list()
  # load domain
  nc_domain <- ncdf4::nc_open(.VICvalicaliR$settings$domain$file)
  mask <- ncdf4::ncvar_get(nc_domain,"mask")
  # create new ncdf file with domain properties
  nc_new <- ncdf4::nc_create(file,nc_domain$var$mask, force_v4 = T)
  ncdf4::ncvar_put(nc_new,"mask",vals = mask)
  ncdf4::ncatt_put(nc_new,"mask",attname = "missing_value", attval = nc_domain$var$mask$missval, prec = "integer")
  # add lat and lon if not regular lat lon grid
  if(!is.null(nc_domain$var$lat) && !is.null(nc_domain$var$lon)){
    lat_vals <- ncdf4::ncvar_get(nc_domain,"lat")
    lon_vals <- ncdf4::ncvar_get(nc_domain,"lon")
    ncdf4::ncvar_add(nc_new,nc_domain$var$lat)
    ncdf4::ncvar_add(nc_new,nc_domain$var$lon)
    ncdf4::nc_close(nc_new)
    nc_new <- ncdf4::nc_open(file,write = T)
    ncdf4::ncvar_put(nc_new,"lat",lat_vals)
    ncdf4::ncvar_put(nc_new,"lon",lon_vals)
  }
  # add global attributes
  ncdf4::ncatt_put(nc_new,0,attname = "user",attval = sys_info[["user"]],prec = "text")
  ncdf4::ncatt_put(nc_new,0, attname = "date_created", attval = as.character(lubridate::now()))
  # close file
  ncdf4::nc_close(nc_new)
  ncdf4::nc_close(nc_domain)
}

ncdf.write <- function(file, st_data){
  log_info(sprintf("Writing %s to %s", names(st_data), file))
}
