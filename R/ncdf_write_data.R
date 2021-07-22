
#' create ncdf
#'
#' @param file
#' @param domain_file
#' @description Creates new NetCDF file from VIC domain file. Writes mask, grid, lat and lon variables.
#'
#' @return
#' @export
#'
#' @examples
ncdf.create <- function(file, domain_file){
  # remove old file
  if(file.exists(file)){
    file.remove(file)
  }
  sys_info <- Sys.info()
  nc_vars <- list()
  # load domain
  nc_domain <- ncdf4::nc_open(domain_file)
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
  ncdf4::ncatt_put(nc_new, 0, attname = "VICvalicaliR_version", attval = as.character(packageVersion("VICvalicaliR")))
  ncdf4::ncatt_put(nc_new, 0, attname = "VICvalicaliR_reference", attval = "https://github.com/nauta008/VICvalicaliR", prec = "text")

  # close file
  ncdf4::nc_close(nc_new)
  ncdf4::nc_close(nc_domain)
  log_info(sprintf("Created ncdf output file %s", file))
}


#' write data
#'
#' @param file
#' @param st_data
#' @description Writes a stars data object to NetCDF file
#'
#' @return
#' @importFrom dplyr select
#' @export
#'
#' @examples
ncdf.write.data <- function(file, st_data){
  log_info(sprintf("Writing %s to %s", paste(names(st_data),collapse = ', '), file))
  var_names <- names(st_data)
  data_dim_names <- dimnames(st_data)
  data_dims <- dim(st_data)

  # open file
  nc <- ncdf4::nc_open(file, write = T)
  file_dim_names <- names(nc$dim)

  nc_dim_list <- list()
  # loop through dims
  for(dim_name in data_dim_names){
    if(dim_name %in% file_dim_names){
      nc_dim <- nc$dim[[dim_name]]
    }
    else{
      nc_dim <- ncdf4::ncdim_def(dim_name,units = "", vals=stars::st_get_dimension_values(st_data,which = dim_name, center = T))
    }
    nc_dim_list[[dim_name]] <- nc_dim
  }

  # loop through attributes
  for(var_name in var_names){
    # create new var
    if(!var_name %in% names(nc$var)){
      long_name <- var_name
      nc_var <- ncdf4::ncvar_def(var_name,"", nc_dim_list, 1e20, longname = long_name,prec = "float",compression = 9)
      ncdf4::ncvar_add(nc,nc_var)
    }
    else{
      nc_var <- nc$var[[var_name]]
    }
    ncdf4::nc_close(nc)
    nc <- ncdf4::nc_open(file, write = T)
    ncdf4::ncvar_put(nc,var_name, vals= st_data %>% select(var_name) %>% pull())
  }
  ncdf4::nc_close(nc)
}
