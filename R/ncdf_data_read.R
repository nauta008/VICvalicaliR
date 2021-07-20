
ncdf.data.read <- function(conn,var, sdt, edt){
  log_info(sprintf("Read %s from %s", var,conn))
  nc <- ncdf4::nc_open(conn)
  # get dim names
  dim_names <- sapply(nc$var[[var]]$dim,FUN = function(x){x$name})
  dim_lenghts <- sapply(nc$var[[var]]$dim,FUN = function(x){x$len})
  # set subset
  subset <- cbind(start=rep(1,times=length(dim_names)),count=dim_lenghts)
  # assume ncdf always have x and y
  nx <- nc$dim$x$len
  ny <- nc$dim$y$len
  # check for time dimension
  if("time" %in% dim_names){
    t_units <- unlist(strsplit(nc$dim$time$units, split = " "))
    time <- as.POSIXct(ncdf4.helpers::nc.get.time.series(nc))
    # UBER HACK for time origins 0001-01-01. In some systems there is a 2 day time difference. I have no better method to fix this
    if(as.Date(t_units[3])== as.Date("0001-01-01") &&  as.Date( nc$dim$time$vals[1], origin = "0001-01-01") == as.Date("1993-01-03")){
      log_warn(sprintf("Fixing time time for %s. Please check timeseries for this file using ncdf4.helpers:: nc.get.time.series(%s)", conn,conn))
      time <- time - lubridate::days(2)
    }
    time_dim_idx <- which(dim_names=="time")
    sdt_idx <- 1
    edt_idx <- nc$dim$time$len
    if(! missing(sdt)){
      sdt_idx <- which(as.character(time)==as.character(sdt))
      if(length(sdt_idx)!=1){
        log_warn(sprintf("Cannot find start date %s in ncdf file %s. Read all times from start.", as.character(sdt),conn))
        sdt_idx <- 1
      }
    }
    if(! missing(edt)){
      edt_idx <- which(as.character(time)==as.character(edt))
      if(length(edt_idx)!= 1){
        log_warn(sprintf("Cannot find end date %s in ncdf file %s. Read all times till end", as.character(edt),conn))
        edt_idx <- 1
      }
    }
    subset[time_dim_idx,"start"] <- sdt_idx
    subset[time_dim_idx,"count"] <- edt_idx - sdt_idx + 1
  }

  ncdf4::nc_close(nc)
  # read into stars data
  st_data <- stars::read_ncdf(conn,var=var, ncsub=subset, make_units = F)
  if("time" %in% dim_names){
    time_subset <- time[sdt_idx:edt_idx]
    st_data <- stars::st_set_dimensions(st_data,which = "time", values=time_subset)
  }
  return(st_data)
}
