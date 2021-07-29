
setMethod('data.get', signature("Dataset"), function(x, var_name ,conn){
  if(missing(conn) && is.null(x@from)){
    log_error(sprintf("Cannot identify source file for variable %s", var_name))
    stop()
  }

  if(!is.null(x@from) && missing(conn)){
    conn <- .VICvalicaliR$settings[[x@from]]$file
  }

  start <- NULL
  if(!is.null(x@start)){
    start <- x@start
  }
  end <- NULL
  if(!is.null(x@end)){
    end <- x@end
  }
  data_st <- ncdf.data.read(conn, var_name, sdt=start, edt=end)
  data_st <- .set.coords(data_st)

  for(data_filter in x@filter){
    data_st <- filter(data_filter,data_st)
  }

  for(data_aggregation in x@aggregation){
    data_st <- aggregate(data_aggregation,data_st)
  }
  return(data_st)

})
