
setMethod('aggregate', signature("TemporalAggregation"), function(x,st_data){
  log_debug(sprintf("Temporal aggregation of %s by %s", paste(names(st_data),sep = ","), x@by))
  dim_names <- dimnames(st_data)
  # TODO: some functions return multiple results. How to handle these output?
  if(x@by == "month" || x@by=="year" || x@by=="day"){
    # aperm will set dimension order back to original
    st_data <- stars:::aggregate.stars(st_data, by=x@by, FUN=x@fun, na.rm=T)
    st_data <- aperm(st_data,dim_names)
  }
  # TODO: how to handle aggregation with st_apply? Dimensions will change
  else if(x@by =="time"){
    st_data <- stars::st_apply(st_data, MARGIN=x@by,FUN=x@fun)
  }
  else{
    log_warn(sprintf("Cannot perform temporal aggregation on %s", paste(names(st_data),sep = ",")))
  }

  return(st_data)

})
