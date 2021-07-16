

setMethod('data.get', signature("Dataset"), function(x,conn, var_name , ...){

  data_st <- ncdf.data.read(conn, var_name, ...)

  for(data_aggregation in x@aggregation){
    data_st <- aggregate(data_aggregation,data_st)
  }
  return(data_st)

})
