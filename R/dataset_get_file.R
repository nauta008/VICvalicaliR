
setMethod("get.file", signature("Dataset"), function(x,file_format="pdf", prefix=""){

  f_path <- file.path(.VICvalicaliR$settings$output,"plot")
  f_name <- sprintf("%s", x@var)

  if(length(x@aggregation)>0){
    agg_list <- list()
    for(aggregation in x@aggregation){
      if(aggregation@type=="temporal"){
        agg_name <- sprintf("by_%s", aggregation@by)
        agg_list <- append(agg_list,agg_name)
      }
      agg_sub_name <- sprintf("agg_%s", paste(agg_list, collapse = "_"))

    }
    f_name <- sprintf("%s_%s", f_name, agg_sub_name)
  }

  f_name <- sprintf("%s_%s.%s", prefix, f_name, file_format)
  return(f_name)

})
