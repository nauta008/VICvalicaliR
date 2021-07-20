
setMethod("filter", signature("Filter"), function(x, st_data){
  var_name <- names(st_data)
  # The filter stars data result must contain x,y
  filter_st <- data.get(x@dataset, x@dataset@var)
  filtered_arr <- filter_st %>% pull()
  if(x@operator=="gt"){
    filtered_arr[filtered_arr < x@value] <- NA
    filtered_arr[!is.na(filtered_arr)] <- 1
  }
  if(x@operator == "lt"){
    filtered_arr[filtered_arr > x@value] <- NA
    filtered_arr[!is.na(filtered_arr)] <- 1
  }
  mask_st <- stars::st_as_stars(filtered_arr, dimensions=stars::st_dimensions(filter_st))
  mask_dims <- dim(mask_st)
  data_dims <- dim(st_data)
  # replicate one of the data objects so we have equal dimensions. We apply mask over x,y. So we check for dimensions stations
  if(!all(mask_dims == data_dims)){
    for(i_mask_dim in 1:length(mask_dims)){
      # if dimension is missing in data. we replicate the data
      mask_dim_name <- names(mask_dims)[i_mask_dim]
      n_dim <- mask_dims[i_mask_dim]
      if(!mask_dim_name %in% names(data_dims)){
        st_data <- stars::st_as_stars(replicate(n_dim,st_data), dimensions=stars::st_dimensions(st_data))
        along <- list()
        along[[mask_dim_name]] = stars::st_get_dimension_values(mask_st, which = mask_dim_name, center=T)
        st_data <- stars::st_redimension(st_data, along=along)
      }
    }
  }

  dim_order <- dimnames(st_data)
  st_data <- stars::st_apply(st_data,MARGIN=c("time"),FUN=function(y){
    y * mask_st %>% pull()
  }) %>% aperm(perm=dim_order)

  #st_data <- st_data * mask_st %>% stars::st_as_stars(dimensions=stars::st_dimensions(st_data))
  names(st_data) <- var_name
  return(st_data)

})
