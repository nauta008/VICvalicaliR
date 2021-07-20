
timeseries.plot <- function(data_sf, file=NULL){
  log_debug("Plot validation timeseries")
  dims <- dim(data_sf)
  var_name <- names(data_sf)

  if(!is.null(file)){
    if(file.exists(file)){
      file.remove(file)
    }
    log_info(sprintf("Write time series plots of %s to file %s.", var_name, file))
    pdf(file,width = 8, height = 4, onefile = T)
  }
  # TODO: maybe nice to plot a map with location of station and table with meta: upstream area, medium discharge, verification score ect.
  for(i_station in 1:dims[3]){
    for(i_geom in 1:dims[1]){
      data_slice_sf <- data_sf %>% slice(stations,i_station) %>% slice(geometry, i_geom)
      if(!all(is.na(data_slice_sf[[var_name]]))){
        p_ts <- ggplot2::ggplot(as.data.frame(data_slice_sf), ggplot2::aes_string(x="time", y=var_name,group="set",color="set",linetype="set")) +
          ggplot2::geom_line() + ggplot2::scale_color_manual(values = c("black","blue")) + ggplot2:: theme_bw()
        print(p_ts)
      }
    }
  }

  dev.off()
}
