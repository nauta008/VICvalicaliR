
timeseries.plot <- function(data_sf, file=NULL){
  dims <- dim(data_sf)
  data_df <- as.data.frame(data_sf)

  if(!is.null(file)){
    pdf(file)
  }

  for(i_geom in 1:dim[1]){
    for(i_station in 1:dim[3]){
      # slice data for each time series
      # ggplot(data=df, aes(x=time,y=discharge, group=set, linetype=set, colour=set)) + geom_line(size=1) + theme_bw() + scale_linetype_manual(values = c("solid","dashed")) + scale_color_manual(values=c("black","blue"))

    }
  }

  dev.off()
}
