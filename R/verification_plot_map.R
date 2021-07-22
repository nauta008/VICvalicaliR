
setMethod("plot.map", signature("Verification"), function(x,st_data, file,...){
  maps <- list()
  var_names <- names(st_data)
  breaks <- get.breaks(x)

  if(!missing(file)){
    if(file.exists(file)){
      file.remove(file)
    }
    pdf(file, onefile = T, width = 8, height = 4)
    log_info(sprintf("Write verification maps of %s to file %s.", x@method, file))
  }

  for(i in 1:length(var_names)){
    i_veri_name <- var_names[i]
    i_breaks <- breaks[[i_veri_name]]
    st_data_slice <- st_data %>% select(i_veri_name) %>% stars::st_apply(MARGIN = c(1,2), rename=F,mean, na.rm=T)
    base_map <- .plot.basemap(.VICvalicaliR$domain$proj4, .VICvalicaliR$domain$bbox)
    title <- ggplot2::ggtitle(label = sprintf("%s %s", i_veri_name, x@dataset@var),subtitle = sprintf("%s-%s", as.character(x@start), as.character(x@end)))
    data_layer <- stars::geom_stars(data=st_data_slice)
    if(!is.null(i_breaks)){
      # from continious to discrete data
      st_data_slice <- cut(st_data_slice, breaks=i_breaks)
      data_layer <- stars::geom_stars(data=st_data_slice)
      # define discrete scale
      i_map <- base_map + data_layer + ggplot2::scale_fill_brewer(na.value="transparent", drop=F, palette = "RdYlGn", direction =1) +  ggplot2::guides(fill=ggplot2::guide_legend(reverse = T))
    }
    else{
      # define continous scale
      i_map <- base_map + data_layer + ggplot2::scale_fill_gradientn(na.value="transparent",colours= RColorBrewer::brewer.pal(10,"RdYlGn"))
    }
    i_map <- i_map + title
    maps[[i_veri_name]] <- i_map
    print(i_map)
  }
  dev.off()
  return(maps)
})


.plot.basemap <- function(proj4,bbox){

  x_coord_lim <- bbox[c(1,3)]
  y_coord_lim <- bbox[c(2,4)]

  countries_data <- sf::st_transform(countries, sf::st_crs(proj4))
  rivers_data <- sf::st_transform(rivers, sf::st_crs(proj4))

  p_countries <- ggplot2::geom_sf(data=countries_data, fill="transparent")
  p_rivers <- ggplot2::geom_sf(data=rivers_data, col="blue")

  p <- ggplot2::ggplot() + ggplot2::theme_bw(base_size = 12)
  p <- p + p_countries + p_rivers + ggplot2::coord_sf(crs = sf::st_crs(proj4), xlim = x_coord_lim,ylim = y_coord_lim, expand = F)
  return(p)
}
