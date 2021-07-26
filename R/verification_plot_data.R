
# TODO: FINISH
setMethod("plot.data", signature("Verification"), function(x,  base_map , st_data){
  ggplot <- NULL
  var_name <- names(st_data)[1]
  if(toupper(x@method) =="KGE"){

    if(var_name == "kge"){
      ggplot <- base_map + stars::geom_stars(data=st_data) +
        ggplot2::scale_fill_stepsn(na.value="transparent", colors=RColorBrewer::brewer.pal(10,"RdYlGn") , limits=c(0,1),  n.breaks=10, oob=scales::squish)
    }
    else if(var_name == "corr"){
      ggplot <-  base_map + stars::geom_stars(data=st_data) +
        ggplot2::scale_fill_stepsn(na.value="transparent", colors=RColorBrewer::brewer.pal(10,"RdYlGn") ,limits=c(0,1), n.breaks=10)
    }
    else if(var_name == "rat_sd"){
      ggplot <- base_map + stars::geom_stars(data=cut(st_data, breaks= c(0,0.1,0.5,0.75,0.9,1.1,1.5,2,10, Inf))) +
        ggplot2::scale_fill_manual(drop=F,na.value="transparent", values= RColorBrewer::brewer.pal(9,"RdBu")) + ggplot2::theme_dark()
    }
    else if(var_name =="rat_mean"){
      ggplot <- base_map + stars::geom_stars(data=cut(st_data, breaks= c(0,0.1,0.5,0.75,0.9,1.1,1.5,2,10, Inf))) +
        ggplot2::scale_fill_manual(drop=F,na.value="transparent",values= RColorBrewer::brewer.pal(9,"RdBu"))  + ggplot2::theme_dark()
    }
    else if(var_name == "rat_cv"){
      ggplot <- base_map + stars::geom_stars(data=cut(st_data, breaks= c(0,0.1,0.5,0.75,0.9,1.1,1.5,2,10, Inf))) +
        ggplot2::scale_fill_manual(drop=F,na.value="transparent", values= RColorBrewer::brewer.pal(9,"RdBu")) +ggplot2::theme_dark()
    }
  }
  else if(toupper(x@method) =="NSE"){

    ggplot <- base_map + stars::geom_stars(data=st_data) +
      ggplot2::scale_fill_stepsn(na.value="transparent", colors=RColorBrewer::brewer.pal(10,"RdYlGn"), limits=c(0,1) ,n.breaks=10, oob=scales::squish)
  }
  return(ggplot)
})
