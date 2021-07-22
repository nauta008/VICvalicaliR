
as.dataset <- function(settings){
  dataset <- new("Dataset")
  # REQUIRED slots
  if(is.null(settings$start) || is.null(settings$end)){
    stop("Missing arguments start or end.")
  }
  dataset@var <- settings$var
  dataset@start <- lubridate::parse_date_time(settings$start,"ymd")
  dataset@end <- lubridate::parse_date_time(settings$end, "ymd")
  # OPTIONAL slots
  if(!is.null(settings$from)){
    dataset@from <- settings$from
  }
  if(!is.null(settings$filter)){
    for(filter_settings in settings$filter){
      if(is.null(filter_settings$data$start)){
        filter_settings$data$start <- settings$start
      }
      if(is.null(filter_settings$data$end)){
        filter_settings$data$end <- settings$end
      }
      i_filter <- as.filter(filter_settings)
      dataset@filter <- append(dataset@filter, i_filter)
    }
  }
  if(!is.null(settings$aggregation)){
    for(agg_settings in settings$aggregation){
      dataset@aggregation <- append(dataset@aggregation, as.aggregation(agg_settings))
    }
  }

  return(dataset)
}
