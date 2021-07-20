
as.dataset <- function(settings){
  dataset <- new("Dataset")
  # REQUIRED slots
  dataset@var <- settings$var
  # OPTIONAL slots
  if(!is.null(settings$aggregation)){
    for(agg_settings in settings$aggregation){
      dataset@aggregation <- append(dataset@aggregation, as.aggregation(agg_settings))
    }
  }

  return(dataset)
}
