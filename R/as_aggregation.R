
as.aggregation <- function(settings){
  agg <- NULL
  if(toupper(settings$type) == "TEMPORAL"){
    agg <- as.temporal_aggregation(settings)
  }
  return(agg)
}

as.temporal_aggregation <- function(settings){
  agg <- new("TemporalAggregation", by=settings$by)
  # optional settings
  if(!is.null(settings$fun)){
    agg@fun <- settings$fun
  }
  return(agg)
}

