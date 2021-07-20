
as.filter <- function(settings){
  filter <- new("Filter")
  filter@dataset <- as.dataset(settings$data)
  filter@operator <- settings$operator
  filter@value <- as.numeric(settings$value)
  return(filter)
}
