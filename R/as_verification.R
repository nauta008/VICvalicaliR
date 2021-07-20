
as.verification <- function(settings){
  # set properties of verification class from settings list
  verification <- new("Verification")
  verification@method <- toupper(settings$method)
  verification@start <- settings$start
  verification@end <- settings$end

  verification@dataset <- as.dataset(settings$data)

  return(verification)
}

