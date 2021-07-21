
#' as params
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
as.params <- function(file){
  params <- new("Params", file=file)
  return(params)
}
