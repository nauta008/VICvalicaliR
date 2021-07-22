
#' @export
.VICvalicaliR <- new.env(parent = emptyenv())

.VICvalicaliR$settings <- list(
  mode=NULL,
  log=NULL,
  proj4=NULL,
  log_level="INFO"
)

.VICvalicaliR$cache <- list(

)

.VICvalicaliR$domain <- list(
  proj4 <- NA,
  bbox <- NA,
  xvals <- NA,
  yvals <- NA
)
