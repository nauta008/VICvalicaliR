

#' Run Tool
#'
#' @param config
#'
#' @return
#' @export
#' @name run_tool
#' @examples
#' # example of a validation
#' con <- system.file("extdata","validation_config.yml", package="VICvalicaliR")
#' run_tool(con)
#' con <- system.file("extdata","calibration_config.yml", package="VICvalicaliR")
#' files_to_copy <- c(system.file("extdata","vic_image.exe", package="VICvalicaliR"),system.file("extdata","vic_global_params.txt", package="VICvalicaliR"),
#' system.file("extdata","pr_WFDE5_3h_1993.nc", package="VICvalicaliR"),system.file("extdata","psurf_WFDE5_3h_1993.nc", package="VICvalicaliR"),
#' system.file("extdata","tas_WFDE5_3h_1993.nc", package="VICvalicaliR"),system.file("extdata","wind_WFDE5_3h_1993.nc", package="VICvalicaliR"),
#' system.file("extdata","lwdown_WFDE5_3h_1993.nc", package="VICvalicaliR"), system.file("extdata","swdown_WFDE5_3h_1993.nc", package="VICvalicaliR"),
#' system.file("extdata","vp_WFDE5_3h_1993.nc", package="VICvalicaliR"),
#' system.file("extdata","rhine_domain.nc", package="VICvalicaliR"),system.file("extdata","rhine_vic_params.nc", package="VICvalicaliR"),
#' system.file("extdata","rhine_lisflood_routing_params.nc", package="VICvalicaliR"),system.file("extdata","init_state.nc", package="VICvalicaliR") )
#' dir.create("./example_calibration", showWarnings=F)
#' for(fpath in files_to_copy){
#'   if(!file.exists(fpath)){
#'      system(sprintf("cp %s ./example_calibration", fpath))
#'   }
#'
#' }
#' run_tool(con)
run_tool <- function(config){
  start_dt <- lubridate::now()
  settings.read(config)
  # create working directory
  dir.create(.VICvalicaliR$settings$output, recursive = T ,showWarnings = F)
  # add log messages to console and file
  logger.init()
  log_info(sprintf("Start VICvalicaliR at %s", as.character(lubridate::now())))
  # init domain
  domain.init(.VICvalicaliR$settings$domain$file)
  if(.VICvalicaliR$settings$mode == "validation"){
    validation <- as.validation(.VICvalicaliR$settings$validation)
    run(validation)
  }
  if(.VICvalicaliR$settings$mode == "calibration"){
    # TODO: validate required settings
    calibration <- as.calibration(.VICvalicaliR$settings$calibration)
    start(calibration)

  }
  end_dt <- lubridate::now()
  timing <- round(lubridate::as.period(lubridate::as.interval(start_dt,end_dt) ), digits = 2)
  log_success(sprintf("VICvalicalR finished in %s.", as.character(timing)))
}
