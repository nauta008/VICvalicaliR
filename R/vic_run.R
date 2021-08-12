
vic.run <- function(global_params_file,wd=getwd(),vic_exe=system.file("extdata","vic_image.exe", package="VICvalicaliR")){

  curr_wd <- getwd()
  setwd(wd)
  system(sprintf("%s -g %s", vic_exe, global_params_file))
  setwd(curr_wd)
}
