
params <- read.csv(system.file("extdata","gamma_par.csv", package="VICvalicaliR"), header = T,stringsAsFactors = F, sep=",")

gamma_params <- list()

for(i in 1:nrow(params)){
  entry <- params[i,]
  gamma_params[[entry$name]] <- entry
}


usethis::use_data(gamma_params, overwrite = TRUE, internal = TRUE)
