
params <- read.csv(system.file("extdata","beta_par.csv", package="VICvalicaliR"), header = T,stringsAsFactors = F, sep=",")

beta_params <- list()

for(i in 1:nrow(params)){
  entry <- params[i,]
  beta_params[[entry$name]] <- entry
}


usethis::use_data(beta_params, overwrite = TRUE, internal = TRUE)
