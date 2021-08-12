
tf.ks.cosby <- function(clay, sand,ks_gamma1=gamma_params$ks_gamma1$default,
                        ks_gamma2=gamma_params$ks_gamma2$default, ks_gamma3=gamma_params$ks_gamma3$default){

  # clay and sand must be in percentages
  ks <- 10^(ks_gamma1 + ks_gamma2 * clay + ks_gamma3 * sand) # which gives ks in inches/hr
  ks <- ks * 24 * 25.4 # convert to mm/day
  return(ks)

}
