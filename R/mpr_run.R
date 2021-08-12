
MPR.run <- function(beta_param_names, gamma_params, predictors ,beta_params_file){

  # beta_param_names should be ordered on dependencies
  for(bname in beta_param_names){

    if(bname == "ks"){

      sand <- ncdf.data.read(predictors$soil, "sand")
      clay <- ncdf.data.read(predictors$soil, "clay")
      ztot <- ncdf.data.read(predictors$soil, "ztot")
      depth <- ncdf.data.read(beta_params_file, "depth")
      beta_params <- tf.ks.cosby(clay, sand, gamma_params$ks_gamma1,gamma_params$ks_gamma2, gamma_params$ks_gamma3)

    }

  }

}


model.depth.calc <- function(hfrac,ztot){
  # assumes, three model layers
  hmodel <- array(NA, dim = c(dim(ztot),3))
  z_top_model <- array(NA, dim = c(dim(ztot),3))
  model_lyr_top <- array(0, dim=dim(ztot))
  for(ilyr in 1:2){
    hmodel[,,ilyr] <- hfrac[ilyr] * (ztot-model_lyr_top)
    z_top_model[,,ilyr] <- model_lyr_top + hmodel[,,ilyr]
    model_lyr_top <- z_top_model[,,ilyr]
  }
  ilyr <- ilyr + 1
  hmodel[,,ilyr] <- ztot - model_lyr_top
  z_top_model[,,ilyr] <- ztot
  return(list(hmodel=hmodel, zmodel=z_top_model))

}

soil.layer.thickness.calc <- function(soil_data, ztot){
  n_slyr <- dim(soil_data)[3]
  z_top_slyrs <- st_get_dimension_values(soil_data, which = "depth")
  h_slyrs <- diff(z_top_soil)
  h_soil <- array(NA, dim=dim(soil_data))
  for(z in 1:(n_slyr-1)){
    h_soil[,,z] <- h_slyrs[z]
  }
  h_soil[,,n_slyr] <- ztot %>% pull() - z_top_slyrs[n_slyr]
  return(h_soil)
}


soil.layer.weights.calc.par <- function(soil_lyr_thicnkness, mod_lyr_bottom_depth){
  n_slyr <<- dim(soil_lyr_thicnkness)[3]
  n_mlyr <<- dim(mod_lyr_bottom_depth)[3]
  nx <- 1:dim(mod_lyr_bottom_depth)[1]
  ny <- 1:dim(mod_lyr_bottom_depth)[2]
  xy <- expand.grid(nx,ny)

  calc.weights <- function(cell){
    idx_top <- 1
    z_top_model <- 0
    x <- unlist(cell[1])
    y <- unlist(cell[2])
    w <- array(NA, dim=c(n_mlyr,n_slyr))
    if(!all(is.na(mod_lyr_bottom_depth[x,y,]))){
      for(z in 1:n_mlyr){
        z_bottom_model <- mod_lyr_bottom_depth[x,y,z]
        h_mlyr <- z_bottom_model - z_top_model
        hsoil <- soil_lyr_thicnkness[x,y,]
        # till which soil layer index reaches the model layer
        idx_bottom <- which(z_bottom_model-cumsum(hsoil) <= 0)
        if(length(idx_bottom)==0){
          idx_bottom <- NA
        }
        else{
          idx_bottom <- idx_bottom[1]
        }
        # compute weights
        wghts <- rep(0, n_slyr)
        # model layer is in one soil layer
        if(idx_top == idx_bottom){
          # all soil layers has zero weight except the found index
          wghts[idx_top] <- 1
        }
        # model layer has multiple soil layers. compute weights for every soil layer in model layer
        else{
          # for bottom and top soil layer we should take a fraction of the soil layer depth
          for(slyr in idx_top:idx_bottom){
            if(slyr==idx_top){
              h_slyr <- sum(soil_lyr_thicnkness[x,y,1:idx_top]) - z_top_model
            }
            else if(slyr==idx_bottom){
              h_slyr <- z_bottom_model - sum(soil_lyr_thicnkness[x,y,1:(idx_bottom-1)])
              wghts[idx_bottom] <- h_slyr/h_mlyr
            }
            else{
              h_slyr <- soil_lyr_thicnkness[x,y,slyr]
            }
            wghts[slyr] <- h_slyr/h_mlyr
          }
        }

        # validate
        if(abs(1-sum(wghts)> 1e-8)){
          log_error(sprintf("Sum of soil layer weights should equal 1. x,y,z = %s, %s, %s",x,y,z))
          stop()
        }
        w[z,] <- wghts
        # go to next model lyr and set next top to current bottom
        idx_top <- idx_bottom
        z_top_model <- z_bottom_model
      }
    }
    return(w)
  }

  l <-  list()

  tic("make list")
  for(i in 1:dim(xy)[1]){
    l[[i]] <- xy[i,]
  }
  toc()

  tic("calc weights")
  res <- mclapply(l,FUN = calc.weights)
  toc()

  tic("as array")
  w <- array(as.numeric(unlist(res)), dim=c(n_mlyr,n_slyr,dim(mod_lyr_bottom_depth)[1],dim(mod_lyr_bottom_depth)[2]))
  toc()

  tic("reshape array")
  w_new <- aperm(w,c(3,4,1,2))
  toc()

  #stopCluster(cl)
  #w <- soil_lyr_weights
  #rm(soil_lyr_weights)
  return(w_new)

}

soil.layer.weights.calc <- function(soil_lyr_thicnkness, mod_lyr_bottom_depth){
  n_slyr <- dim(soil_lyr_thicnkness)[3]
  n_mlyr <- dim(mod_lyr_bottom_depth)[3]
  soil_lyr_weights <- array(NA,dim = c(dim(mod_lyr_bottom_depth),n_slyr))
  nx <- 1:dim(mod_lyr_bottom_depth)[1]
  ny <- 1:dim(mod_lyr_bottom_depth)[2]
  xy <- expand.grid(nx,ny)

  for(x in 1:dim(mod_lyr_bottom_depth)[1]){
    for(y in 1:dim(mod_lyr_bottom_depth)[2]){
      idx_top <- 1
      z_top_model <- 0
      # all model layers must have a bottom depth. If not, ztot had no data value
      if(!all(is.na(mod_lyr_bottom_depth[x,y,]))){
        for(z in 1:n_mlyr){
          z_bottom_model <- mod_lyr_bottom_depth[x,y,z]
          h_mlyr <- z_bottom_model - z_top_model
          hsoil <- soil_lyr_thicnkness[x,y,]
          # till which soil layer index reaches the model layer
          idx_bottom <- which(z_bottom_model-cumsum(hsoil) <= 0)
          if(length(idx_bottom)==0){
            idx_bottom <- NA
          }
          else{
            idx_bottom <- idx_bottom[1]
          }
          # compute weights
          wghts <- rep(0, n_slyr)
          # model layer is in one soil layer
          if(idx_top == idx_bottom){
            # all soil layers has zero weight except the found index
            wghts[idx_top] <- 1
          }
          # model layer has multiple soil layers. compute weights for every soil layer in model layer
          else{
            # for bottom and top soil layer we should take a fraction of the soil layer depth
            for(slyr in idx_top:idx_bottom){
              if(slyr==idx_top){
                h_slyr <- sum(soil_lyr_thicnkness[x,y,1:idx_top]) - z_top_model
              }
              else if(slyr==idx_bottom){
                h_slyr <- z_bottom_model - sum(soil_lyr_thicnkness[x,y,1:(idx_bottom-1)])
                wghts[idx_bottom] <- h_slyr/h_mlyr
              }
              else{
                h_slyr <- soil_lyr_thicnkness[x,y,slyr]
              }

              wghts[slyr] <- h_slyr/h_mlyr
            }

          }

          # validate
          if(abs(1-sum(wghts)> 1e-8)){
            log_error(sprintf("Sum of soil layer weights should equal 1. x,y,z = %s, %s, %s",x,y,z))
            stop()
          }
          soil_lyr_weights[x,y,z,] <- wghts

          # go to next model lyr and set next top to current bottom
          idx_top <- idx_bottom
          z_top_model <- z_bottom_model
        }
      }
    }
  }

  return(soil_lyr_weights)
  #return(res)

}
