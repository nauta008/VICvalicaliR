
setMethod("get.breaks", signature("Verification"), function(x){

  breaks <- NULL
  if(toupper(x@method) =="KGE"){
    breaks <- list()
    breaks$kge <- c(-Inf,0,0.2,0.4,0.6,0.8,1)
    breaks$corr <- c(0,0.1,0.2,0.4,0.6,0.8,1)
    breaks$rat_sd <- NULL
    breaks$rat_mean <- c(0,0.1,0.5,0.75,0.9,1.1,1.25,1.5,2,Inf)
    breaks$rat_cv <- NULL

  }
  else if(toupper(x@method) =="NSE"){
    breaks <- list()
    breaks$NSE <- c(-Inf,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  }
  return(breaks)

})
