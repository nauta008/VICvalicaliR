
setMethod("data.get", signature("Verification"), function(x){
  #return(.validation.data.get(x@dataset, sdt=x@start, edt=x@end))
  return(.validation.data.get(x@dataset, x@sim_file, x@obs_file))
})
