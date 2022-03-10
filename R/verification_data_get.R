
setMethod("data.get", signature("Verification"), function(x){
  return(.validation.data.get(x@dataset))
})
