
#' @name run
setGeneric("run", function(x) standardGeneric("run"))

#' @name start
setGeneric("start", function(x, ...) standardGeneric("start"))

setGeneric("filter", function(x, st_data ,...) standardGeneric("filter"))
setGeneric("aggregate", function(x,st_data) standardGeneric("aggregate"))

setGeneric("create", function(x) standardGeneric("create"))

setGeneric("data.get",function(x,...) standardGeneric("data.get"))

setGeneric("get.file", function(x,...) standardGeneric("get.file"))

setGeneric("create.map", function(x,st_data,...) standardGeneric("create.map"))
setGeneric("plot.data", function(x,base_map, st_data, ...) standardGeneric("plot.data"))

setGeneric("eval.model", function(x,params,...) standardGeneric("eval.model"))
