# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Frequency
#'
#' @slot value numeric.
#' @slot period character.
#'
setClass("Frequency", slots = list(value= "numeric",period="character"))

setClass("Dataset", slots = list(from="character",var="character", aggregation="list", filter="list" ,start="POSIXct", end="POSIXct"), prototype = list(
  aggregation=list(),
  filter=list()
))
setClass("Filter", slots = list(dataset="Dataset", value="numeric",operator="character"))
setClass("Aggregation", slots = list(type="character"))
setClass("TemporalAggregation",slots = list(by="character", fun="function") ,contains = "Aggregation", prototype = list(
  type="temporal",
  fun=mean
))
setClass("Timeseries", slots = list(dataset="Dataset"))
setClass("Verification", slots=list(method="character",state="character",start="POSIXct", end="POSIXct",dataset="Dataset"), prototype = list(
  state="CREATED"
))

#' Validation
#'
#' @slot start POSIXct.
#' @slot end POSIXct.
#'
setClass("Validation", slots = list(start="POSIXct", end="POSIXct", verifications="list",timeseries="list",netcdf="character",ts_plots="character"), prototype = list(
  # timeseries <- list(),
  # verifications <- list()
))

setClass("Params", slots = list(file="character"))


