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


setClass("Verification", slots=list(method="character", var="character",state="character",start="POSIXct", end="POSIXct"), prototype = list(
  state="CREATED"
))

#' Validation
#'
#' @slot start POSIXct.
#' @slot end POSIXct.
#'
setClass("Validation", slots = list(start="POSIXct", end="POSIXct", verifications="list",output="character"))




