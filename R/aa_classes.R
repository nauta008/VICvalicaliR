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
#' @return
#'
#' @examples
setClass("Frequency", slots = list(value= "numeric",period="character"))
#setClass("ObservationSettings", slots = list(file="character", dis="character"))
#setClass("SimulationSettings", slots = list(file="character"))
#setClass("ValidationSettings", slots = list(start="character",end="character"))
#setClass("Settings", slots=list(mode="character",log="character", observations="ObservationSettings"))

#' Validation
#'
#' @slot start POSIXct.
#' @slot end POSIXct.
#'
#' @return
#'
#' @examples
setClass("Validation", slots = list(start="POSIXct", end="POSIXct"))




