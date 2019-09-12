#' Run TMDL Shiny App Browser
#' 
#' Launches TMDL Data Explorer shinyapp.
#' @export runTMDLApp
#' @import DT
#' @import shinyjs
#' @import shiny
#' @import openxlsx
#' @import colorspace
#' @importFrom yarrr piratepal
#' @importFrom reshape2 melt
#' @importFrom ggplot2 alpha



runTMDLApp <- function(){
  shiny::runApp(system.file('TMDLApp', package='tmdlTools'))
}