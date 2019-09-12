#' Run Ecoli Shiny App Browser
#' 
#' Launches E.coli Data Explorer shinyapp.
#' @export runEcoliApp
#' @import DT
#' @import shinyjs
#' @import shiny
#' @import openxlsx
#' @import colorspace
#' @importFrom yarrr piratepal
#' @importFrom reshape2 melt
#' @importFrom ggplot2 alpha



runEcoliApp <- function(){
  shiny::runApp(system.file('EcoliApp', package='tmdlTools'))
}