#' Run TMDL Shiny App Browser
#' 
#' Function uses output from calcLoadings to plot time series, LDC, and monthly and seasonal geomeans in shiny browser.
#' @param wb_path A file path to the .xlsx file containing E.coli and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param specs Logical. If TRUE, uses geom_crit, max_crit, and mos from workbook. If FALSE, function requires inputs of geom_crit, max_crit, and mos.
#' @param geom_crit Numeric. The geometric mean criterion for the E.coli dataset, taken from R317-2-14.
#' @param max_crit Numeric. The maximum criterion for the E.coli dataset, taken from R317-2-14.
#' @param mos Numeric proportion. The percent margin of safety (as a proportion) to use when calculating percent exceedance/reduction needed.
#' @export runTMDLApp
#' @import DT
#' @import shinyjs
#' @import shiny
#' @import openxlsx
#' @importFrom yarrr piratepal

runTMDLApp <- function(wb_path, specs = TRUE, geom_crit = 126, max_crit = 406, mos = 0.1){
  wb_path <<- wb_path
  if(specs==FALSE){
    geom_crit <<- geom_crit
    max_crit <<- max_crit
    mos <<- mos
  }
  shiny::runApp(system.file('TMDLApp', package='tmdlTools'))
}