################## SHINY APP ##########################################
require(openxlsx)
require(lubridate)
require(DT)
require(shinyjs)
require(yarrr)
require(colorspace)
require(reshape2)
require(markdown)
# wb_path <- "C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\Fremont_data_2019-02-22.xlsx"
# wb.dat <- openxlsx::loadWorkbook(wb_path)
# ecoli.dat <- openxlsx::readWorkbook(wb.dat,sheet="Daily_Geomean_Data",startRow=1)
# ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
# if("Flow_data"%in%wb.dat$sheet_names){
#   flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
#   flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
# }
# if("LDC_Data"%in%wb.dat$sheet_names){
#   loading.dat <- openxlsx::readWorkbook(wb.dat, sheet="LDC_Data")
#   loading.dat$Date <- as.Date(loading.dat$Date, origin="1899-12-30")
# }else{loading.dat=NULL}
# month.dat <- openxlsx::readWorkbook(wb.dat,sheet="Monthly_Data",startRow=1)
# rec.dat <- openxlsx::readWorkbook(wb.dat,sheet="Rec_Season_Data",startRow=1)
# irg.dat <- openxlsx::readWorkbook(wb.dat,sheet="Irg_Season_Data",startRow=1)
# specs <- openxlsx::readWorkbook(wb.dat,sheet="Inputs",startRow=1)
# 
# max_crit = specs$Value[specs$Parameter=="Max Criterion"]
# geom_crit = specs$Value[specs$Parameter=="Geometric Mean Criterion"]
# cf = specs$Value[specs$Parameter=="Correction Factor"]
# mos = specs$Value[specs$Parameter=="Margin of Safety"]

perc.red <- function(x,y){100-x/y*100}

# Define UI for application that draws a histogram
ui <- fluidPage(title="E.coli Data Explorer",
                titlePanel(title=div(img(width="8%",height="8%",src="dwq_logo_small.png"), em("Escherichia coli"),"Data Visualization Tool")),
                tabsetPanel(id="all_the_things",
                            tabPanel("User Guide",
                                     includeMarkdown("C:\\Users\\ehinman\\Documents\\GitHub\\tmdlTools\\user_guide\\user_guide.Rmd")),
                            tabPanel("Upload Data",
                                     h3("Select your Excel workbook containing E.coli data"),
                                     sidebarPanel(fileInput("workbook","Select Workbook"),
                                                  uiOutput("tmdltool")),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Parameter Data", DTOutput("parameterdat"), style= "font-size:75%"),
                                         tabPanel("Flow Data", DTOutput("flowdat"), style= "font-size:75%")
                                       )
                                     )),
                            conditionalPanel(condition = "input.workbook")
              )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# File Info
  workbook <- reactiveValues()
# Obtain file path
  observeEvent(input$workbook,{
    fileup = input$workbook
    workbook$wb_path = fileup$datapath})
    
 output$tmdltool <- renderUI({
    req(input$workbook)
    radioButtons("tmdltool", label = "Run tmdlTools?", selected = character(0), choices=c("Yes","No"), inline=TRUE)
  })
 observe({
   if(input$tmdltool=="Yes"){
     out <- tmdlTools::tmdlCalcs(workbook$wb_path, overwrite = FALSE)
   }else{
       out = openxlsx::loadWorkbook(workbook$wb_path)
     }
 })
    # Run through TMDLcalcs
    
    exceeds <- ifelse(out$ecoli$E.coli_Geomean>out$max_crit, 1, 0)
    workbook$ecoli = out$ecoli
    workbook$exceeds = exceeds
    if(!is.null(out$flow)){
      workbook$flow = out$flow
      workbook$ldc = out$ldc 
    }
    # Add list items from out to reactive values object
    workbook$month = out$month
    workbook$rec = out$rec
    workbook$irg = out$irg
    workbook$max_crit = out$max_crit
    workbook$geom_crit = out$geom_crit
    workbook$cf = out$cf
    workbook$mos = out$mos
  })

# Load data tables on first page
  output$parameterdat <- renderDT(workbook$ecoli, 
                                  rownames = FALSE,
                                  options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
  output$flowdat <- renderDT(workbook$flow,
                             rownames = FALSE, 
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
  
  
  
 
}

# Run the application
shinyApp(ui = ui, server = server)
