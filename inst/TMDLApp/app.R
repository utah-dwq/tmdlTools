################## SHINY APP ##########################################
require(openxlsx)
require(lubridate)
require(DT)
require(shinyjs)
require(yarrr)
require(colorspace)
require(reshape2)
require(markdown)

perc.red <- function(x,y){100-x/y*100}

# Define UI for application that draws a histogram
ui <- fluidPage(title="E.coli Data Explorer",
                titlePanel(title=div(img(width="8%",height="8%",src="dwq_logo_small.png"), em("Escherichia coli"),"Data Visualization Tool")),
                tabsetPanel(id="all_the_things",
                            tabPanel("Upload Data",
                                     useShinyjs(),
                                     h3("Select your Excel workbook containing E.coli data"),
                                     sidebarPanel(fileInput("workbook","Select Workbook"),
                                                  uiOutput("tmdltool"),
                                                  uiOutput("selectsheet")),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Data View", DTOutput("datview"), style= "font-size:75%"),
                                         tabPanel("Check Inputs", DTOutput("inputdat"), style= "font-size:75%")
                                       )
                                     )),
                            tabPanel("User Guide",
                                     includeMarkdown("C:\\Users\\ehinman\\Documents\\GitHub\\tmdlTools\\user_guide\\user_guide.Rmd"))
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
 
# Run tmdl tools widget, which disables once clicked   
 output$tmdltool <- renderUI({
    req(input$workbook)
    radioButtons("tmdltool", label = "Run tmdlTools?", selected = character(0), choices=c("Yes","No"), inline=TRUE)
  })
 
 observeEvent(input$tmdltool,{
   disable("tmdltool")
 })

# Create drop down menu of sheets contained in xlsx file

 output$selectsheet <- renderUI({
   req(workbook$inputs)
   selectInput("selectsheet", label = "Select sheet to view.", selected = NULL, choices=c("Raw Concentrations"="ecoli","Flow"="flow",
                                                                                          "Daily Geomean Concentrations" = "dailyvalues", "Loadings"="ldc",
                                                                                          "Monthly Geomeans" = "month", "Rec/Non-Rec Geomeans"= "rec",
                                                                                          "Irrigation/Non-Irrigation Geomeans"="irg"))
 })
 
### Reading in the data from the file to the reactive environment ### 
 observe({
   req(input$tmdltool)
   if(input$tmdltool=="Yes"){
     out <- tmdlTools::tmdlCalcs(workbook$wb_path, overwrite = FALSE)
   }else{
       dat = openxlsx::loadWorkbook(workbook$wb_path)
       sheets = dat$sheet_names[!dat$sheet_names=="READ ME"]
       out <- lapply(sheets, function(x)openxlsx::readWorkbook(workbook$wb_path, sheet = x, detectDates = TRUE))
       names(out) = sheets
   }
   convertds = out$Inputs$Value[out$Inputs$Parameter=="Rec Season Start"|out$Inputs$Parameter=="Rec Season End"|out$Inputs$Parameter=="Irrigation Season Start"|out$Inputs$Parameter=="Irrigation Season End"]
   convertds1 = as.Date(convertds, origin = "1899-12-30")
   convertds1 = format(convertds1, "%b %d")
   out$Inputs[out$Inputs$Value%in%convertds,"Value"] = convertds1
   workbook$ecoli = out$Ecoli_data
   workbook$dailyvalues = out$Daily_Geomean_Data
   workbook$month = out$Monthly_Data
   workbook$rec = out$Rec_Season_Data
   workbook$irg = out$Irg_Season_Data
   workbook$inputs = out$Inputs 
   
   if(!is.null(out$Flow_data)){
     workbook$flow = out$Flow_data
     workbook$ldc = out$LDC_Data
   }
 })
 
 observe({
   req(input$selectsheet)
   tableview = isolate(workbook[[input$selectsheet]])
   
   # Load data tables on first page
   output$datview <- renderDT(tableview, 
                              rownames = FALSE,
                              options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
   
 })

  output$inputdat <- renderDT(workbook$inputs,
                             rownames = FALSE, 
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
 
}

# Run the application
shinyApp(ui = ui, server = server)

#### UNUSED CODE ####
# out$geom_crit = out$Inputs[out$Inputs$Parameter=="Geometric Mean Criterion","Value"]
# out$max_crit = out$Inputs[out$Inputs$Parameter=="Max Criterion","Value"]
# out$cf = out$Inputs[out$Inputs$Parameter=="Correction Factor","Value"]
# out$mos = out$Inputs[out$Inputs$Parameter=="Margin of Safety","Value"]
