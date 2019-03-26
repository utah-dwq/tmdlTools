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
                                                  uiOutput("selectsheet"),
                                                  uiOutput("dwnloadbutton")),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Data View", DTOutput("datview"), style= "font-size:75%"),
                                         tabPanel("Check Inputs", DTOutput("inputdat"), style= "font-size:75%")
                                       )
                                     )),
                            tabPanel("Time Series",
                                     shinyjs::useShinyjs(),
                                     h3("Bacterial Concentrations Over Time by Site"),
                                     sidebarPanel(selectInput("plottype", label = "Select Plot Type", choices = c("Point","Line"), selected = "Point"),
                                                  div(id = "date",
                                                      uiOutput("tsdatrange")),
                                                  actionButton("reset_input","Reset Date Range"),
                                                  br(),
                                                  br(),
                                                  uiOutput("checkbox")),
                                     mainPanel(plotOutput("Time_Series"),
                                               hr(),
                                               br(),
                                               div(DT::dataTableOutput("Time_Data"), style= "font-size:75%"))),
                            tabPanel("Monthly",
                                     h3("Bacterial Concentrations/Loadings by Month"),
                                     sidebarPanel(uiOutput("monthsite"),
                                                  div(id="date1",uiOutput("mondatrange")),
                                                  actionButton("reset_input1","Reset Date Range"),
                                                  br(),
                                                  br(),
                                                  uiOutput("unit_type"),
                                                  checkboxInput("medplot", label = strong("View Medians and Quartiles"))),
                                     mainPanel(plotOutput("Monthly_Geomeans", height="700px"),
                                               DT::dataTableOutput("Monthly_Data", height=500))),
                            tabPanel("User Guide",
                                     includeMarkdown("user_guide.Rmd"))
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
   req(workbook$Inputs)
   selectInput("selectsheet", label = "Select sheet to view.", selected = NULL, choices=c("Raw Concentrations"="Ecoli_data","Flow"="Flow_data",
                                                                                          "Daily Geomean Concentrations" = "Daily_Geomean_Data", "Loadings"="LDC_Data",
                                                                                          "Monthly Geomeans" = "Monthly_Data", "Rec/Non-Rec Geomeans"= "Rec_Season_Data",
                                                                                          "Irrigation/Non-Irrigation Geomeans"="Irg_Season_Data"))
 })

# Download button that shows after sheet widget
output$dwnloadbutton <- renderUI({
  req(input$selectsheet)
  downloadButton("export_tmdltools","Export tmdlTools output")
})
 
### Reading in the data from the file to the reactive environment ### 
 observe({
   req(input$tmdltool)
   if(input$tmdltool=="Yes"){
     out <- tmdlTools::tmdlCalcs(workbook$wb_path, exportfromfunc = FALSE)
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
   workbook$Ecoli_data = out$Ecoli_data
   workbook$Inputs = out$Inputs
   if(!is.null(out$Daily_Geomean_Data)){
     out$Daily_Geomean_Data$E.coli_Geomean = round(out$Daily_Geomean_Data$E.coli_Geomean,1)
     workbook$Daily_Geomean_Data = out$Daily_Geomean_Data
     workbook$Monthly_Data = out$Monthly_Data
     workbook$Rec_Season_Data = out$Rec_Season_Data
     workbook$Irg_Season_Data = out$Irg_Season_Data
   }
   if(!is.null(out$Flow_data)){
     workbook$Flow_data = out$Flow_data
     if(!is.null(out$LDC_Data)){
       workbook$LDC_Data = out$LDC_Data
     }
   }
 })

 # Object to be fed to the download handler
 wbdwn <- reactiveValues()
 
 # Create workbook where each workbook reactive values object is added to the wbdwn reactive values object for use in download handler.
 observe({
   req(workbook$Inputs)
   wbdownload <- isolate(reactiveValuesToList(workbook))
   wbdownload$wb_path = NULL
   
   wb <- openxlsx::createWorkbook()
   
   for(i in 1:length(wbdownload)){
     addWorksheet(wb, names(wbdownload)[i], gridLines = TRUE)
     writeData(wb, sheet = names(wbdownload)[i], wbdownload[i], rowNames = FALSE)
   }
   wbdwn$outputworkbook = wb
 })
 
 # Download results of tmdlTools
 output$export_tmdltools <- downloadHandler(
   filename = paste0(unlist(strsplit(input$workbook$name,".xlsx")),"_",Sys.Date(),".xlsx"),
   content = function(file) {
     openxlsx::saveWorkbook(isolate(wbdwn$outputworkbook), file)
   }
 )
 
 
 observe({
   req(input$selectsheet)
   tableview = isolate(workbook[[input$selectsheet]])
   
   # Load data tables on first page
   output$datview <- renderDT(tableview, 
                              rownames = FALSE,
                              options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
   
 })

  output$inputdat <- renderDT(workbook$Inputs,
                             rownames = FALSE, 
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))

### TIME SERIES SECTION ###
  
# Get time series max and min date range based on data upload
output$tsdatrange <- renderUI({
  timeseries <- isolate(workbook$Daily_Geomean_Data)
  sliderInput("tsdatrange",
                 label="Date Range",
                 min=min(timeseries$Date),
                 max=max(timeseries$Date),
                 value = c(min(timeseries$Date),max(timeseries$Date)),
                 dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Reset date range on button click
observeEvent(input$reset_input,{
  reset("date")})

# Create checkbox menu based on sites present
output$checkbox <- renderUI({
  timeseries <- isolate(workbook$Daily_Geomean_Data)
  choice <-  unique(timeseries$ML_Name)
  checkboxGroupInput("checkbox","Select Site(s)", choices = choice, selected = choice[1])
  
})

# Create timeseries data object based on data input, sites, and date ranges selected
timeseriesdat <- reactiveValues()
observe({
  req(input$checkbox)
  x = isolate(workbook$Daily_Geomean_Data)
  x = x[x$ML_Name %in% input$checkbox,]
  timeseriesdat$min = input$tsdatrange[1]
  timeseriesdat$max = input$tsdatrange[2]
  timeseriesdat$x <- x[x$Date>timeseriesdat$min&x$Date<timeseriesdat$max,]
})

output$Time_Series <- renderPlot({
  req(input$checkbox)
  req(input$tsdatrange[1],input$tsdatrange[2])
  
  # Plot inputs from reactive values
  x = timeseriesdat$x
  min = timeseriesdat$min
  max = timeseriesdat$max
  crits = isolate(workbook$Inputs)
  maxcrit = crits$Value[crits$Parameter == "Max Criterion"]
  geomcrit = crits$Value[crits$Parameter == "Geometric Mean Criterion"]
  
  # Get number of sites
  uni.sites <- unique(x$ML_Name)
  colrs <- yarrr::piratepal("basel")
  
  # Create an empty plot
  plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min, max), ylim=c(0, 2420))
  axis.Date(1, at=seq(min, max, by="6 months"), format="%m-%Y", las=2, cex=0.8)
  abline(h=maxcrit,col="orange", lwd=2)
  abline(h=geomcrit, col="red", lwd=2)
  text(min+150,as.numeric(maxcrit)-100, paste0("Max Crit - ",maxcrit))
  text(min+400,as.numeric(geomcrit)-100, paste0("Geometric Mean Crit - ",geomcrit))
  site = vector()
  colr = vector()
  # Start plotting
  for(i in 1:length(uni.sites)){
    y = x[x$ML_Name==uni.sites[i],]
    perc.exc = round(length(y$E.coli_Geomean[y$E.coli_Geomean>as.numeric(maxcrit)])/length(y$E.coli_Geomean)*100, digits=0)
    if(input$plottype=="Line"){
      lines(y$E.coli_Geomean~y$Date, lwd=1, lty=1, col=colrs[i])
    }
    points(y$E.coli_Geomean~y$Date, pch=21, cex=2, col="black", bg=colrs[i])
    site[i] = paste0(as.character(uni.sites[i])," (",perc.exc,"% Exceed)")
    colr[i] = colrs[i]
  }
  l=legend("topleft",c(site),col="black",pt.bg=c(colrs), pch=21, bty="n", pt.cex=2,cex=1)
})



output$Time_Data <- renderDT(timeseriesdat$x,
                             rownames = FALSE,
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))


### MONTH TAB SECTION ###

# Sites to choose from
output$monthsite <- renderUI({
  monthsites <- isolate(workbook$Monthly_Data)
  monthsites = unique(monthsites$ML_Name)
  selectInput("site2",
              label = "Site Name",
              choices=monthsites)
})

# Dates to choose from
output$mondatrange <- renderUI({
  monthdata <- isolate(workbook$Daily_Geomean_Data)
  sliderInput("mondatrange",
              label="Date Range",
              min=min(monthdata$Date),
              max=max(monthdata$Date),
              value = c(min(monthdata$Date),max(monthdata$Date)),
              dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Reset Date Range
observeEvent(input$reset_input1,{
  reset("date1")})

# Craft drop down menu for concentration and loading
output$unit_type <- renderUI({
  monthdata <- isolate(workbook$Daily_Geomean_Data)
  monthdata = monthdata[monthdata$ML_Name==input$site2&!is.na(monthdata$Observed_Loading),"Observed_Loading"]
  if(length(monthdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("unit_type","Select Measurement Type", choices = subd, selected = subd[1])
})

}

# Run the application
shinyApp(ui = ui, server = server)

#### UNUSED CODE ####
# out$geom_crit = out$Inputs[out$Inputs$Parameter=="Geometric Mean Criterion","Value"]
# out$max_crit = out$Inputs[out$Inputs$Parameter=="Max Criterion","Value"]
# out$cf = out$Inputs[out$Inputs$Parameter=="Correction Factor","Value"]
# out$mos = out$Inputs[out$Inputs$Parameter=="Margin of Safety","Value"]
