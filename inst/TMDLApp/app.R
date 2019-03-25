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
   out$Daily_Geomean_Data$E.coli_Geomean = round(out$Daily_Geomean_Data$E.coli_Geomean,1)
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
### Time Series Tab ###
  
  observeEvent(input$tmdltool, {
      insertTab(inputId = "all_the_things",
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
                         div(DT::dataTableOutput("Time_Data"), style= "font-size:75%"))),target="User Guide")
  })


output$tsdatrange <- renderUI({
  timeseries <- isolate(workbook$dailyvalues)
  print(min(timeseries$Date))
  sliderInput("tsdatrange",
                 label="Date Range",
                 min=min(timeseries$Date),
                 max=max(timeseries$Date),
                 value = c(min(timeseries$Date),max(timeseries$Date)),
                 dragRange = TRUE, timeFormat="%Y-%m-%d")
})

observeEvent(input$reset_input,{
  reset("date")})

output$checkbox <- renderUI({
  timeseries <- isolate(workbook$dailyvalues)
  choice <-  unique(timeseries$ML_Name)
  checkboxGroupInput("checkbox","Select Site(s)", choices = choice, selected = choice[1])
  
})

timeseriesdat <- reactiveValues()

observe({
  req(input$checkbox)
  print(input$tsdatrange)
  x = isolate(workbook$dailyvalues)
  x = x[x$ML_Name %in% input$checkbox,]
  timeseriesdat$min = input$tsdatrange[1]
  timeseriesdat$max = input$tsdatrange[2]
  timeseriesdat$x <- x[x$Date>timeseriesdat$min&x$Date<timeseriesdat$max,]
})

output$Time_Series <- renderPlot({
  req(input$checkbox)
  req(input$tsdatrange[1],input$tsdatrange[2])
  x = timeseriesdat$x
  min = timeseriesdat$min
  max = timeseriesdat$max
  # Get number of sites
  uni.sites <- unique(x$ML_Name)
  colrs <- yarrr::piratepal("basel")
  
  # Create an empty plot
  plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min, max), ylim=c(0, 2420))
  axis.Date(1, at=seq(min, max, by="6 months"), format="%m-%Y", las=2, cex=0.8)
  abline(h=input$crit2,col="orange", lwd=2)
  abline(h=input$crit1, col="red", lwd=2)
  text(min+150,as.numeric(input$crit1)-100, "Max Crit")
  text(min+400,as.numeric(input$crit2)-100, "Geometric Mean Crit")
  site = vector()
  colr = vector()
  # Start plotting
  for(i in 1:length(uni.sites)){
    y = x[x$ML_Name==uni.sites[i],]
    perc.exc = round(length(y$E.coli_Geomean[y$E.coli_Geomean>as.numeric(input$crit1)])/length(y$E.coli_Geomean)*100, digits=0)
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

}
# Run the application
shinyApp(ui = ui, server = server)

#### UNUSED CODE ####
# out$geom_crit = out$Inputs[out$Inputs$Parameter=="Geometric Mean Criterion","Value"]
# out$max_crit = out$Inputs[out$Inputs$Parameter=="Max Criterion","Value"]
# out$cf = out$Inputs[out$Inputs$Parameter=="Correction Factor","Value"]
# out$mos = out$Inputs[out$Inputs$Parameter=="Margin of Safety","Value"]
