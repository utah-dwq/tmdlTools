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
                                     tags$strong("NOTE:"),p("Workbooks must fit the E.coli tmdlTools template, but you will have the option in this app to run the tmdlCalcs (e.g. calculate loadings, seasonal geomeans) function on the uploaded dataset."),
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
   wbdownload <- reactiveValuesToList(workbook)
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
     openxlsx::saveWorkbook(wbdwn$outputworkbook, file)
   }
 )
 
 
 observe({
   req(input$selectsheet)
   tableview = workbook[[input$selectsheet]]
   
   # Load data tables on first page
   output$datview <- renderDT(tableview, 
                              rownames = FALSE,
                              options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
   
 })

  output$inputdat <- renderDT(workbook$Inputs,
                             rownames = FALSE, 
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))

## Save Criteria in own reactive values for use in all plots ##
crits <- reactiveValues()

observe({
  req(workbook$Inputs)
  inputs <- workbook$Inputs
  crits$maxcrit = as.numeric(inputs$Value[inputs$Parameter == "Max Criterion"])
  crits$geomcrit = as.numeric(inputs$Value[inputs$Parameter == "Geometric Mean Criterion"])
})
  
### TIME SERIES SECTION ###
  
# Get time series max and min date range based on data upload
output$tsdatrange <- renderUI({
  req(workbook$Daily_Geomean_Data)
  timeseries <- workbook$Daily_Geomean_Data
  sliderInput("tsdatrange",
                 label="Date Range",
                 min=min(timeseries$Date),
                 max=max(timeseries$Date),
                 value = c(min(timeseries$Date),max(timeseries$Date)),
                 dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Reset date range on button click
# observeEvent(input$reset_input,{
#   reset("date")})

# Create checkbox menu based on sites present
output$checkbox <- renderUI({
  req(workbook$Daily_Geomean_Data)
  timeseries <- workbook$Daily_Geomean_Data
  choice <-  unique(timeseries$ML_Name)
  checkboxGroupInput("checkbox","Select Site(s)", choices = choice, selected = choice[1])
  
})

# Create timeseries data object based on data input, sites, and date ranges selected
timeseriesdat <- reactiveValues()
observe({
  req(input$checkbox)
  x = workbook$Daily_Geomean_Data
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
  
  # Get number of sites
  uni.sites <- unique(x$ML_Name)
  colrs <- yarrr::piratepal("basel")
  
  # Create an empty plot
  plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min, max), ylim=c(0, 2420))
  axis.Date(1, at=seq(min, max, by="6 months"), format="%m-%Y", las=2, cex=0.8)
  abline(h=crits$maxcrit,col="orange", lwd=2)
  abline(h=crits$geomcrit, col="red", lwd=2)
  text(min+150,crits$maxcrit-100, paste0("Max Crit - ",crits$maxcrit," MPN/100 mL"))
  text(min+400,crits$geomcrit-100, paste0("Geometric Mean Crit - ",crits$geomcrit," MPN/100 mL"))
  site = vector()
  colr = vector()
  # Start plotting
  for(i in 1:length(uni.sites)){
    y = x[x$ML_Name==uni.sites[i],]
    perc.exc = round(length(y$E.coli_Geomean[y$E.coli_Geomean>as.numeric(crits$maxcrit)])/length(y$E.coli_Geomean)*100, digits=0)
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
  req(workbook$Daily_Geomean_Data)
  monthsites <- workbook$Monthly_Data
  monthsites = unique(monthsites$ML_Name)
  selectInput("monthsite",
              label = "Site Name",
              choices=monthsites)
})

# Dates to choose from
output$mondatrange <- renderUI({
  req(workbook$Daily_Geomean_Data)
  monthdata <- workbook$Daily_Geomean_Data
  sliderInput("mondatrange",
              label="Date Range",
              min=min(monthdata$Date),
              max=max(monthdata$Date),
              value = c(min(monthdata$Date),max(monthdata$Date)),
              dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Reset Date Range
# observeEvent(input$reset_input1,{
#   reset("date1")})

# Craft drop down menu for concentration and loading
output$unit_type <- renderUI({
  req(workbook$Daily_Geomean_Data)
  monthdata <- workbook$Monthly_Data
  monthdata = monthdata[monthdata$ML_Name==input$monthsite&!is.na(monthdata$Observed_Loading),"Observed_Loading"]
  if(length(monthdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("unit_type","Select Measurement Type", choices = subd, selected = subd[1])
})

# Create dataset for use in plots and tables
selectedmonthdata <- reactiveValues()

observe({
  req(input$monthsite)
  req(input$mondatrange)
  dailygeomeans <- workbook$Daily_Geomean_Data
  seldailygeomeans <- dailygeomeans[dailygeomeans$ML_Name==input$monthsite&dailygeomeans$Date>input$mondatrange[1]&dailygeomeans$Date<input$mondatrange[2],]
  seldailygeomeans$month = lubridate::month(seldailygeomeans$Date, label=TRUE)
  selectedmonthdata$seldg = seldailygeomeans
  aggseldg <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=function(x){exp(mean(log(x)))})
  aggseldg$Percent_Reduction <- ifelse(aggseldg$E.coli_Geomean>crits$geomcrit,round(perc.red(crits$geomcrit,aggseldg$E.coli_Geomean), digits=0),0)
  selectedmonthdata$aggseldg = aggseldg

})

output$Monthly_Geomeans <- renderPlot({
  req(selectedmonthdata$aggseldg)
  barcolors = piratepal(palette="up")
  if(input$unit_type=="Concentration"){
    x = selectedmonthdata$aggseldg
    print(x)
    # Straight bar plot - concentrations
    uplim = max(x$E.coli_Geomean)*1.2
    mo_conc.p <- x$E.coli_Geomean
    barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$month,ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
    legend("topright",legend=c("Geomean Standard", "% Reduction Needed"), bty="n", fill=c("white","white"), border=c("white","white"),lty=c(1,NA),lwd=c(2,NA),cex=1)
    box(bty="l")
    abline(h=crits$geomcrit, col="black", lwd=2)
    barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction))
    barperc <- barperc[barperc$X3>0,]
    if(dim(barperc)[1]>0){
      barperc$X4 <- paste(barperc$X3,"%",sep="")
      text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1)
    }
    if(input$medplot){
      # Obtain boxplot stats from loading data
      y = selectedmonthdata$seldg
      y = droplevels(y[order(y$month),])
      
      # Get axes right to accommodate boxplot overlay (if checkbox checked)
      uplim1 = quantile(y$E.coli_Geomean,1)
      uplim1 = max(uplim, uplim1)
      
      # Bar plot
      barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans with Quartile Overlay", ylim=c(0, uplim1), names.arg = x$month, ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
      abline(h=crits$geomcrit, col="black", lty=2, lwd=2)
      legend("topright",legend=c("Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,1),fill=c(NA,NA,"white"),border=c("white","white","white"),lty=c(1,2,NA),lwd=c(3,2,NA),cex=1)
      box(bty="l")
      
      # x-axis arguments for boxplot based on barplot placement
      
      boxplot(y$E.coli_Geomean~y$month,
              lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(barcolors[1],0.1), boxwex = 0.7, at=barp[,1], add=TRUE)
    }
  }
  if(input$unit_type=="Loading"){
    cols = piratepal(palette="up")
    # Narrow dataset
    monthdatl <- workbook$LDC_Data
    datrange <- monthdatl[monthdatl$ML_Name==input$monthsite&monthdatl$Date>input$mondatrange[1]&monthdatl$Date<input$mondatrange[2],c("MLID","ML_Name","Date","Loading_Capacity_MOS","Observed_Loading")]
    if(dim(datrange)[1]>0){
      datrange <- datrange[!is.na(datrange$Observed_Loading),]
      datstack <- reshape2::melt(data = datrange, id.vars = c("MLID", "ML_Name", "Date"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
      datstack$month = lubridate::month(datstack$Date, label=TRUE)
      datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
      names(datstack)[names(datstack)=="value"]<-"Loading"
      x <- aggregate(Loading~month+MLID+ML_Name+Meas_Type, dat=datstack, FUN=function(x){exp(mean(log(x)))})
      x = dcast(data=x, month~Meas_Type, value.var="Loading")
      x = x[order(x$month),]
      x$Percent_Reduction = ifelse(x$Observed_Loading>x$Loading_Capacity_MOS,round(perc.red(x$Loading_Capacity_MOS,x$Observed_Loading), digits=0),0)
      uplim = max(c(x$Observed_Loading,x$Loading_Capacity_MOS))*1.2
      mo_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
      
      # Straight bar plots
      barp <- barplot(t(mo_load.p), beside=T, main = "Monthly E.coli Loading Geomeans",names.arg=x$month, ylim=c(0, uplim), ylab="E.coli Loading (MPN/day)",col=c(cols[1],cols[2]))
      legend("topright",legend=c("Observed Loading","Loading Capacity", "% Reduction Needed"), bty="n", fill=c(cols[1],cols[2],"white"), border=c("black","black","white"),cex=1)
      box(bty="l")
      barps <- barp[1,]
      barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
      barperc <- barperc[barperc$V3>0,]
      if(dim(barperc)[1]>0){
        barperc$V3 <- paste(barperc$V3,"%",sep="")
        text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
      }
      
      if(input$medplot){
        # Get axes right to accommodate boxplot overlay (if checkbox checked)
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)
        
        # Bar plot
        barp <- barplot(t(mo_load.p), beside=T, names.arg = x$month, main = "Monthly E.coli Loading Geomeans with Quartile Overlay", ylim=c(0, uplim1*1.1), ylab="E.coli Loading (MPN/day)",col=c(cols[1],cols[2]))
        legend("topright",legend=c("Observed Loading","Loading Capacity", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,1),fill=c(cols[1],cols[2],NA,"white"),border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,3,NA),cex=1)
        box(bty="l")
        
        # x-axis arguments for boxplot based on barplot placement
        ax <- c(barp[1,],barp[2,])
        ax_spots = ax[order(ax)]
        
        boxplot(datstack$Loading~datstack$Meas_Type+lubridate::month(datstack$Date),
                lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(cols[1],cols[2]),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
      }
    }
  }
})





}

# Run the application
shinyApp(ui = ui, server = server)

#### UNUSED CODE ####
# out$geom_crit = out$Inputs[out$Inputs$Parameter=="Geometric Mean Criterion","Value"]
# out$max_crit = out$Inputs[out$Inputs$Parameter=="Max Criterion","Value"]
# out$cf = out$Inputs[out$Inputs$Parameter=="Correction Factor","Value"]
# out$mos = out$Inputs[out$Inputs$Parameter=="Margin of Safety","Value"]
