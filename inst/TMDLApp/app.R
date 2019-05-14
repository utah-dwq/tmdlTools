################## SHINY APP ##########################################
require(openxlsx)
require(lubridate)
require(DT)
require(shinyjs)
require(yarrr)
require(colorspace)
require(reshape2)
require(markdown)
require(dplyr)
require(plyr)

source("tmdlCalcs.R")

perc.red <- function(x,y){100-x/y*100}

# Define UI for application that draws a histogram
ui <- fluidPage(title="E.coli Data Explorer",
                titlePanel(title=div(img(width="8%",height="8%",src="dwq_logo_small.png"), em("Escherichia coli"),"Data Visualization Tool")),
                tabsetPanel(id="all_the_things",
                            tabPanel("Upload Data",
                                     useShinyjs(),
                                     h3("Select your Excel workbook containing ", em("E.coli "), "data"),
                                     p(strong("NOTE: "),"Workbooks must fit the ", em("E.coli"), " template, but you have the option in this app to calculate loadings and seasonal geomeans on the uploaded dataset."),
                                     sidebarPanel(fileInput("workbook","Select Workbook"),
                                                  uiOutput("loadcalcs"),
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
                                     sidebarPanel(radioButtons("plottype", label = "Select Plot Type", choices = c("Point","Line"), selected = "Point", inline = TRUE),
                                                  div(id = "date",
                                                      uiOutput("tsdatrange")),
                                                  br(),
                                                  br(),
                                                  uiOutput("checkbox"),
                                                  br(),
                                                  uiOutput("checkbox1")),
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
                                                  uiOutput("mon_unit_type"),
                                                  checkboxInput("mon_medplot", label = strong("View Medians and Quartiles"))),
                                     mainPanel(plotOutput("Monthly_Geomeans", height="700px"),
                                               hr(),
                                               br(),
                                               div(DT::dataTableOutput("Monthly_Data"), style= "font-size:75%"))),
                            tabPanel("Rec/Non-Rec",
                                     h3("Bacterial Concentrations/Loadings by Year"),
                                     sidebarPanel(uiOutput("recsite"),
                                                  br(),
                                                  br(),
                                                  uiOutput("rec_unit_type"),
                                                  checkboxInput("rec_medplot", label = strong("View Medians and Quartiles"))),
                                     mainPanel(plotOutput("Rec_Geomeans", height="700px"),
                                               hr(),
                                               br(),
                                               div(DT::dataTableOutput("Rec_Data"), style= "font-size:75%"))),
                            tabPanel("Irrigation/Non-Irrigation",
                                     h3("Bacterial Concentrations/Loadings by Year"),
                                     sidebarPanel(uiOutput("irgsite"),
                                                  br(),
                                                  br(),
                                                  uiOutput("irg_unit_type"),
                                                  checkboxInput("irg_medplot", label = strong("View Medians and Quartiles"))),
                                     mainPanel(plotOutput("Irg_Geomeans", height="700px"),
                                               hr(),
                                               br(),
                                               div(DT::dataTableOutput("Irg_Data"), style= "font-size:75%"))),
                            tabPanel("Load Duration Curves",
                                     h3("Bacterial Loadings Across Flow Regimes"),
                                     p(strong("NOTE: "),em("E.coli "),"loadings are calculated using the ", strong("geometric mean criterion "),"based on the water body's beneficial use classification."),
                                     sidebarPanel(uiOutput("ldcsite"),
                                                  radioButtons("ldc_type", label = "Plot Type", choices = c("Scatterplot", "Boxplot"), selected = "Scatterplot", inline = TRUE),
                                                  selectInput("pt_type",
                                                              label = "Data Category",
                                                              choices=c("Calendar Seasons","Recreation Seasons","Irrigation Seasons")), width = 3),
                                     mainPanel(plotOutput("LDC", width="100%", height="700px"),
                                               hr(),
                                               div(DT::dataTableOutput("LDC_Data", height=20),style="font-size:75%"))),
                            tabPanel("User Guide",
                                     includeMarkdown("user_guide.Rmd"))
              )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
###################################### FILE UPLOAD SECTION ################################

# File Info
  workbook <- reactiveValues()
# Obtain file path
  observeEvent(input$workbook,{
    fileup = input$workbook
    workbook$wb_path = fileup$datapath})

# Run tmdl tools widget, which disables once clicked
 output$loadcalcs <- renderUI({
    req(input$workbook)
    radioButtons("loadcalcs", label = "Perform Geomean/Loading Calculations?", selected = character(0), choices=c("Yes","No"), inline=TRUE)
  })

 observeEvent(input$loadcalcs,{
   disable("loadcalcs")
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
  downloadButton("export_loadcalcs","Export workbook")
})

### Reading in the data from the file to the reactive environment ###
 observe({
   req(input$loadcalcs)
   if(input$loadcalcs=="Yes"){
     out <- tmdlCalcs(workbook$wb_path, exportfromfunc = FALSE)
   }else{
       dat = openxlsx::loadWorkbook(workbook$wb_path)
       sheets = dat$sheet_names[!dat$sheet_names=="READ ME"]
       out <- lapply(sheets, function(x)openxlsx::readWorkbook(workbook$wb_path, sheet = x, detectDates = TRUE))
       names(out) = sheets
   }
   # convertds = out$Inputs$Value[out$Inputs$Parameter=="Rec Season Start"|out$Inputs$Parameter=="Rec Season End"|out$Inputs$Parameter=="Irrigation Season Start"|out$Inputs$Parameter=="Irrigation Season End"]
   # convertds1 = as.Date(convertds, origin = "1899-12-30")
   # convertds1 = format(convertds1, "%b-%d")
   # out$Inputs[out$Inputs$Value%in%convertds,"Value"] = convertds1
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
       # rounding
       ldc = out$LDC_Data
       nums <- unlist(lapply(ldc, is.numeric))
       ldc[,nums] = round(ldc[,nums],1)
       workbook$LDC_Data = ldc
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
     writeData(wb, sheet = names(wbdownload)[i], wbdownload[[i]], rowNames = FALSE, colNames = TRUE)
   }
   wbdwn$outputworkbook = wb
 })

 # Download results of loadcalcs
 output$export_loadcalcs <- downloadHandler(
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


# Add loading tab if loadings present

  observe({
    if(!is.null(workbook$LDC_Data)){
      showTab(inputId="all_the_things", target = "Load Duration Curves")
    }else{
      hideTab(inputId="all_the_things", target = "Load Duration Curves")
    }
  })
  
# Add unique site names to LDC drop down list (if flow data are available)
output$ldcsite <- renderUI({
  req(workbook$LDC_Data)
  selectInput("ldcsite",
              label = "Site Name",
              choices=c(unique(workbook$LDC_Data$ML_Name)))
})

## Save Criteria in own reactive values for use in all plots ##
crits <- reactiveValues()

observe({
  req(workbook$Inputs)
  inputs <- workbook$Inputs
  crits$maxcrit = as.numeric(inputs$Value[inputs$Parameter == "Max Criterion"])
  crits$geomcrit = as.numeric(inputs$Value[inputs$Parameter == "Geometric Mean Criterion"])
})

################################# TIME SERIES SECTION ########################################

# Get time series max and min date range based on data upload
output$tsdatrange <- renderUI({
  req(workbook$Daily_Geomean_Data)
  if(!is.null(workbook$Flow_data)){
    time1 = workbook$Daily_Geomean_Data$Date
    time2 = workbook$Flow_data$Date
    timeseries = unique(c(time1,time2))
  }else{
    timeseries <- workbook$Daily_Geomean_Data$Date
  }

  sliderInput("tsdatrange",
                 label="Date Range",
                 min=min(timeseries),
                 max=max(timeseries),
                 value = c(min(timeseries),max(timeseries)),
                 dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Reset date range on button click
# observeEvent(input$reset_input,{
#   reset("date")})

# Create checkbox menu for e.coli concentrations based on sites present
output$checkbox <- renderUI({
  req(workbook$Daily_Geomean_Data)
  timeseries <- workbook$Daily_Geomean_Data
  choice <-  unique(timeseries$ML_Name)
  checkboxGroupInput("checkbox","Select E.coli Site(s)", choices = choice, selected = choice[1])

})

# Create checkbox menu for flow based on tabs and sites present
output$checkbox1 <- renderUI({
  req(workbook$Flow_data)
  flow <- workbook$Flow_data
  choice <-  unique(flow$ML_Name)
  checkboxGroupInput("checkbox1","Select Flow Site(s)", choices = choice, selected = character(0))

})

# Create timeseries data object based on data input, sites, and date ranges selected
timeseriesdat <- reactiveValues()
observe({
  req(input$checkbox)
  x = workbook$Daily_Geomean_Data
  x = x[x$ML_Name %in% input$checkbox,]
  if(!is.null(input$tsdatrange)){
    timeseriesdat$min = input$tsdatrange[1]
    timeseriesdat$max = input$tsdatrange[2] 
  }
  timeseriesdat$x <- x[x$Date>timeseriesdat$min&x$Date<timeseriesdat$max,]
  
})

observe({
  req(input$checkbox1)
  x1 = workbook$Flow_data
  x1 = x1[x1$ML_Name %in% input$checkbox1,]
  timeseriesdat$x1 <- x1[x1$Date>timeseriesdat$min&x1$Date<timeseriesdat$max,]
})

output$Time_Series <- renderPlot({
  req(timeseriesdat$min,timeseriesdat$max)
if(!is.null(input$checkbox)|!is.null(input$checkbox1)){
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
  text(min+200,crits$maxcrit-100, paste0("Max Crit - ",crits$maxcrit," MPN/100 mL"))
  text(min+500,crits$geomcrit-100, paste0("Geometric Mean Crit - ",crits$geomcrit," MPN/100 mL"))
  site = vector()
  colr = vector()
 
  if(!is.null(input$checkbox)){
    # Start plotting ecoli concentrations
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
  }
  
  # Start plotting flow
  if(!is.null(input$checkbox1)){
    x1 = timeseriesdat$x1
    uni.sites.1 = unique(x1$ML_Name)
    colrs1 <- yarrr::piratepal("basel", trans = 0.5)
    
    site1 = vector()
    colr1 = vector()
    par(new = TRUE)
    plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(min, max), ylim = c(0,max(x1$Flow)))
    axis(side = 4)
    mtext(side = 4, line = 3, "Flow (cfs)")
    for(i in 1:length(uni.sites.1)){
      y1 = x1[x1$ML_Name==uni.sites.1[i],]
      if(input$plottype=="Line"){
        lines(y1$Flow~y1$Date, lwd=1, lty=1, col=colrs1[i])
      }
      points(y1$Flow~y1$Date, pch=23, cex=2, col="black", bg=colrs1[i])
      site1[i] = uni.sites.1[i]
      colr1[i] = colrs1[i]
    }
    l=legend("topright",c(site1),col="black",pt.bg=c(colrs1), pch=23, bty="n", pt.cex=2,cex=1)
  }
}
})



output$Time_Data <- renderDT(timeseriesdat$x,
                             rownames = FALSE,
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))


###################################### MONTH TAB SECTION #####################################

# Sites to choose from
output$monthsite <- renderUI({
  req(workbook$Daily_Geomean_Data)
  #monthsites <- workbook$Monthly_Data
  monthsites = unique(workbook$Monthly_Data$ML_Name)
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
output$mon_unit_type <- renderUI({
  req(workbook$Daily_Geomean_Data)
  monthdata <- workbook$Monthly_Data
  monthdata = monthdata[monthdata$ML_Name==input$monthsite&!is.na(monthdata$Observed_Loading),"Observed_Loading"]
  if(length(monthdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("mon_unit_type","Select Measurement Type", choices = subd, selected = subd[1])
})

# Create dataset for use in plots and tables
selectedmonthdata <- reactiveValues()

observe({
  req(input$monthsite)
  req(input$mondatrange)
  dailygeomeans <- workbook$Daily_Geomean_Data
  seldailygeomeans <- dailygeomeans[dailygeomeans$ML_Name==input$monthsite&dailygeomeans$Date>=input$mondatrange[1]&dailygeomeans$Date<=input$mondatrange[2],]
  seldailygeomeans$month = lubridate::month(seldailygeomeans$Date, label=TRUE)
  selectedmonthdata$seldg = seldailygeomeans
  aggseldg0 <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=function(x){exp(mean(log(x)))})
  aggseldg1 <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=length)
  names(aggseldg1)[names(aggseldg1)=="E.coli_Geomean"] <- "Ncount"
  aggseldg = merge(aggseldg0, aggseldg1, all = TRUE)
  aggseldg$Percent_Reduction <- ifelse(aggseldg$E.coli_Geomean>crits$geomcrit,round(perc.red(crits$geomcrit,aggseldg$E.coli_Geomean), digits=0),0)
  selectedmonthdata$aggseldg = aggseldg

})

output$Monthly_Geomeans <- renderPlot({
  req(selectedmonthdata$aggseldg)
  barcolors = piratepal(palette="up")
  if(input$mon_unit_type=="Concentration"){
    x = selectedmonthdata$aggseldg
    x = droplevels(x[order(x$month),])
    # Straight bar plot - concentrations
    uplim = max(x$E.coli_Geomean)*1.2
    mo_conc.p <- x$E.coli_Geomean
    barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$month,ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
    legend("topright",legend=c("Geomean Standard", "% Reduction Needed"), bty="n", fill=c("white","white"), border=c("white","white"),lty=c(1,NA),lwd=c(2,NA),cex=1)
    box(bty="l")
    abline(h=crits$geomcrit, col="black", lwd=2)
    ncount = paste0("n=",x$Ncount)
    mtext(ncount, side = 1, line = 0, cex=0.8, at = barp)
    barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction))
    barperc <- barperc[barperc$X3>0,]
    if(dim(barperc)[1]>0){
      barperc$X4 <- paste(barperc$X3,"%",sep="")
      text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1)
    }
    if(input$mon_medplot){
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
      mtext(ncount, side = 1, line = 0, cex = 0.8, at = barp)

      # x-axis arguments for boxplot based on barplot placement

      boxplot(y$E.coli_Geomean~y$month,
              lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(barcolors[1],0.1), boxwex = 0.7, at=barp[,1], add=TRUE)
    }
  }
  if(input$mon_unit_type=="Loading"){
    cols = piratepal(palette="up")
    # Narrow dataset
    monthdatl <- workbook$LDC_Data
    datrange <- monthdatl[monthdatl$ML_Name==input$monthsite&monthdatl$Date>=input$mondatrange[1]&monthdatl$Date<=input$mondatrange[2],c("MLID","ML_Name","Date","TMDL","Observed_Loading")]
    if(dim(datrange)[1]>0){
      datrange <- datrange[!is.na(datrange$Observed_Loading),]
      datstack <- reshape2::melt(data = datrange, id.vars = c("MLID", "ML_Name", "Date"), value.vars=c("TMDL","Observed_Loading"), variable.name = "Meas_Type")
      datstack$month = lubridate::month(datstack$Date, label=TRUE)
      datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
      names(datstack)[names(datstack)=="value"]<-"Loading"
      x <- aggregate(Loading~month+MLID+ML_Name+Meas_Type, dat=datstack, FUN=function(x){exp(mean(log(x)))})
      x_ncount <- aggregate(Loading~month+MLID+ML_Name+Meas_Type, dat=datstack, FUN=length)
      x_ncount <- x_ncount[x_ncount$Meas_Type=="Observed_Loading",]
      names(x_ncount)[names(x_ncount)=="Loading"]<- "Ncount"
      x_ncount = x_ncount[order(x_ncount$month),]
      x = dcast(data=x, month~Meas_Type, value.var="Loading")
      x = x[order(x$month),]
      x$Percent_Reduction = ifelse(x$Observed_Loading>x$TMDL,round(perc.red(x$TMDL,x$Observed_Loading), digits=0),0)
      uplim = max(c(x$Observed_Loading,x$TMDL))*1.2
      mo_load.p <- x[,names(x)%in%c("Observed_Loading","TMDL")]

      # Straight bar plots
      barp <- barplot(t(mo_load.p), beside=T, main = "Monthly E.coli Loading Geomeans",names.arg=x$month, ylim=c(0, uplim), ylab="E.coli Loading (GigaMPN/day)",col=c(cols[1],cols[2]))
      legend("topright",legend=c("Observed Loading","TMDL", "% Reduction Needed"), bty="n", fill=c(cols[1],cols[2],"white"), border=c("black","black","white"),cex=1)
      box(bty="l")
      barps <- barp[1,]
      barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
      barperc <- barperc[barperc$V3>0,]
      if(dim(barperc)[1]>0){
        barperc$V3 <- paste(barperc$V3,"%",sep="")
        text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
      }
      
      # Add ncounts
      ncountpos = colMeans(barp)
      mtext(c(paste0("n=",x_ncount$Ncount)),side = 1,line = 0,at = ncountpos, cex=0.8)

      if(input$mon_medplot){
        # Get axes right to accommodate boxplot overlay (if checkbox checked)
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)

        # Bar plot
        barp <- barplot(t(mo_load.p), beside=T, names.arg = x$month, main = "Monthly E.coli Loading Geomeans with Quartile Overlay", ylim=c(0, uplim1*1.1), ylab="E.coli Loading (GigaMPN/day)",col=c(cols[1],cols[2]))
        legend("topright",legend=c("Observed Loading","TMDL", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,1),fill=c(cols[1],cols[2],NA,"white"),border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,3,NA),cex=1)
        box(bty="l")
        mtext(c(paste0("n=",x_ncount$Ncount)),side = 1,line = 0,at = ncountpos, cex=0.8)
        # x-axis arguments for boxplot based on barplot placement
        ax <- c(barp[1,],barp[2,])
        ax_spots = ax[order(ax)]

        boxplot(datstack$Loading~datstack$Meas_Type+lubridate::month(datstack$Date),
                lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(cols[1],cols[2]),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
      }
    }
  }
})

# Observer to identify which data to show....
observe({
  req(selectedmonthdata$aggseldg)
  if(input$mon_unit_type=="Concentration"){
    selectedmonthdata$table = selectedmonthdata$aggseldg
  }else{
    data <- workbook$LDC_Data
    data1 <- data[data$ML_Name==input$monthsite&data$Date>=input$mondatrange[1]&data$Date<=input$mondatrange[2],]
    selectedmonthdata$table = data1
  }
})

# Data table
output$Monthly_Data <- renderDT(selectedmonthdata$table,
                             rownames = FALSE,
                             options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))


################################## REC TAB SECTION ##########################################

# Sites to choose from
output$recsite <- renderUI({
  req(workbook$Rec_Season_Data)
  recsites <- unique(workbook$Rec_Season_Data$ML_Name)
  selectInput("recsite",
              label = "Site Name",
              choices=recsites)
})

# Craft drop down menu for concentration and loading
output$rec_unit_type <- renderUI({
  req(workbook$Rec_Season_Data)
  recdata <- workbook$Rec_Season_Data
  recdata = recdata[recdata$ML_Name==input$recsite&!is.na(recdata$Observed_Loading),"Observed_Loading"]
  if(length(recdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("rec_unit_type","Select Measurement Type", choices = subd, selected = subd[1])
})

# Plotting
output$Rec_Geomeans <- renderPlot({
  # Require site name input before drawing plot
  req(input$rec_unit_type)
  # Data
  recdata = workbook$Rec_Season_Data
  ecolidata = workbook$Daily_Geomean_Data
  # Color schemes (unless otherwise defined)
  legcols = colorspace::rainbow_hcl(2) # legend colors--never redefined.
  colucols = colorspace::rainbow_hcl(2) # bars--redefined if only one category exists
  ### CONCENTRATION PLOTS ###
  if(input$rec_unit_type=="Concentration"){
    # Obtain data from rec.dat
    x <- recdata[recdata$ML_Name==input$recsite,]
    x$Rec_Season[x$Rec_Season=="Not Rec Season"] = paste0("z","NotRec") # ensures barplots are ordered rec season-->not rec season (alphabetical issue)
    x = x[order(x$Year),]
    # Set upper y limit for plot based on max geomean
    uplim = max(x$E.coli_Geomean)*1.2
    # Create columns for "Rec Season" and "Not Rec Season" to separate into different bar plots.
    recstack <- reshape2::dcast(data = x, Year~Rec_Season,value.var = "E.coli_Geomean")
    recstack1 = recstack[,!names(recstack)%in%"Year"]
    # Get the ncount set up
    recstack_ncount <- reshape2::dcast(data = x, Year~Rec_Season,value.var = "Ncount_rec_C")
    recstack_ncount_flat <- reshape2::melt(recstack_ncount, id="Year", value.name = "Count")
    recstack_ncount_flat = recstack_ncount_flat[order(recstack_ncount_flat$Year),]
    # Check to see if both rec and non rec seasons represented
    present = c("Rec Season", "zNotRec")%in%colnames(recstack)
    if(any(present==FALSE)){ # if rec or non rec are missing...
      buddies=FALSE # beside becomes false
      colucols = ifelse("Rec Season"%in%colnames(recstack),colucols[1],colucols[2]) # restrict colors to the one corresponding to the present category
    }else{
      buddies=TRUE # beside becomes true
    }
    # The actual plot...
    rec_conc <- barplot(t(recstack1), beside=buddies, names.arg = recstack$Year, main="E.coli Geomeans by Year", ylim=c(0, uplim*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
    legend("topright",legend=c("Rec Season", "Not Rec Season","Geomean Standard","% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2], NA,NA), border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,2,NA),cex=1)
    box(bty="l")
    abline(h=crits$geomcrit, col="black", lwd=2)
    
    # Positioning the ncounts 
    here = c(rec_conc[1,], rec_conc[2,])
    here = here[order(here)]
    ncount_text = cbind.data.frame(recstack_ncount_flat, here)
    ncount_text = ncount_text[!is.na(ncount_text$Count),]
    mtext(paste0("n=",ncount_text$Count), side = 1, line = 0, at = c(ncount_text$here), cex= 0.8)
    
    # Label bars that exceed
    # Get percent reductions
    perc_lab <- reshape2::dcast(data = x, Year~Rec_Season,value.var = "Percent_Reduction_C")
    perc_labs = melt(perc_lab, id.vars = c("Year"), value.vars = c("zNotRec", "Rec Season"))
    perc_labs = perc_labs[order(perc_labs$Year),]

    # Get height of bars
    perc_y = melt(recstack, id.vars = c("Year"), value.vars = c("zNotRec", "Rec Season"), value.name = "value1")
    perc_y = perc_y[order(perc_y$Year),]

    # Merge percent reductions and height at which they should be reported.
    percs <- merge(perc_labs,perc_y, all=TRUE)
    percs = percs[order(percs$Year),]

    # Get x pos of bars
    if(any(present==FALSE)){ # if rec or non rec are missing, that means that rec_conc only has one set of axis positions
      perc_at = rec_conc
    }else{
      perc_at = c(rec_conc[1,],rec_conc[2,]) # if both present, there are two rows of numbers
      perc_at = perc_at[order(perc_at)]
    }
    recperc <- data.frame(perc_at,percs) # meld data with x axis position
    recperc1 <- recperc[recperc$value>0&!is.na(recperc$value),]

    # Plot text of percent reduction onto plot
    if(dim(recperc1)[1]>0){
      recperc1$percn <- paste(recperc1$value,"%",sep="")
      text(recperc1$perc_at,recperc1$value1+0.1*mean(recperc1$value1),labels=recperc1$percn,cex=1)
    }
    ## CONC BOXPLOTS ##
    if(input$rec_medplot){
      # Obtain boxplot stats from geomean data
      y <- ecolidata[ecolidata$ML_Name==input$recsite,c("MLID","Date","ML_Name","Rec_Season","E.coli_Geomean")]
      y$Year = lubridate::year(y$Date)
      y$Rec_Season[y$Rec_Season=="Not Rec Season"] = paste0("z","NotRec")

      # Update upper y axis limit if outliers are beyond max barplot height (usually are)
      uplim1 = quantile(y$E.coli_Geomean,1)
      uplim1 = max(uplim, uplim1)

      # Create original bar plot with different legend
      barplot(t(recstack1), beside=buddies, names.arg = recstack$Year, main="E.coli Geomeans by Year", ylim=c(0, uplim1*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
      abline(h=crits$geomcrit, col="black", lty=2, lwd=2)
      legend("topright",legend=c("Rec Season","Not Rec Season","Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(legcols[1],legcols[2],NA,NA,"white"),border=c("black","black","white","white","white"),lty=c(NA,NA,1,2,NA),lwd=c(NA,NA,3,2,NA),cex=1)
      box(bty="l")
      mtext(paste0("n=",ncount_text$Count), side = 1, line = 0, at = c(ncount_text$here), cex= 0.8)
      
      # x-axis arguments for boxplot based on barplot placement
      boxat = recperc[,"perc_at"]
      boxplot(y$E.coli_Geomean~y$Rec_Season+y$Year,
              lty=1, xaxt="n",yaxt="n", frame=FALSE, col=ggplot2::alpha(colucols,0.1), boxwex = 0.7, at=boxat, add=TRUE)
    }
  }
  ### LOADING PLOTS ###
  if(input$rec_unit_type=="Loading"){
    # Define TMDL color
    loadcol = colorspace::rainbow_hcl(3)[3]
    # Filter to data needed to produce plots
    x <- recdata[recdata$ML_Name==input$recsite,]
    x = x[complete.cases(x),]
    x = x[order(x$Year),]
    if(dim(x)[1]>0){
      # Determine if rec/non rec represented
      uni = unique(x$Rec_Season)
      uplim = max(c(x$Observed_Loading,x$TMDL))*1.2
      if(length(uni)>1){ # if both represented...
        par(mfrow=c(1,2)) # create two plot panes...
        # Separate into rec and non rec
        # Rec data
        rec_load.p <- x[x$Rec_Season=="Rec Season",names(x)%in%c("Observed_Loading","TMDL","Year", "Ncount_rec_L")]
        rownames(rec_load.p)= rec_load.p$Year
        recload_ncount = rec_load.p$Ncount_rec_L
        rec_load.p = rec_load.p[,!names(rec_load.p)%in%c("Year", "Ncount_rec_L")]
        # Rec barplot
        barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim), main="Rec Season",ylab="E.coli Loading (GigaMPN/day)",col=c(colucols[1],loadcol))
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Rec Season"]))
        barperc <- barperc[barperc$V3>0,]
        # Plot percent reduction needed text, if applicable.
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
        }
        ncount_here = colMeans(barp)
        mtext(paste0("n=",recload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
        
        
        # Non-Rec data, same process
        nrec_load.p <- x[x$Rec_Season=="Not Rec Season",names(x)%in%c("Observed_Loading","TMDL","Year", "Ncount_rec_L")]
        rownames(nrec_load.p)= nrec_load.p$Year
        nrecload_ncount = nrec_load.p$Ncount_rec_L
        nrec_load.p = nrec_load.p[,!names(nrec_load.p)%in%c("Year", "Ncount_rec_L")]
        barp <- barplot(t(nrec_load.p), beside=T, names.arg=x$Year[x$Rec_Season=="Not Rec Season"], ylim=c(0, uplim), main="Not Rec Season",col=c(colucols[2],loadcol))
        legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","TMDL", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
        box(bty="l")
        ncount_here1 = colMeans(barp)
        mtext(paste0("n=",nrecload_ncount), side = 1, line = 0, at = c(ncount_here1), cex= 0.8)
        
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Not Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Not Rec Season"]))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
        }}else{ # If only one category represented...
          par(mfrow=c(1,1)) # only make one plot
          colucol = ifelse(uni=="Rec Season",colucols[1],colucols[2]) # redefine bar color
          rec_load.p <- x[,names(x)%in%c("Observed_Loading","TMDL","Year","Ncount_rec_L")]
          rownames(rec_load.p)= rec_load.p$Year
          recload_ncount = rec_load.p$Ncount_rec_L
          rec_load.p = rec_load.p[,!names(rec_load.p)%in%c("Year","Ncount_rec_L")]
          # Bar plot singular - same as rec
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim), main=uni,ylab="E.coli Loading (GigaMPN/day)",col=c(colucol,loadcol))
          legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","TMDL", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
          box(bty="l")
          
          #add ncount
          ncount_here = colMeans(barp)
          mtext(paste0("n=",recload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          
          barps <- barp[1,]
          barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction_L))
          barperc <- barperc[barperc$V3>0,]
          if(dim(barperc)[1]>0){
            barperc$V3 <- paste(barperc$V3,"%",sep="")
            text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
          }
        }
      ## LOADING BOXPLOTS ##
      if(input$rec_medplot){
        # Obtain boxplot stats from loading data
        loaddata = workbook$LDC_Data
        y <- loaddata[loaddata$ML_Name==input$recsite,c("MLID","ML_Name","Date","Rec_Season","TMDL","Observed_Loading")]
        y <- y[!is.na(y$Observed_Loading),]
        datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date","Rec_Season"), value.vars=c("TMDL","Observed_Loading"), variable.name = "Meas_Type")
        names(datstack)[names(datstack)=="value"]<-"Loading"
        datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])

        # Update upper y axis limit of outliers are beyond max barplot height
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)

        # Create OG barplot with new legend.
        if(length(uni)>1){# determine whether one or two plot panels needed...
          par(mfrow=c(1,2))
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Rec Season"], main="Rec Season",ylab="E.coli Loading (GigaMPN/day)",col=c(colucols[1],loadcol))
          box(bty="l")
          
          #Add ncount
          mtext(paste0("n=",recload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading[datstack$Rec_Season=="Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Rec Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[1], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)


          barp <- barplot(t(nrec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Not Rec Season"], main="Not Rec Season",col=c(colucols[2],loadcol))
          legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","TMDL", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(colucols[1],colucols[2],loadcol,NA,"white"),border=c("black","black","black","white","white"),lty=c(NA,NA,NA,1,NA),lwd=c(NA,NA,NA,3,NA),cex=1)
          box(bty="l")
          
          # Add ncount
          mtext(paste0("n=",nrecload_ncount), side = 1, line = 0, at = c(ncount_here1), cex= 0.8)
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading[datstack$Rec_Season=="Not Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Not Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Not Rec Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[2], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }else{
          par(mfrow=c(1,1))
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year, main=uni,ylab="E.coli Loading (GigaMPN/day)",col=c(colucol,loadcol))
          box(bty="l")
          
          # Add ncount
          mtext(paste0("n=",recload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading~datstack$Meas_Type+lubridate::year(datstack$Date),
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucol, loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }

      }
    }


  }

})

observe({
  req(input$rec_unit_type)
  rec_data = workbook$Rec_Season_Data
  recdat <- rec_data[rec_data$ML_Name==input$recsite,]
  recdat = recdat[order(recdat$Year),]
  # Data table
  output$Rec_Data <- renderDT(recdat,
                              rownames = FALSE,
                              options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
  
})

############################### IRRIGATION SEASON TAB #######################################
# Sites to choose from
output$irgsite <- renderUI({
  req(workbook$Irg_Season_Data)
  irgsites <- unique(workbook$Irg_Season_Data$ML_Name)
  selectInput("irgsite",
              label = "Site Name",
              choices=irgsites)
})

# Craft drop down menu for concentration and loading
output$irg_unit_type <- renderUI({
  req(workbook$Irg_Season_Data)
  irgdata <- workbook$Irg_Season_Data
  irgdata = irgdata[irgdata$ML_Name==input$irgsite&!is.na(irgdata$Observed_Loading),"Observed_Loading"]
  if(length(irgdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("irg_unit_type","Select Measurement Type", choices = subd, selected = subd[1])
})

# Plotting
output$Irg_Geomeans <- renderPlot({
  # Require site name input before drawing plot
  req(input$irg_unit_type)
  # Data
  irgdata = workbook$Irg_Season_Data
  ecolidata = workbook$Daily_Geomean_Data
  # Color schemes (unless otherwise defined)
  legcols = colorspace::terrain_hcl(2)
  colucols = colorspace::terrain_hcl(2)
  
  ### CONCENTRATION PLOTS ###
  if(input$irg_unit_type=="Concentration"){
    # Obtain data from irg.dat
    x <- irgdata[irgdata$ML_Name==input$irgsite,]
    x = x[order(x$Year),]
    # Set upper y limit for plot based on max geomean
    uplim = max(x$E.coli_Geomean)*1.2
    # Create columns for "Irg Season" and "Not Irg Season" to separate into different bar plots.
    irgstack <- reshape2::dcast(data = x, Year~Irg_Season,value.var = "E.coli_Geomean")
    irgstack1 = irgstack[,!names(irgstack)%in%"Year"]
    # Get the ncount set up
    irgstack_ncount <- reshape2::dcast(data = x, Year~Irg_Season,value.var = "Ncount_irg_C")
    irgstack_ncount_flat <- reshape2::melt(irgstack_ncount, id="Year", value.name = "Count")
    irgstack_ncount_flat = irgstack_ncount_flat[order(irgstack_ncount_flat$Year),]
    # Check to see if both irg and non irg seasons represented
    present = c("Irrigation Season", "Not Irrigation Season")%in%colnames(irgstack)
    if(any(present==FALSE)){ # if irg or non irg are missing...
      buddies=FALSE # beside becomes false
      colucols = ifelse("Irrigation Season"%in%colnames(irgstack),colucols[1],colucols[2]) # restrict colors to the one corresponding to the present category
    }else{
      buddies=TRUE # beside becomes true
    }
    # The actual plot...
    irg_conc <- barplot(t(irgstack1), beside=buddies, names.arg = irgstack$Year, main="E.coli Geomeans by Year", ylim=c(0, uplim*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
    legend("topright",legend=c("Irrigation Season", "Not Irrigation Season","Geomean Standard","% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2], NA,NA), border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,2,NA),cex=1)
    box(bty="l")
    abline(h=crits$geomcrit, col="black", lwd=2)
    
    # Positioning the ncounts 
    here = c(irg_conc[1,], irg_conc[2,])
    here = here[order(here)]
    ncount_text = cbind.data.frame(irgstack_ncount_flat, here)
    ncount_text = ncount_text[!is.na(ncount_text$Count),]
    mtext(paste0("n=",ncount_text$Count), side = 1, line = 0, at = c(ncount_text$here), cex= 0.8)
    
    # Label bars that exceed
    # Get percent reductions
    perc_lab <- reshape2::dcast(data = x, Year~Irg_Season,value.var = "Percent_Reduction_C")
    perc_labs = melt(perc_lab, id.vars = c("Year"), value.vars = c("Not Irrigation Season", "Irrigation Season"))
    perc_labs = perc_labs[order(perc_labs$Year),]

    # Get height of bars
    perc_y = melt(irgstack, id.vars = c("Year"), value.vars = c("Not Irrigation Season", "Irrigation Season"), value.name = "value1")
    perc_y = perc_y[order(perc_y$Year),]

    # Merge percent reductions and height at which they should be reported.
    percs <- merge(perc_labs,perc_y, all=TRUE)
    percs = percs[order(percs$Year),]

    # Get x pos of bars
    if(any(present==FALSE)){ # if irg or non irg are missing, that means that irg_conc only has one set of axis positions
      perc_at = irg_conc
    }else{
      perc_at = c(irg_conc[1,],irg_conc[2,]) # if both present, there are two rows of numbers
      perc_at = perc_at[order(perc_at)]
    }
    irgperc <- data.frame(perc_at,percs) # meld data with x axis position
    irgperc1 <- irgperc[irgperc$value>0&!is.na(irgperc$value),]

    # Plot text of percent reduction onto plot
    if(dim(irgperc1)[1]>0){
      irgperc1$percn <- paste(irgperc1$value,"%",sep="")
      text(irgperc1$perc_at,irgperc1$value1+0.1*mean(irgperc1$value1),labels=irgperc1$percn,cex=1)
    }
    ## CONC BOXPLOTS ##
    if(input$irg_medplot){
      # Obtain boxplot stats from geomean data
      y <- ecolidata[ecolidata$ML_Name==input$irgsite,c("MLID","Date","ML_Name","Irg_Season","E.coli_Geomean")]
      y$Year = lubridate::year(y$Date)

      # Update upper y axis limit if outliers are beyond max barplot height (usually are)
      uplim1 = quantile(y$E.coli_Geomean,1)
      uplim1 = max(uplim, uplim1)

      # Create original bar plot with different legend
      barplot(t(irgstack1), beside=buddies, names.arg = irgstack$Year, main="E.coli Geomeans by Year", ylim=c(0, uplim1*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
      abline(h=crits$geomcrit, col="black", lty=2, lwd=2)
      legend("topright",legend=c("Irrigation Season","Not Irrigation Season","Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(legcols[1],legcols[2],NA,NA,"white"),border=c("black","black","white","white","white"),lty=c(NA,NA,1,2,NA),lwd=c(NA,NA,3,2,NA),cex=1)
      box(bty="l")
      mtext(paste0("n=",ncount_text$Count), side = 1, line = 0, at = c(ncount_text$here), cex= 0.8)

      # x-axis arguments for boxplot based on barplot placement
      boxat = irgperc[,"perc_at"]
      boxplot(y$E.coli_Geomean~y$Irg_Season+y$Year,
              lty=1, xaxt="n",yaxt="n", frame=FALSE, col=ggplot2::alpha(colucols,0.1), boxwex = 0.7, at=boxat, add=TRUE)
    }
  }
  ### LOADING PLOTS ###
  if(input$irg_unit_type=="Loading"){
    # Define TMDL color
    loadcol = colorspace::rainbow_hcl(3)[3]
    # Filter to data needed to produce plots
    x <- irgdata[irgdata$ML_Name==input$irgsite,]
    x = x[complete.cases(x),]
    x = x[order(x$Year),]
    if(dim(x)[1]>0){
      # Determine if irg/non irg represented
      uni = unique(x$Irg_Season)
      uplim = max(c(x$Observed_Loading,x$TMDL))*1.2
      if(length(uni)>1){ # if both represented...
        par(mfrow=c(1,2)) # create two plot panes...
        # Separate into irg and non irg
        # Irrigation data
        irg_load.p <- x[x$Irg_Season=="Irrigation Season",names(x)%in%c("Observed_Loading","TMDL","Year","Ncount_irg_L")]
        irgload_ncount = irg_load.p$Ncount_irg_L
        rownames(irg_load.p)= irg_load.p$Year
        irg_load.p = irg_load.p[,!names(irg_load.p)%in%c("Year","Ncount_irg_L")]
        # Irg barplot
        barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim), main="Irrigation Season",ylab="E.coli Loading (GigaMPN/day)",col=c(colucols[1],loadcol))
        box(bty="l")
        
        #ncount
        ncount_here = colMeans(barp)
        mtext(paste0("n=",irgload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
        
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Irg_Season=="Irrigation Season"], x$Percent_Reduction_L[x$Irg_Season=="Irrigation Season"]))
        barperc <- barperc[barperc$V3>0,]
        # Plot percent reduction needed text, if applicable.
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
        }

        # Non-Irrigation data, same process
        nirg_load.p <- x[x$Irg_Season=="Not Irrigation Season",names(x)%in%c("Observed_Loading","TMDL","Year","Ncount_irg_L")]
        nirgload_ncount = nirg_load.p$Ncount_irg_L
        rownames(nirg_load.p)= nirg_load.p$Year
        nirg_load.p = nirg_load.p[,!names(nirg_load.p)%in%c("Year","Ncount_irg_L")]
        barp <- barplot(t(nirg_load.p), beside=T, names.arg=x$Year[x$Irg_Season=="Not Irrigation Season"], ylim=c(0, uplim), main="Not Irrigation Season",col=c(colucols[2],loadcol))
        legend("topright",legend=c("Observed Loading - Irrigation","Observed Loading - Not Irrigation","TMDL", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
        box(bty="l")
        
        #ncount
        ncount_here1 = colMeans(barp)
        mtext(paste0("n=",nirgload_ncount), side = 1, line = 0, at = c(ncount_here1), cex= 0.8)
        
        
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Irg_Season=="Not Irrigation Season"], x$Percent_Reduction_L[x$Irg_Season=="Not Irrigation Season"]))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
        }}else{ # If only one category represented...
          par(mfrow=c(1,1)) # only make one plot
          colucol = ifelse(uni=="Irrigation Season",colucols[1],colucols[2]) # redefine bar color
          irg_load.p <- x[,names(x)%in%c("Observed_Loading","TMDL","Year","Ncount_irg_L")]
          irgload_ncount = irg_load.p$Ncount_irg_L
          rownames(irg_load.p)= irg_load.p$Year
          irg_load.p = irg_load.p[,!names(irg_load.p)%in%c("Year","Ncount_irg_L")]
          # Bar plot singular - same as irg
          barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim), main=uni,ylab="E.coli Loading (GigaMPN/day)",col=c(colucol,loadcol))
          legend("topright",legend=c("Observed Loading - Irrigation","Observed Loading - Not Irrigation","TMDL", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
          box(bty="l")
          
          #ncount
          ncount_here = colMeans(barp)
          mtext(paste0("n=",irgload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          
          
          barps <- barp[1,]
          barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction_L))
          barperc <- barperc[barperc$V3>0,]
          if(dim(barperc)[1]>0){
            barperc$V3 <- paste(barperc$V3,"%",sep="")
            text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
          }
        }
      ## LOADING BOXPLOTS ##
      if(input$irg_medplot){
        # Obtain boxplot stats from loading data
        loaddata = workbook$LDC_Data
        y <- loaddata[loaddata$ML_Name==input$irgsite,c("MLID","ML_Name","Date","Irg_Season","TMDL","Observed_Loading")]
        y <- y[!is.na(y$Observed_Loading),]
        datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date","Irg_Season"), value.vars=c("TMDL","Observed_Loading"), variable.name = "Meas_Type")
        names(datstack)[names(datstack)=="value"]<-"Loading"
        datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])

        # Update upper y axis limit of outliers are beyond max barplot height
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)

        # Create OG barplot with new legend.
        if(length(uni)>1){# determine whether one or two plot panels needed...
          par(mfrow=c(1,2))
          barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Irg_Season=="Irrigation Season"], main="Irrigation Season",ylab="E.coli Loading (GigaMPN/day)",col=c(colucols[1],loadcol))
          box(bty="l")
          
          #ncount
          ncount_here = colMeans(barp)
          mtext(paste0("n=",irgload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading[datstack$Irg_Season=="Irrigation Season"]~datstack$Meas_Type[datstack$Irg_Season=="Irrigation Season"]+lubridate::year(datstack$Date)[datstack$Irg_Season=="Irrigation Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[1], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)


          barp <- barplot(t(nirg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Irg_Season=="Not Irrigation Season"], main="Not Irrigation Season",col=c(colucols[2],loadcol))
          legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","TMDL", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(colucols[1],colucols[2],loadcol,NA,"white"),border=c("black","black","black","white","white"),lty=c(NA,NA,NA,1,NA),lwd=c(NA,NA,NA,3,NA),cex=1)
          box(bty="l")
          
          #ncount
          ncount_here1 = colMeans(barp)
          mtext(paste0("n=",nirgload_ncount), side = 1, line = 0, at = c(ncount_here1), cex= 0.8)
          

          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading[datstack$Irg_Season=="Not Irrigation Season"]~datstack$Meas_Type[datstack$Irg_Season=="Not Irrigation Season"]+lubridate::year(datstack$Date)[datstack$Irg_Season=="Not Irrigation Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[2], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }else{
          par(mfrow=c(1,1))
          barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year, main=uni,ylab="E.coli Loading (GigaMPN/day)",col=c(colucol,loadcol))
          box(bty="l")
          
          #ncount
          ncount_here = colMeans(barp)
          mtext(paste0("n=",irgload_ncount), side = 1, line = 0, at = c(ncount_here), cex= 0.8)
          

          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]

          boxplot(datstack$Loading~datstack$Meas_Type+lubridate::year(datstack$Date),
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucol, loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }

      }
    }


  }

})

observe({
  req(input$irg_unit_type)
  irg_data = workbook$Irg_Season_Data
  irgdat <- irg_data[irg_data$ML_Name==input$irgsite,]
  irgdat = irgdat[order(irgdat$Year),]
  # Data table
  output$Irg_Data <- renderDT(irgdat,
                              rownames = FALSE,
                              options = list(dom="ft", paging = FALSE, scrollX=TRUE, scrollY = "300px"))
  
})


########################################### LOADING TAB ######################################

output$LDC <- renderPlot({
  req(input$pt_type)
  req(input$ldcsite)

  ldcdata = workbook$LDC_Data
  x = ldcdata[ldcdata$ML_Name==input$ldcsite,]
  flow.plot <- x[order(x$Flow_Percentile),]

  # Pull out observed loadings (E.coli data)
  ecoli.loads <- x[!is.na(x$E.coli_Geomean),]

  plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (GigaMPN/day)", xlim=c(0, 100), ylim=c(0,max(c(ecoli.loads$Observed_Loading, ecoli.loads$Loading_Capacity))), main=paste("Load Duration Curve:",x$ML_Name[1]))
  abline(v=10, lty=2)
  abline(v=40, lty=2)
  abline(v=60, lty=2)
  abline(v=90, lty=2)
  text(5, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"High \n Flows")
  text(25, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Moist \n Conditions")
  text(50, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Mid-Range \n Flows")
  text(75, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Dry \n Conditions")
  text(95, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Low \n Flows")
  lines(flow.plot$TMDL~flow.plot$Flow_Percentile, col="firebrick3", lwd=2)


# Plot types     
if(input$ldc_type == "Scatterplot"){
  if(input$pt_type=="Calendar Seasons"){
    colpal <- colorspace::sequential_hcl(4)
    wine <- ecoli.loads[ecoli.loads$CalSeason=="Winter",]
    spre <- ecoli.loads[ecoli.loads$CalSeason=="Spring",]
    sume <- ecoli.loads[ecoli.loads$CalSeason=="Summer",]
    fale <- ecoli.loads[ecoli.loads$CalSeason=="Fall",]
    
    
    points(wine$Observed_Loading~wine$Flow_Percentile, pch=21, col="black", bg=colpal[4], cex=2)
    points(spre$Observed_Loading~spre$Flow_Percentile, pch=21, col="black", bg=colpal[3], cex=2)
    points(sume$Observed_Loading~sume$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
    points(fale$Observed_Loading~fale$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
    legend("topright",legend=c("TMDL", "E.coli Loading - Winter", "E.coli Loading - Spring", "E.coli Loading - Summer","E.coli Loading - Fall"), bty="n", col=c("firebrick3","black","black","black","black"), lty=c(1,NA,NA,NA,NA),lwd=c(2,NA,NA,NA,NA),pch=c(NA,21,21,21,21), pt.bg=c(NA,colpal[4],colpal[3],colpal[2],colpal[1]), pt.cex=c(NA,2,2,2,2),cex=1)
  }

  # Point colors
  if(input$pt_type=="Recreation Seasons"){
    colpal <- colorspace::rainbow_hcl(2)
    rec <- ecoli.loads[ecoli.loads$Rec_Season=="Rec Season",]
    nonrec <- ecoli.loads[ecoli.loads$Rec_Season=="Not Rec Season",]
    
    points(rec$Observed_Loading~rec$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
    points(nonrec$Observed_Loading~nonrec$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
    legend("topright",legend=c("TMDL","E.coli Loading - Rec", "E.coli Loading - Non-Rec"), bty="n", col=c("firebrick3","black","black"), lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,21,21), pt.bg=c(NA,colpal), pt.cex=c(NA,2,2),cex=1)
    
  }
  
  if(input$pt_type=="Irrigation Seasons"){
    colpal <- colorspace::terrain_hcl(2)
    irg <- ecoli.loads[ecoli.loads$Irg_Season=="Irrigation Season",]
    nonirg <- ecoli.loads[ecoli.loads$Irg_Season=="Not Irrigation Season",]
    
    points(irg$Observed_Loading~irg$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
    points(nonirg$Observed_Loading~nonirg$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
    legend("topright",legend=c("TMDL", "E.coli Loading - Irrigation", "E.coli Loading - No Irrigation"), bty="n", col=c("firebrick3","black","black"), lty=c(1,NA,NA),lwd=c(2,NA,NA),pch=c(NA,21,21), pt.bg=c(NA,colpal), pt.cex=c(NA,2,2),cex=1)
    
  }
}else{
  colpal <- colorspace::sequential_hcl(4)
  # Add boxplots
  ecoli.loads$Flow_Cat = "High"
  ecoli.loads$Flow_Cat[ecoli.loads$Flow_Percentile>10&ecoli.loads$Flow_Percentile<=40] = "Moist"
  ecoli.loads$Flow_Cat[ecoli.loads$Flow_Percentile>40&ecoli.loads$Flow_Percentile<=60] = "MidRange"
  ecoli.loads$Flow_Cat[ecoli.loads$Flow_Percentile>60&ecoli.loads$Flow_Percentile<=90] = "Dry"
  ecoli.loads$Flow_Cat[ecoli.loads$Flow_Percentile>90&ecoli.loads$Flow_Percentile<=100] = "Low"
  ecoli.loads$Flow_Cat = factor(ecoli.loads$Flow_Cat, levels= c("High","Moist", "MidRange","Dry","Low"))
  
  boxplot(ecoli.loads$Observed_Loading~ecoli.loads$Flow_Cat, col=ggplot2::alpha(colpal[1],0.5), at = c(5,25,50,75,95),lty=1, xaxt="n", frame=FALSE, boxwex = 5, add=TRUE)
  legend("topright",legend=c("TMDL", "E.coli Loading"), bty="n", col=c("firebrick3","black"), lty=c(1,NA),lwd=c(2,NA),pch=c(NA,22), pt.bg=c(NA,ggplot2::alpha(colpal[1],0.5)), pt.cex=c(NA,2),cex=1)
  
  }
  
    
})

output$LDC_Data <- renderDT(workbook$LDC_Data,
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
