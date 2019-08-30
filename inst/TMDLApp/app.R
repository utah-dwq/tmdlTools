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
require(tidyr)

source("tmdlCalcs.R")

perc.red <- function(x,y){100-x/y*100}
deqPalette <- c("#0080b7","#00afd8","#ADD361")
dwqPalette <- c("#034963","#0b86a3","#00a1c6")
boxcolors = dwqPalette
linecolors = deqPalette

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
                                     mainPanel(h4("Data View"),
                                               h5("Toggle between sheets in the workbook using the drop down menu at the bottom of the sidebar."),
                                               DTOutput("datview"), style= "font-size:75%")
                                     ),
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
                            tabPanel("Upstream-Downstream",
                                     h3("Bacterial Concentrations Upstream to Downstream"),
                                     h5("Use the date range slider to select the period of record over which to view ",em("E.coli"), " concentrations. The red dotted line in the boxplot represents the maximum",em("E.coli"), "concentration criterion, while the orange dotted line represents the geometric mean criterion."),
                                     sidebarPanel(uiOutput("usds_date"),
                                                  checkboxInput("vieworddat", label = "View data points?")),
                                     mainPanel(plotOutput("UD_Geomeans", height="700px"))),
                            tabPanel("Monthly",
                                     h3("Bacterial Concentrations/Loadings by Month"),
                                     sidebarPanel(uiOutput("monthsite"),
                                                  div(id="date1",uiOutput("mondatrange")),
                                                  br(),
                                                  uiOutput("mon_unit_type"),
                                                  checkboxInput("viewmondat", label = "View data points?")),
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
                                                  checkboxInput("viewrecdat", label = "View data points?")),
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
                                                  checkboxInput("viewirgdat", label = "View data points?")),
                                     mainPanel(plotOutput("Irg_Geomeans", height="800px"),
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
                                               div(DT::dataTableOutput("LDC_Data", height=20),style="font-size:75%")))#,
                            # tabPanel("User Guide",
                            #          includeMarkdown("user_guide.Rmd"))
              )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
###################################### FILE UPLOAD SECTION ################################

# File Info
  workbook <- reactiveValues()
  snames <- reactiveValues()
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
   workbook$Ecoli_data = out$Ecoli_data
   workbook$Inputs = out$Inputs
   if(!is.null(out$Site_order)){
     workbook$Site_order = out$Site_order
   }
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
   snames$sheets = names(workbook)[!names(workbook)=="wb_path"]
 })

 # Create drop down menu of sheets contained in xlsx file
 
 output$selectsheet <- renderUI({
   req(workbook$Inputs)
   selectInput("selectsheet", label = "Select sheet to view.", selected = NULL, choices=c(snames$sheets))
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
   if(length(wbdownload)>7){
     worksheetOrder(wb) = c(5,1,7,3,9,2,4,6,8)
   }else{worksheetOrder(wb) = c(4,1,6,7,2,3,5)}
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
                              rownames = FALSE,selection='none',filter="top",
                              options = list(scrollY = '600px', paging = FALSE, scrollX=TRUE))

 })

# Add loading tab if loadings present
  observe({
    if(!is.null(workbook$LDC_Data)){
      showTab(inputId="all_the_things", target = "Load Duration Curves")
    }else{
      hideTab(inputId="all_the_things", target = "Load Duration Curves")
    }
  })

# Add upstream downstream tab if order present
  observe({
    if(!is.null(workbook$Site_order)){
      showTab(inputId="all_the_things", target = "Upstream-Downstream")
    }else{
      hideTab(inputId="all_the_things", target = "Upstream-Downstream")
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

# Create ecoli dataset and place date range and colors into reactive object
observe({
  req(workbook$Daily_Geomean_Data)
  # Assign site colors
  colrs <- yarrr::piratepal("basel")
  sites = unique(workbook$Daily_Geomean_Data$ML_Name)
  nsites = length(sites)
  ML_Col = as.character(rep(colrs, length.out = nsites))
  sitecols = data.frame(ML_Name = sites, ML_Col = rep(colrs, length.out = nsites))
  timeseriesdat$sitecols = sitecols
  
  # Sort to ecoli sites and date range
  x = workbook$Daily_Geomean_Data
  x = x[x$ML_Name %in% input$checkbox,]
  if(!is.null(input$tsdatrange)){
    timeseriesdat$min = input$tsdatrange[1]
    timeseriesdat$max = input$tsdatrange[2] 
  }
  timeseriesdat$x <- x[x$Date>=timeseriesdat$min&x$Date<=timeseriesdat$max,]
  
})

# Create flow dataset 
observe({
  req(input$checkbox1)
  x1 = workbook$Flow_data
  x1 = x1[x1$ML_Name %in% input$checkbox1,]
  timeseriesdat$x1 <- x1[x1$Date>=timeseriesdat$min&x1$Date<=timeseriesdat$max,]
})

output$Time_Series <- renderPlot({
  req(timeseriesdat$min,timeseriesdat$max)
  colrs = timeseriesdat$sitecols
  min = timeseriesdat$min
  max = timeseriesdat$max

# Base plot  
if(!is.null(input$checkbox)|!is.null(input$checkbox1)){
  # Create an empty plot
  plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min, max), ylim=c(0, 2420))
  axis.Date(1, at=seq(min, max, by="6 months"), format="%m-%Y", las=2, cex=0.8)
  abline(h=crits$maxcrit,col="orange", lwd=2)
  abline(h=crits$geomcrit, col="red", lwd=2)
  text(min+200,crits$maxcrit-100, paste0("Max Crit - ",crits$maxcrit," MPN/100 mL"))
  text(min+500,crits$geomcrit-100, paste0("Geometric Mean Crit - ",crits$geomcrit," MPN/100 mL"))
  site = vector()
  colr = vector()
}

# E.coli plots
  if(!is.null(input$checkbox)){
    x = timeseriesdat$x
    # Get number of sites
    uni.sites <- unique(x$ML_Name)
    
    # Start plotting ecoli concentrations
    for(i in 1:length(uni.sites)){
      concol = as.character(colrs$ML_Col[colrs$ML_Name == uni.sites[i]])
      y = x[x$ML_Name==uni.sites[i],]
      perc.exc = round(length(y$E.coli_Geomean[y$E.coli_Geomean>as.numeric(crits$maxcrit)])/length(y$E.coli_Geomean)*100, digits=0)
      if(input$plottype=="Line"){
        lines(y$E.coli_Geomean~y$Date, lwd=1, lty=1, col=concol)
      }
      points(y$E.coli_Geomean~y$Date, pch=21, cex=2, col="black", bg=concol)
      site[i] = paste0(as.character(uni.sites[i])," (",perc.exc,"% Exceed)")
      colr[i] = concol
    }
    l=legend("topleft",c(site),col="black",pt.bg=c(colr), pch=21, bty="n", pt.cex=2,cex=1)
  }
  
# Flow plots
  if(!is.null(input$checkbox1)){
    x1 = timeseriesdat$x1
    uni.sites.1 = unique(x1$ML_Name)
    site1 = vector()
    colr1 = vector()
    par(new = TRUE)
    plot(1, type="n", xlab="", ylab="", axes = F, xlim=c(min, max), ylim = c(0,max(x1$Flow)))
    axis(side = 4)
    mtext(side = 4, line = 2, "Flow (cfs)")
    for(i in 1:length(uni.sites.1)){
      flowcol = as.character(colrs$ML_Col[colrs$ML_Name == uni.sites.1[i]])
      y1 = x1[x1$ML_Name==uni.sites.1[i],]
      if(input$plottype=="Line"){
        lines(y1$Flow~y1$Date, lwd=1, lty=1, col=flowcol)
      }
      points(y1$Flow~y1$Date, pch=23, cex=2, col="black", bg=ggplot2::alpha(flowcol, 0.6))
      site1[i] = uni.sites.1[i]
      colr1[i] = flowcol
    }
    l=legend("topright",c(site1),col="black",pt.bg=c(ggplot2::alpha(colr1, 0.6)), pch=23, bty="n", pt.cex=2,cex=1)
  }
})

####################################### UPSTREAM DOWNSTREAM SECTION ##########################

# Dates to choose from
output$usds_date <- renderUI({
  req(workbook$Daily_Geomean_Data)
  us_ds_data <- workbook$Daily_Geomean_Data
  sliderInput("usdsdate",
              label="Date Range",
              min=min(us_ds_data$Date),
              max=max(us_ds_data$Date),
              value = c(min(us_ds_data$Date),max(us_ds_data$Date)),
              dragRange = TRUE, timeFormat="%Y-%m-%d")
})

output$UD_Geomeans <- renderPlot({
  req(input$usdsdate)
  par(mar= c(10,4,4,1))
  geomeans <- workbook$Daily_Geomean_Data
  selgeomeans <- geomeans[geomeans$Date>=input$usdsdate[1]&geomeans$Date<=input$usdsdate[2],]
  ranks <- workbook$Site_order
  usds_data = merge(selgeomeans, ranks, all.x = TRUE)
  usds_data$ML_Name = factor(usds_data$ML_Name, levels = c(as.character(ranks$ML_Name)))
  udn_count = tapply(usds_data$E.coli_Geomean, usds_data$ML_Name, length)
  
  boxplot(usds_data$E.coli_Geomean~usds_data$ML_Name, ylab = "E.coli (MPN/100 mL)", xlab = "",ylim = c(-50, max(usds_data$E.coli_Geomean)),col = boxcolors[2], lty = 1, outline = FALSE, las = 2, cex.axis = 0.8)
  abline(h = crits$geomcrit, col = linecolors[3], lwd = 3, lty = 2)
  abline(h = crits$maxcrit, col = linecolors[2], lwd = 3, lty = 2)
  legend("topright",legend = c("Max Crit","Geom Crit"), lty = c(2,2), lwd = c(3,3), col = c(linecolors[2],linecolors[3]), bty = "n")
  
  #mtext(paste0("n=",udn_count), side = 1, line = 3, at = 1:length(unique(usds_data$ML_Name)), cex= 1)
  text(x = 1:length(unique(usds_data$ML_Name)), y = rep(-40, length(unique(usds_data$ML_Name))), paste0("n=",udn_count), cex = 0.8)
  # add data points
  if(input$vieworddat){
    points(usds_data$Order, usds_data$E.coli_Geomean, pch = 22, cex = 1.2, col = "black", bg = boxcolors[1])
  }
  
  })

###################################### MONTH TAB SECTION #####################################

# Sites to choose from
output$monthsite <- renderUI({
  req(workbook$Daily_Geomean_Data)
  selectInput("monthsite",
              label = "Site Name",
              choices=unique(workbook$Daily_Geomean_Data$ML_Name))
})

# Dates to choose from
output$mondatrange <- renderUI({
  req(input$monthsite)
  req(workbook$Daily_Geomean_Data)
  sitedates = workbook$Daily_Geomean_Data[workbook$Daily_Geomean_Data$ML_Name==input$monthsite,]
  sliderInput("mondatrange",
              label="Date Range",
              min=min(sitedates$Date),
              max=max(sitedates$Date),
              value = c(min(sitedates$Date),max(sitedates$Date)),
              dragRange = TRUE, timeFormat="%Y-%m-%d")
})

# Craft drop down menu for concentration and loading
output$mon_unit_type <- renderUI({
  req(input$mondatrange)
  req(workbook$Daily_Geomean_Data)
  monthdata <- workbook$Monthly_Data
  monthdata = monthdata[monthdata$ML_Name==input$monthsite&!is.na(monthdata$Observed_Loading),"Observed_Loading"]
  if(length(monthdata)>0){
    subd=c("Concentration","Loading")
  }else{subd=c("Concentration")}
  selectInput("mon_unit_type","Select Measurement Type", choices = subd)
})

# Create dataset for use in plots and tables
selectedmonthdata <- reactiveValues()

observe({
  req(input$mon_unit_type)
  if(input$mon_unit_type=="Concentration"){
    dailygeomeans <- workbook$Daily_Geomean_Data
    seldailygeomeans <- dailygeomeans[dailygeomeans$ML_Name==input$monthsite&dailygeomeans$Date>=input$mondatrange[1]&dailygeomeans$Date<=input$mondatrange[2],]
    seldailygeomeans$month = lubridate::month(seldailygeomeans$Date, label=TRUE)
    monthpositions_1 = data.frame("position" = 1:12, "month" = month.abb)
    seldailygeomeans = merge(seldailygeomeans,monthpositions_1, all.x = TRUE)
    selectedmonthdata$alldata = seldailygeomeans
    
    aggseldg0 <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=function(x){exp(mean(log(x)))})
    aggseldg0$E.coli_Geomean = round(aggseldg0$E.coli_Geomean, digits = 1)
    aggseldg1 <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=length)
    names(aggseldg1)[names(aggseldg1)=="E.coli_Geomean"] <- "Ncount"
    aggseldg2 <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=seldailygeomeans, FUN=median)
    names(aggseldg2)[names(aggseldg2)=="E.coli_Geomean"] <- "Median"
    aggseldg2$Median = round(aggseldg2$Median, digits = 1)
    aggseldg = merge(aggseldg0, aggseldg1, all = TRUE)
    aggseldg = merge(aggseldg, aggseldg2, all = TRUE)
    aggseldg$Percent_Reduction <- ifelse(aggseldg$E.coli_Geomean>crits$geomcrit,round(perc.red(crits$geomcrit,aggseldg$E.coli_Geomean), digits=0),0)
    aggseldg = merge(aggseldg, monthpositions_1, all.x = TRUE)
    selectedmonthdata$aggregdata = aggseldg
    table = aggseldg[,!names(aggseldg)%in%c("position","MLID","ML_Name")]
    table = table[,c("month","Ncount","E.coli_Geomean","Median","Percent_Reduction")]
    table = table[order(table$month),]
    
    output$Monthly_Data <- renderDT(table,
                                    colnames = c("Month","Ncount","Geomean (MPN/100 mL)","Median (MPN/100 mL)","Reduction Needed (%)"),
                                    rownames = FALSE,selection='none',
                                    options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
    
  }
  
  # Need to stop app from crashing when loading data not available from user selection
  if(input$mon_unit_type=="Loading"&input$monthsite%in%workbook$LDC_Data$ML_Name){
    
    # Isolate to site and period of interest
    mon_loadings = workbook$LDC_Data
    mon_loads <- mon_loadings[mon_loadings$ML_Name==input$monthsite&mon_loadings$Date>=input$mondatrange[1]&mon_loadings$Date<=input$mondatrange[2],]
    mon_loads$month = lubridate::month(mon_loads$Date, label=TRUE)

    # Flatten by loading type
    monloads_flat = reshape2::melt(mon_loads, measure.vars = c("TMDL","Observed_Loading"), variable.name = "Load_Type", value.name = "Loading")
    selectedmonthdata$alldata = monloads_flat
    
    # Create summary table for app
    agg_monloads0 = aggregate(Loading~month+Load_Type, dat = monloads_flat, FUN=function(x){exp(mean(log(x)))})
    agg_monloads0$Loading = round(agg_monloads0$Loading, digits = 1)
    names(agg_monloads0)[names(agg_monloads0)=="Loading"] <- "Geomean_Loading"
    agg_monloads1 = aggregate(Loading~month+Load_Type, dat = monloads_flat, FUN=length)
    names(agg_monloads1)[names(agg_monloads1)=="Loading"] <- "Ncount"
    agg_monloads2 = aggregate(Loading~month+Load_Type, dat = monloads_flat, FUN=median)
    agg_monloads2$Loading = round(agg_monloads2$Loading, digits = 1)
    names(agg_monloads2)[names(agg_monloads2)=="Loading"] <- "Median_Loading"
    aggseldg = merge(agg_monloads0, agg_monloads1, all = TRUE)
    aggseldg = merge(aggseldg, agg_monloads2, all = TRUE)
    
    # Percent Reduction calcs
    library(tidyr)
    gmean_isolated = aggseldg[,c("month","Load_Type","Geomean_Loading")]
    gmean_iso = gmean_isolated %>% spread(Load_Type,Geomean_Loading)
    gmean_iso$Percent_Reduction <- ifelse(gmean_iso$Observed_Loading>gmean_iso$TMDL,round(perc.red(gmean_iso$TMDL,gmean_iso$Observed_Loading), digits = 0),0)
    gmean_iso = gmean_iso[!is.na(gmean_iso$Observed_Loading),c("month","Percent_Reduction")]
    gmean_iso$Load_Type = "Observed_Loading"
    aggseldg2 = merge(aggseldg, gmean_iso, all.x = TRUE)
    selectedmonthdata$aggregdata = aggseldg2
    
    table = aggseldg2
    table = table[order(table$Load_Type, table$month),c("month","Load_Type","Ncount","Geomean_Loading","Median_Loading","Percent_Reduction")]
    output$Monthly_Data <- renderDT(table,
                                    colnames = c("Month","Load Type","Ncount","Geomean (MPN/day)","Median (MPN/day)","Reduction Needed (%)"),
                                    rownames = FALSE,selection='none',filter="top",
                                    options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
  }
})

output$Monthly_Geomeans <- renderPlot({
  req(selectedmonthdata$aggregdata)
      if(input$mon_unit_type=="Concentration"){
        y = selectedmonthdata$alldata
        y = droplevels(y[order(y$month),])
        positions = unique(y[,c("month","position")])
        posi = positions[order(positions$month),]
        boxplot(y$E.coli_Geomean~y$month, at = unique(y$position), ylab = "E.coli (MPN/100 mL)", xlab = "",col = boxcolors[2], lty = 1, outline = FALSE)
        abline(h = crits$geomcrit, col = linecolors[3], lwd = 3, lty = 2)
        abline(h = crits$maxcrit, col = linecolors[2], lwd = 3, lty = 2)
        legend("topright",legend = c("Max Crit","Geom Crit"), lty = c(2,2), lwd = c(3,3), col = c(linecolors[2],linecolors[3]), bty = "n")
        
        ncounts = selectedmonthdata$aggregdata[,c("month","Ncount")]
        ncounts = ncounts$Ncount[order(ncounts$month)]
        
        mtext(paste0("n=",ncounts), side = 1, line = 3, at = posi$position, cex=1)
        
        # add data points
        if(input$viewmondat){
          points(y$position, y$E.coli_Geomean, pch = 22, cex = 1.2, col = "black", bg = boxcolors[1])
        }
      }
      if(input$mon_unit_type=="Loading"&input$monthsite%in%workbook$LDC_Data$ML_Name){
        
        monloads_flat = selectedmonthdata$alldata
        
        # Get boxplot labels
        monlabs = sort(unique(monloads_flat$month))
        monthpositions = c(1:35)
        spacing = monthpositions%%3
        where = monthpositions[!spacing==0]
        monlab_pos = where[c(TRUE, FALSE)]+0.5
        
        repmons = data.frame("month" = rep(month.abb, each = 2), "position" = where, "Load_Type"= rep(c("TMDL","Observed_Loading"),12))
        
        # data frame with all loading data and their positions 
        pointpos = merge(monloads_flat, repmons, all.x = TRUE)
        
        boxplot(monloads_flat$Loading~monloads_flat$Load_Type+monloads_flat$month, at = where, xaxt = "n", ylab = "Loading (MPN/day)", xlab = "",col = boxcolors[1:2], lty = 1, outline = FALSE)
        axis(side = 1, at = monlab_pos,labels = month.abb)
        legend("topright",legend = c("TMDL","Observed Loading"), pch = c(15,15), col = c(boxcolors[1],boxcolors[2]), pt.bg = "black", pt.cex = 1.5, bty = "n")
        mtext(paste0("n=",selectedmonthdata$aggregdata$Ncount), side = 1, line = 3, at = where, cex= 0.7)
        
        if(input$viewmondat){
          points(pointpos$position, pointpos$Loading, pch = 22, cex = 1, col = "black", bg = boxcolors[3])
        }
      }
      })

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

# Create rec data reactive object
recdataset <- reactiveValues()

observe({
  req(input$rec_unit_type)
  
  recdataset$aggregdat = workbook$Rec_Season_Data[workbook$Rec_Season_Data$ML_Name==input$recsite,]
  
  if(input$rec_unit_type=="Concentration"){
    concdata = workbook$Daily_Geomean_Data[workbook$Daily_Geomean_Data$ML_Name==input$recsite,]
    concdata$Year = lubridate::year(concdata$Date)
    concdata$Rec_Season = factor(concdata$Rec_Season, levels = c("Rec Season","Not Rec Season"))
    
    recdataset$data = concdata
    
    # For the table
    aggreg = recdataset$aggregdat
    concmed = aggregate(E.coli_Geomean~Rec_Season+Year, data = concdata, FUN = median)
    names(concmed)[names(concmed)=="E.coli_Geomean"] <- "Median"
    aggreg1 = merge(aggreg, concmed, all.x = TRUE)
    aggreg1$E.coli_Geomean = round(aggreg1$E.coli_Geomean, digits = 1)
    aggreg2 = aggreg1[,c("Year","Rec_Season","Ncount_rec_C","E.coli_Geomean","Median","Percent_Reduction_C")]
    names(aggreg2) = c("Year","Season","Ncount","Geomean (MPN/100 mL)", "Median (MPN/100 mL)", "Percent Reduction (%)")
    output$Rec_Data <- renderDT(aggreg2,
                                    rownames = FALSE,selection='none',
                                    options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
  }
  
  if(input$rec_unit_type=="Loading"&input$recsite%in%workbook$LDC_Data$ML_Name){
    loaddat = workbook$LDC_Data[workbook$LDC_Data$ML_Name==input$recsite,]
    
    # Because flow data may be present outside of the time period of ecoli data, ensure loading dataset cut to a date range with both TMDL and observed loading
    mindate = min(loaddat$Date[!is.na(loaddat$Observed_Loading)])
    maxdate = max(loaddat$Date[!is.na(loaddat$Observed_Loading)])
    loaddat = loaddat[loaddat$Date>=mindate&loaddat$Date<=maxdate,]
    loaddat$Year = lubridate::year(loaddat$Date)
    
    recdataset$data = loaddat
    
    # For the table
    aggreg = recdataset$aggregdat
    
    # Calculate median
    oloadmed = aggregate(Observed_Loading~Rec_Season+Year, data = loaddat, FUN = median)
    oloadmed$Load_Type = "Observed_Loading"
    names(oloadmed)[names(oloadmed)=="Observed_Loading"] <- "Median"
    tloadmed = aggregate(TMDL~Rec_Season+Year, data = loaddat, FUN = median)
    tloadmed$Load_Type = "TMDL"
    names(tloadmed)[names(tloadmed)=="TMDL"] <- "Median"
    loadmeds = merge(oloadmed, tloadmed, all = TRUE)
    
    # Flatten aggregated data
    aggreg1 = tidyr::gather(aggreg, key = "Load_Type", value = "Geomean_Loading", Observed_Loading, TMDL)
    aggreg1 = aggreg1[!is.na(aggreg1$Geomean_Loading),]
    
    aggreg2 = merge(aggreg1, loadmeds, all.x = TRUE)
    aggreg2$Percent_Reduction_L[aggreg2$Load_Type=="TMDL"] <- "Not applicable"
    aggreg3 = aggreg2[,c("Year","Rec_Season","Ncount_rec_L","Load_Type","Geomean_Loading","Median","Percent_Reduction_L")]
    aggreg3$Geomean_Loading = round(aggreg3$Geomean_Loading, digits = 1)
    names(aggreg3) = c("Year","Season","Ncount","Load Type","Geomean (MPN/day)", "Median (MPN/100 mL)", "Percent Reduction (%)")
    output$Rec_Data <- renderDT(aggreg3,
                                rownames = FALSE,selection='none',filter="top",
                                options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
    
    
  }
  
})

output$Rec_Geomeans <- renderPlot({
  req(input$rec_unit_type)
  
  recdata = recdataset$data
  aggdata = recdataset$aggregdat
  
  if(input$rec_unit_type=="Concentration"){
    
    # Get x-axis set up
    yearnum = length(unique(recdata$Year))
    positions = 1:(yearnum*2+(yearnum-1))
    rid = positions%%3
    positions = positions[!rid==0]
    yearpos = data.frame("Year"=rep(unique(recdata$Year),each = 2), "position" = positions, "Rec_Season" = rep(c("Rec Season","Not Rec Season"),yearnum))
    yearlab = data.frame("position" = positions[c(TRUE,FALSE)]+0.5, "Year" = unique(recdata$Year))

    recdata1 = merge(recdata,yearpos, all.x = TRUE)
    aggdata1 = merge(aggdata, yearpos, all.x = TRUE)
    
    boxplot(recdata1$E.coli_Geomean~recdata1$Rec_Season+recdata1$Year, at = positions, xaxt = "n", xlab = "", ylab = "E.coli (MPN/100 mL)",col = boxcolors[2:1], lty = 1, outline = FALSE)
    axis(1, at = yearlab$position, label = yearlab$Year)
    abline(h = crits$geomcrit, col = linecolors[3], lwd = 3, lty = 2)
    abline(h = crits$maxcrit, col = linecolors[2], lwd = 3, lty = 2)
    legend("topleft",legend = c("Rec","Not Rec",paste("Max Crit -",crits$maxcrit),paste("Geom Crit -",crits$geomcrit)), pch = c(22,22,NA,NA), lty = c(NA, NA, 2,2), lwd = c(NA, NA, 3,3), pt.bg = c(boxcolors[2],boxcolors[1],NA,NA), col = c("black","black", linecolors[2],linecolors[3]),bty = "n")
    #text(aggdata1$position,rep(-10, length(aggdata1$position)), labels = paste("n =",aggdata1$Ncount_rec_C), cex = 0.7)
    mtext(paste("n =",aggdata1$Ncount_rec_C), side = 1, line = 3, at = aggdata1$position, las = 2, cex = 0.9)
    
    # Add data points
    if(input$viewrecdat){
      points(recdata1$position, recdata1$E.coli_Geomean, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
  
  }
  
  if(input$rec_unit_type=="Loading"&input$recsite%in%workbook$LDC_Data$ML_Name){
    
    recdata2 = recdata[!is.na(recdata$Rec_Season),]
    recdata_flat = tidyr::gather(recdata2, key = "Type", value = "Load", c("TMDL","Observed_Loading"))
    
    plotrange = c(min(recdata_flat$Load),max(recdata_flat$Load))
    
    ### Rec season

    recl = recdata_flat[which(recdata_flat$Rec_Season=="Rec Season"),]
    
    # Get x-axis set up
    yearnum1 = length(unique(recl$Year))
    positions1 = 1:(yearnum1*2+(yearnum1-1))
    rid1 = positions1%%3
    positions1 = positions1[!rid1==0]
    yearpos1 = data.frame("Year"=rep(unique(recl$Year),each = 2), "position" = positions1, "Type" = rep(c("Observed_Loading","TMDL"),yearnum1))
    yearlab1 = data.frame("position" = positions1[c(TRUE,FALSE)]+0.5, "Year" = unique(recl$Year))
    
    recl = merge(recl,yearpos1, all.x = TRUE)
    aggdata1 = merge(aggdata, yearlab1, all.x = TRUE)
    aggyear1 = unique(aggdata1[which(aggdata1$Rec_Season=="Rec Season"),c("Year","Ncount_rec_L","position")])
    aggyear1 = aggyear1[complete.cases(aggyear1),]
    
    ### Not rec season
    nrecl = recdata_flat[which(recdata_flat$Rec_Season=="Not Rec Season"),]
    
    # Get x-axis set up
    yearnum2 = length(unique(nrecl$Year))
    positions2 = 1:(yearnum2*2+(yearnum2-1))
    rid2 = positions2%%3
    positions2 = positions2[!rid2==0]
    yearpos2 = data.frame("Year"=rep(unique(nrecl$Year),each = 2), "position" = positions2, "Type" = rep(c("Observed_Loading","TMDL"),yearnum2))
    yearlab2 = data.frame("position" = positions2[c(TRUE,FALSE)]+0.5, "Year" = unique(nrecl$Year))
    
    nrecl = merge(nrecl,yearpos2, all.x = TRUE)
    aggdata2 = merge(aggdata, yearlab2, all.x = TRUE)
    aggyear2 = unique(aggdata2[which(aggdata2$Rec_Season=="Not Rec Season"),c("Year","Ncount_rec_L","position")])
    aggyear2 = aggyear2[complete.cases(aggyear2),]
    
    par(mfrow = c(1,2))
    
    boxplot(recl$Load~recl$Type+recl$Year, at = positions1, main = "Rec Season", xaxt = "n", xlab = "", ylab = "Loading (MPN/day)",ylim = c(0,plotrange[2]),col = boxcolors[2:1], lty = 1, outline = FALSE)
    axis(1, at = yearlab1$position, label = yearlab1$Year)
    legend("topleft",legend = c("Rec Season","Not Rec Season","TMDL"), pch = c(22,22,22), pt.bg = c(boxcolors[2],boxcolors[3],boxcolors[1]), col = c("black","black", "black"),bty = "n")
    text(aggyear1$position,rep(-10, length(aggyear1$position)), labels = paste("n =",aggyear1$Ncount_rec_L), cex = 0.8)
    
    if(input$viewrecdat){
      points(recl$position, recl$Load, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
    
    boxplot(nrecl$Load~nrecl$Type+nrecl$Year, at = positions2, main = "Not Rec Season", xaxt = "n", xlab = "", ylim = c(0,plotrange[2]),ylab = "",col = boxcolors[c(3,1)], lty = 1, outline = FALSE)
    axis(1, at = yearlab2$position, label = yearlab2$Year)
    text(aggyear2$position,rep(-10, length(aggyear2$position)), labels = paste("n =",aggyear2$Ncount_rec_L), cex = 0.8)
  
    if(input$viewrecdat){
      points(nrecl$position, nrecl$Load, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
    
    }
  
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

# Create irg data reactive object
irgdataset <- reactiveValues()

observe({
  req(input$irg_unit_type)
  
  irgdataset$aggregdat = workbook$Irg_Season_Data[workbook$Irg_Season_Data$ML_Name==input$irgsite,]
  
  if(input$irg_unit_type=="Concentration"){
    concdata = workbook$Daily_Geomean_Data[workbook$Daily_Geomean_Data$ML_Name==input$irgsite,]
    concdata$Year = lubridate::year(concdata$Date)
    concdata$Irg_Season = factor(concdata$Irg_Season, levels = c("Irrigation Season","Not Irrigation Season"))
    
    irgdataset$data = concdata
    
    # For the table
    aggreg = irgdataset$aggregdat
    concmed = aggregate(E.coli_Geomean~Irg_Season+Year, data = concdata, FUN = median)
    names(concmed)[names(concmed)=="E.coli_Geomean"] <- "Median"
    aggreg1 = merge(aggreg, concmed, all.x = TRUE)
    aggreg1$E.coli_Geomean = round(aggreg1$E.coli_Geomean, digits = 1)
    aggreg2 = aggreg1[,c("Year","Irg_Season","Ncount_irg_C","E.coli_Geomean","Median","Percent_Reduction_C")]
    names(aggreg2) = c("Year","Season","Ncount","Geomean (MPN/100 mL)", "Median (MPN/100 mL)", "Percent Reduction (%)")
    output$Irg_Data <- renderDT(aggreg2,
                                rownames = FALSE,selection='none',
                                options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
  }
  
  if(input$irg_unit_type=="Loading"&input$irgsite%in%workbook$LDC_Data$ML_Name){
    loaddat = workbook$LDC_Data[workbook$LDC_Data$ML_Name==input$irgsite,]
    
    # Because flow data may be present outside of the time period of ecoli data, ensure loading dataset cut to a date range with both TMDL and observed loading
    mindate = min(loaddat$Date[!is.na(loaddat$Observed_Loading)])
    maxdate = max(loaddat$Date[!is.na(loaddat$Observed_Loading)])
    loaddat = loaddat[loaddat$Date>=mindate&loaddat$Date<=maxdate,]
    loaddat$Year = lubridate::year(loaddat$Date)
    
    irgdataset$data = loaddat
    
    # For the table
    aggreg = irgdataset$aggregdat
    
    # Calculate median
    oloadmed = aggregate(Observed_Loading~Irg_Season+Year, data = loaddat, FUN = median)
    oloadmed$Load_Type = "Observed_Loading"
    names(oloadmed)[names(oloadmed)=="Observed_Loading"] <- "Median"
    tloadmed = aggregate(TMDL~Irg_Season+Year, data = loaddat, FUN = median)
    tloadmed$Load_Type = "TMDL"
    names(tloadmed)[names(tloadmed)=="TMDL"] <- "Median"
    loadmeds = merge(oloadmed, tloadmed, all = TRUE)
    
    # Flatten aggregated data
    aggreg1 = tidyr::gather(aggreg, key = "Load_Type", value = "Geomean_Loading", Observed_Loading, TMDL)
    aggreg1 = aggreg1[!is.na(aggreg1$Geomean_Loading),]
    
    aggreg2 = merge(aggreg1, loadmeds, all.x = TRUE)
    aggreg2$Percent_Reduction_L[aggreg2$Load_Type=="TMDL"] <- "Not applicable"
    aggreg3 = aggreg2[,c("Year","Irg_Season","Ncount_irg_L","Load_Type","Geomean_Loading","Median","Percent_Reduction_L")]
    aggreg3$Geomean_Loading = round(aggreg3$Geomean_Loading, digits = 1)
    names(aggreg3) = c("Year","Season","Ncount","Load Type","Geomean (MPN/day)", "Median (MPN/100 mL)", "Percent Reduction (%)")
    output$Irg_Data <- renderDT(aggreg3,
                                rownames = FALSE,selection='none',filter="top",
                                options = list(scrollY = '700px', paging = FALSE, scrollX=FALSE, dom = "t"))
    
    
  }
  
})

output$Irg_Geomeans <- renderPlot({
  req(input$irg_unit_type)
  
  irgdata = irgdataset$data
  aggdata = irgdataset$aggregdat
  
  if(input$irg_unit_type=="Concentration"){
    
    # Get x-axis set up
    yearnum = length(unique(irgdata$Year))
    positions = 1:(yearnum*2+(yearnum-1))
    rid = positions%%3
    positions = positions[!rid==0]
    yearpos = data.frame("Year"=rep(unique(irgdata$Year),each = 2), "position" = positions, "Irg_Season" = rep(c("Irrigation Season","Not Irrigation Season"),yearnum))
    yearlab = data.frame("position" = positions[c(TRUE,FALSE)]+0.5, "Year" = unique(irgdata$Year))
    
    irgdata1 = merge(irgdata,yearpos, all.x = TRUE)
    aggdata1 = merge(aggdata, yearpos, all.x = TRUE)
    
    boxplot(irgdata1$E.coli_Geomean~irgdata1$Irg_Season+irgdata1$Year, at = positions, xaxt = "n", xlab = "", ylab = "E.coli (MPN/100 mL)",col = boxcolors[2:1], lty = 1, outline = FALSE)
    axis(1, at = yearlab$position, label = yearlab$Year)
    abline(h = crits$geomcrit, col = linecolors[3], lwd = 3, lty = 2)
    abline(h = crits$maxcrit, col = linecolors[2], lwd = 3, lty = 2)
    legend("topleft",legend = c("Irrigation","Not Irrigation",paste("Max Crit -",crits$maxcrit),paste("Geom Crit -",crits$geomcrit)), pch = c(22,22,NA,NA), lty = c(NA, NA, 2,2), lwd = c(NA, NA, 3,3), pt.bg = c(boxcolors[2],boxcolors[1],NA,NA), col = c("black","black", linecolors[2],linecolors[3]),bty = "n")
    #text(aggdata1$position,rep(-10, length(aggdata1$position)), labels = paste("n =",aggdata1$Ncount_irg_C), cex = 0.7)
    mtext(paste("n =",aggdata1$Ncount_irg_C), side = 1, line = 3, at = aggdata1$position, las = 2, cex = 0.9)
    # Add data points
    if(input$viewirgdat){
      points(irgdata1$position, irgdata1$E.coli_Geomean, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
    
  }
  
  if(input$irg_unit_type=="Loading"&input$irgsite%in%workbook$LDC_Data$ML_Name){
    
    irgdata2 = irgdata[!is.na(irgdata$Irg_Season),]
    irgdata_flat = tidyr::gather(irgdata2, key = "Type", value = "Load", c("TMDL","Observed_Loading"))
    
    plotrange = c(min(irgdata_flat$Load),max(irgdata_flat$Load))
    
    ### Rec season
    
    irgl = irgdata_flat[which(irgdata_flat$Irg_Season=="Irrigation Season"),]
    
    # Get x-axis set up
    yearnum1 = length(unique(irgl$Year))
    positions1 = 1:(yearnum1*2+(yearnum1-1))
    rid1 = positions1%%3
    positions1 = positions1[!rid1==0]
    yearpos1 = data.frame("Year"=rep(unique(irgl$Year),each = 2), "position" = positions1, "Type" = rep(c("Observed_Loading","TMDL"),yearnum1))
    yearlab1 = data.frame("position" = positions1[c(TRUE,FALSE)]+0.5, "Year" = unique(irgl$Year))
    
    irgl = merge(irgl,yearpos1, all.x = TRUE)
    aggdata1 = merge(aggdata, yearlab1, all.x = TRUE)
    aggyear1 = unique(aggdata1[which(aggdata1$Irg_Season=="Irrigation Season"),c("Year","Ncount_irg_L","position")])
    aggyear1 = aggyear1[complete.cases(aggyear1),]
    
    ### Not irg season
    nirgl = irgdata_flat[which(irgdata_flat$Irg_Season=="Not Irrigation Season"),]
    
    # Get x-axis set up
    yearnum2 = length(unique(nirgl$Year))
    positions2 = 1:(yearnum2*2+(yearnum2-1))
    rid2 = positions2%%3
    positions2 = positions2[!rid2==0]
    yearpos2 = data.frame("Year"=rep(unique(nirgl$Year),each = 2), "position" = positions2, "Type" = rep(c("Observed_Loading","TMDL"),yearnum2))
    yearlab2 = data.frame("position" = positions2[c(TRUE,FALSE)]+0.5, "Year" = unique(nirgl$Year))
    
    nirgl = merge(nirgl,yearpos2, all.x = TRUE)
    aggdata2 = merge(aggdata, yearlab2, all.x = TRUE)
    aggyear2 = unique(aggdata2[which(aggdata2$Irg_Season=="Not Irrigation Season"),c("Year","Ncount_irg_L","position")])
    aggyear2 = aggyear2[complete.cases(aggyear2),]
    
    par(mfrow = c(1,2))
    
    boxplot(irgl$Load~irgl$Type+irgl$Year, at = positions1, main = "Irrigation Season", xaxt = "n", xlab = "", ylab = "Loading (MPN/day)",ylim = c(0,plotrange[2]),col = boxcolors[2:1], lty = 1, outline = FALSE)
    axis(1, at = yearlab1$position, label = yearlab1$Year)
    legend("topleft",legend = c("Irrigation Season","Not Irrigation Season","TMDL"), pch = c(22,22,22), pt.bg = c(boxcolors[2],boxcolors[3],boxcolors[1]), col = c("black","black", "black"),bty = "n")
    text(aggyear1$position,rep(-10, length(aggyear1$position)), labels = paste("n =",aggyear1$Ncount_irg_L), cex = 0.8)
    
    if(input$viewirgdat){
      points(irgl$position, irgl$Load, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
    
    boxplot(nirgl$Load~nirgl$Type+nirgl$Year, at = positions2, main = "Not Irrigation Season", xaxt = "n", xlab = "", ylim = c(0,plotrange[2]),ylab = "",col = boxcolors[c(3,1)], lty = 1, outline = FALSE)
    axis(1, at = yearlab2$position, label = yearlab2$Year)
    text(aggyear2$position,rep(-10, length(aggyear2$position)), labels = paste("n =",aggyear2$Ncount_irg_L), cex = 0.8)
    
    if(input$viewirgdat){
      points(nirgl$position, nirgl$Load, pch = 22, cex = 1.2, col = "black", bg = linecolors[1])
    }
    
  }
  
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
  
  boxplot(ecoli.loads$Observed_Loading~ecoli.loads$Flow_Cat, col=ggplot2::alpha(boxcolors[2],0.7), at = c(5,25,50,75,95),lty=1, xaxt="n", frame=FALSE, boxwex = 5, add=TRUE)
  legend("topright",legend=c("TMDL", "E.coli Loading"), bty="n", col=c("firebrick3","black"), lty=c(1,NA),lwd=c(2,NA),pch=c(NA,22), pt.bg=c(NA,ggplot2::alpha(boxcolors[2],0.7)), pt.cex=c(NA,2),cex=1)
  
  }
  
    
})

output$LDC_Data <- renderDT(workbook$LDC_Data,
                            rownames = FALSE,selection='none',filter="top",
                            options = list(scrollY = '300px', paging = FALSE, scrollX=TRUE))
}

# Run the application
shinyApp(ui = ui, server = server)

#### UNUSED CODE ####
# out$geom_crit = out$Inputs[out$Inputs$Parameter=="Geometric Mean Criterion","Value"]
# out$max_crit = out$Inputs[out$Inputs$Parameter=="Max Criterion","Value"]
# out$cf = out$Inputs[out$Inputs$Parameter=="Correction Factor","Value"]
# out$mos = out$Inputs[out$Inputs$Parameter=="Margin of Safety","Value"]

# output$Monthly_Geomeans <- renderPlotly({
#   req(selectedmonthdata$seldg)
#   y = selectedmonthdata$seldg
#   y = droplevels(y[order(y$month),])
#   monthmed = tapply(y$E.coli_Geomean, y$month, median)
#   monthmed_pos = rep(max(monthmed)*-0.05, length(monthmed))
#   monthn_count = tapply(y$E.coli_Geomean, y$month, length)
#   month_geomean = tapply(y$E.coli_Geomean, y$month, function(x){exp(mean(log(x)))})
#   perc_red = ifelse(month_geomean>crits$geomcrit,round(perc.red(crits$geomcrit,month_geomean), digits=0),0)
#   # Add criteria
#   geom = list(type = 'line', x0 = -1, x1 = length(monthmed_pos), y0 = crits$geomcrit, y1 = crits$geomcrit, line=list(dash='dot', color = "orange", width=2))
#   max = list(type = 'line', x0 = -1, x1 = length(monthmed_pos), y0 = crits$maxcrit, y1 = crits$maxcrit, line=list(dash='dot', color = "red", width=2))
#   
#   # initiate a line shape object
#   line <- list(
#     type = "line",
#     line = list(color = "green"),
#     xref = "x",
#     yref = "y"
#   )
#   
#   lines <- list()
#   for (i in c(0:(length(monthmed_pos)-1))) {
#     line[["x0"]] <- i-0.25
#     line[["x1"]] <- i + 0.25
#     line[c("y0", "y1")] <- month_geomean[i+1]
#     lines <- c(lines, list(line))
#   }
#   
#   line[["x0"]] <- -1
#   line[["x1"]] <- length(monthmed_pos)
#   line[c("y0", "y1")] <- crits$geomcrit
#   line$line$color = "orange"
#   lines <- c(lines, list(line))
# 
#   line[["x0"]] <- -1
#   line[["x1"]] <- length(monthmed_pos)
#   line[c("y0", "y1")] <- crits$maxcrit
#   line$line$color = "red"
#   lines <- c(lines, list(line))
#   
#   # add = selectedmonthdata$aggseldg
#   # add = droplevels(add[order(add$month),])
#   #barcolors = piratepal(palette="up")
#   if(input$mon_unit_type=="Concentration"){
#     month_c = plot_ly(x =~y$month, y =~y$E.coli_Geomean,  type ="box", boxpoints = "outliers")%>%
#       layout(xaxis = list(title = ""), yaxis = list(title = "E.coli Concentration (MPN/100 mL)"),font = list(family = "Arial, sans-serif"), shapes = lines)%>%
#       add_annotations(x = 0:(length(monthmed_pos)-1), y = monthmed_pos, text = paste("n =",monthn_count), showarrow = FALSE)
#     month_c
#       # # Straight bar plot - concentrations
#     # uplim = max(x$E.coli_Geomean)*1.2
#     # mo_conc.p <- x$E.coli_Geomean
#     # barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$month,ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
#     # legend("topright",legend=c("Geomean Standard", "% Reduction Needed"), bty="n", fill=c("white","white"), border=c("white","white"),lty=c(1,NA),lwd=c(2,NA),cex=1)
#     # box(bty="l")
#     # abline(h=crits$geomcrit, col="black", lwd=2)
#     # ncount = paste0("n=",x$Ncount)
#     # mtext(ncount, side = 1, line = 0, cex=0.8, at = barp)
#     # barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction))
#     # barperc <- barperc[barperc$X3>0,]
#     # if(dim(barperc)[1]>0){
#     #   barperc$X4 <- paste(barperc$X3,"%",sep="")
#     #   text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1)
#     # }
#     # if(input$mon_medplot){
#     #   # Obtain boxplot stats from loading data
#     #   y = selectedmonthdata$seldg
#     #   y = droplevels(y[order(y$month),])
#     # 
#     #   # Get axes right to accommodate boxplot overlay (if checkbox checked)
#     #   uplim1 = quantile(y$E.coli_Geomean,1)
#     #   uplim1 = max(uplim, uplim1)
#     # 
#     #   # Bar plot
#     #   barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans with Quartile Overlay", ylim=c(0, uplim1), names.arg = x$month, ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
#     #   abline(h=crits$geomcrit, col="black", lty=2, lwd=2)
#     #   legend("topright",legend=c("Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,1),fill=c(NA,NA,"white"),border=c("white","white","white"),lty=c(1,2,NA),lwd=c(3,2,NA),cex=1)
#     #   box(bty="l")
#     #   mtext(ncount, side = 1, line = 0, cex = 0.8, at = barp)
#     # 
#     #   # x-axis arguments for boxplot based on barplot placement
#     # 
#     #   boxplot(y$E.coli_Geomean~y$month,
#     #           lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(barcolors[1],0.1), boxwex = 0.7, at=barp[,1], add=TRUE)
#     # }
#   }
#   if(input$mon_unit_type=="Loading"){
#     cols = piratepal(palette="up")
#     # Narrow dataset
#     monthdatl <- workbook$LDC_Data
#     datrange <- monthdatl[monthdatl$ML_Name==input$monthsite&monthdatl$Date>=input$mondatrange[1]&monthdatl$Date<=input$mondatrange[2],c("MLID","ML_Name","Date","TMDL","Observed_Loading")]
#     if(dim(datrange)[1]>0){
#       datrange <- datrange[!is.na(datrange$Observed_Loading),]
#       datstack <- reshape2::melt(data = datrange, id.vars = c("MLID", "ML_Name", "Date"), value.vars=c("TMDL","Observed_Loading"), variable.name = "Meas_Type")
#       datstack$month = lubridate::month(datstack$Date, label=TRUE)
#       datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
#       names(datstack)[names(datstack)=="value"]<-"Loading"
#       x <- aggregate(Loading~month+MLID+ML_Name+Meas_Type, dat=datstack, FUN=function(x){exp(mean(log(x)))})
#       x_ncount <- aggregate(Loading~month+MLID+ML_Name+Meas_Type, dat=datstack, FUN=length)
#       x_ncount <- x_ncount[x_ncount$Meas_Type=="Observed_Loading",]
#       names(x_ncount)[names(x_ncount)=="Loading"]<- "Ncount"
#       x_ncount = x_ncount[order(x_ncount$month),]
#       x = dcast(data=x, month~Meas_Type, value.var="Loading")
#       x = x[order(x$month),]
#       x$Percent_Reduction = ifelse(x$Observed_Loading>x$TMDL,round(perc.red(x$TMDL,x$Observed_Loading), digits=0),0)
#       uplim = max(c(x$Observed_Loading,x$TMDL))*1.2
#       mo_load.p <- x[,names(x)%in%c("Observed_Loading","TMDL")]
# 
#       # Straight bar plots
#       barp <- barplot(t(mo_load.p), beside=T, main = "Monthly E.coli Loading Geomeans",names.arg=x$month, ylim=c(0, uplim), ylab="E.coli Loading (GigaMPN/day)",col=c(cols[1],cols[2]))
#       legend("topright",legend=c("Observed Loading","TMDL", "% Reduction Needed"), bty="n", fill=c(cols[1],cols[2],"white"), border=c("black","black","white"),cex=1)
#       box(bty="l")
#       barps <- barp[1,]
#       barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
#       barperc <- barperc[barperc$V3>0,]
#       if(dim(barperc)[1]>0){
#         barperc$V3 <- paste(barperc$V3,"%",sep="")
#         text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
#       }
#       
#       # Add ncounts
#       ncountpos = colMeans(barp)
#       mtext(c(paste0("n=",x_ncount$Ncount)),side = 1,line = 0,at = ncountpos, cex=0.8)
# 
#       if(input$mon_medplot){
#         # Get axes right to accommodate boxplot overlay (if checkbox checked)
#         uplim1 = quantile(datstack$Loading,1)
#         uplim1 = max(uplim, uplim1)
# 
#         # Bar plot
#         barp <- barplot(t(mo_load.p), beside=T, names.arg = x$month, main = "Monthly E.coli Loading Geomeans with Quartile Overlay", ylim=c(0, uplim1*1.1), ylab="E.coli Loading (GigaMPN/day)",col=c(cols[1],cols[2]))
#         legend("topright",legend=c("Observed Loading","TMDL", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,1),fill=c(cols[1],cols[2],NA,"white"),border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,3,NA),cex=1)
#         box(bty="l")
#         mtext(c(paste0("n=",x_ncount$Ncount)),side = 1,line = 0,at = ncountpos, cex=0.8)
#         # x-axis arguments for boxplot based on barplot placement
#         ax <- c(barp[1,],barp[2,])
#         ax_spots = ax[order(ax)]
# 
#         boxplot(datstack$Loading~datstack$Meas_Type+lubridate::month(datstack$Date),
#                 lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(cols[1],cols[2]),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
#       }
#     }
#   }
# })

# month_c = plot_ly(x =~y$month, y =~y$E.coli_Geomean,  type ="box", name = "Monthly Concentrations",boxpoints = "outliers")%>%
#   #add_trace(x = month_geomean_df$month, y = month_geomean_df$E.coli_Geomean, name = "Monthly Geomean", line = list(color = "gold"))%>%
#   layout(xaxis = list(title = ""), yaxis = list(title = "E.coli Concentration (MPN/100 mL)"),font = list(family = "Arial, sans-serif"), xaxis2 = list(overlaying = "x", showticklabels = FALSE, showline = FALSE))%>%
#   add_trace(x = ~c(0,1), y = ~c(crits$maxcrit, crits$maxcrit), type = "scatter", mode = "lines", name = "Max Crit", xaxis = "x2", line = list(color = "green", dash = "dot"))%>%
#   add_trace(x = ~c(0,1), y = ~c(crits$geomcrit, crits$geomcrit), type = "scatter", mode = "lines", name = "Geomean Crit", xaxis = "x2", line = list(color = "red", dash = "dot")) #%>%
#   #add_annotations(x = 0:(length(monthmed_pos)-1), y = monthmed_pos, text = paste("n =",monthn_count), showarrow = FALSE)%>%
#   #add_annotations(x = perc_red_df$xpos, y = perc_red_df$ypos, text = paste0(perc_red_df$perc_red,"%"), showarrow = FALSE, font = list(size = 9))

#           month_l = plot_ly(x =~monloads_flat$month, y =~monloads_flat$Loading,  color = ~monloads_flat$Load_Type, type ="box", boxpoints = "outliers")%>%
#             layout(boxmode = "group", xaxis = list(title = ""), yaxis = list(title = "E.coli Loading (GigaMPN/day)"),font = list(family = "Arial, sans-serif"), xaxis2 = list(overlaying = "x", showticklabels = FALSE, showline = FALSE)) #%>%
#           #add_trace(x = ~mloadgeomean$month, y = ~mloadgeomean$Loading, color = ~mloadgeomean$Load_Type, xaxis = "x2", line = list(color = "gold"))
#add_trace(x = month_geomean_df$month, y = month_geomean_df$E.coli_Geomean, name = "Monthly Geomean", line = list(color = "gold"))%>%
#add_annotations(x = 0:(length(monthmed_pos)-1), y = monthmed_pos, text = paste("n =",monthn_count), showarrow = FALSE)%>%
#add_annotations(x = perc_red_df$xpos, y = perc_red_df$ypos, text = paste0(perc_red_df$perc_red,"%"), showarrow = FALSE, font = list(size = 9))


# What data to show in table
# observe({
#   req(workbook$Daily_Geomean_Data)
#   if(!is.null(timeseriesdat$x1)){
#     ecoli_ts = timeseriesdat$x
#     flow_ts = timeseriesdat$x1
#     timeseriesdat$tabledata = merge(ecoli_ts, flow_ts, all = TRUE)
#   }else{
#     timeseriesdat$tabledata = timeseriesdat$x
#   }
# })
# 
# output$Time_Data <- renderDT(timeseriesdat$tabledata,
#                              rownames = FALSE,selection='none',filter="top",
#                              options = list(scrollY = '300px', paging = FALSE, scrollX=TRUE))


