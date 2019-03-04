################## SHINY APP ##########################################
require(openxlsx)
require(lubridate)
require(DT)
require(shinyjs)
require(yarrr)
require(colorspace)
require(reshape2)
require(markdown)
wb_path <- "C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\NFVR_data_EH_2019-02-25.xlsx"
wb.dat <- openxlsx::loadWorkbook(wb_path)
ecoli.dat <- openxlsx::readWorkbook(wb.dat,sheet="Daily_Geomean_Data",startRow=1)
ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
if("Flow_data"%in%wb.dat$sheet_names){
  flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
  flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30") 
}
if("LDC_Data"%in%wb.dat$sheet_names){
  loading.dat <- openxlsx::readWorkbook(wb.dat, sheet="LDC_Data")
  loading.dat$Date <- as.Date(loading.dat$Date, origin="1899-12-30")
}else{loading.dat=NULL}
month.dat <- openxlsx::readWorkbook(wb.dat,sheet="Monthly_Data",startRow=1)
rec.dat <- openxlsx::readWorkbook(wb.dat,sheet="Rec_Season_Data",startRow=1)
irg.dat <- openxlsx::readWorkbook(wb.dat,sheet="Irg_Season_Data",startRow=1)
specs <- openxlsx::readWorkbook(wb.dat,sheet="Inputs",startRow=1)

max_crit = specs$Value[specs$Parameter=="Max Criterion"]
geom_crit = specs$Value[specs$Parameter=="Geometric Mean Criterion"]
cf = specs$Value[specs$Parameter=="Correction Factor"]
mos = specs$Value[specs$Parameter=="Margin of Safety"]

perc.red <- function(x,y){100-x/y*100}

# Define UI for application that draws a histogram
ui <- fluidPage(title="E.coli Data Explorer",
                titlePanel(title=div(img(width="8%",height="8%",src="dwq_logo_small.png"), em("Escherichia coli"),"Data Visualization Tool")),
                tabsetPanel(id="all_the_things",
                            tabPanel("Time Series",
                                     shinyjs::useShinyjs(),
                                     h3("Bacterial Concentrations Over Time by Site"),
                                     sidebarPanel(selectInput("plottype", label = "Select Plot Type", choices = c("Point","Line"), selected = "Point"),
                                                  div(id = "date",
                                                      dateRangeInput("dateRange",
                                                                     label="Date Range",
                                                                     start=min(ecoli.dat$Date),
                                                                     end=max(ecoli.dat$Date),
                                                                     min=min(ecoli.dat$Date),
                                                                     max=max(ecoli.dat$Date)
                                                      )),
                                                  actionButton("reset_input","Reset Date Range"),
                                                  br(),
                                                  br(),
                                                  uiOutput("checkbox"),
                                                  textInput("crit1",label = "Max Criterion",value=max_crit),
                                                  textInput("crit2",label = "Geomean Criterion",value=geom_crit), width=3),
                                     mainPanel(tabsetPanel(id="timeseries",
                                                           tabPanel("Plot",
                                                                    plotOutput("Time_Series")),
                                                           tabPanel("Data",
                                                                    DT::dataTableOutput("Time_Data", height = 500))))),
                            
                            tabPanel("Monthly",
                                     h3("Bacterial Concentrations/Loadings by Month"),
                                     selectInput("site2",
                                                 label = "Site Name",
                                                 choices=c(unique(month.dat$ML_Name))),
                                     div(id="date1",uiOutput("dateRange1")),
                                     actionButton("reset_input1","Reset Date Range"),
                                     br(),
                                     br(),
                                     uiOutput("unit_type"),
                                     checkboxInput("medplot", label = strong("View Medians and Quartiles")),
                                     hr(),
                                     plotOutput("Monthly_Geomeans", height="700px")),
                            tabPanel("Rec/Non-Rec Season",
                                     h3("Bacterial Concentrations/Loadings in Recreation/Non-Recreation Seasons"),
                                     selectInput("site3",
                                                 label = "Site Name",
                                                 choices=c(unique(rec.dat$ML_Name))),
                                     uiOutput("unit_type1"),
                                     checkboxInput("medplot1", label = strong("View Medians and Quartiles")),
                                     hr(),
                                     plotOutput("Rec_Geomeans", height="700px")),
                            tabPanel("Irrigation/Non-Irrigation Season",
                                     h3("Bacterial Concentrations/Loadings in Irrigation/Non-Irrigation Seasons"),
                                     selectInput("site4",
                                                 label = "Site Name",
                                                 choices=c(unique(irg.dat$ML_Name))),
                                     uiOutput("unit_type2"),
                                     checkboxInput("medplot2", label = strong("View Medians and Quartiles")),
                                     hr(),
                                     plotOutput("Irg_Geomeans", height="700px")),
                            tabPanel("User Guide",
                                     includeMarkdown("C:\\Users\\ehinman\\Documents\\GitHub\\tmdlTools\\user_guide\\user_guide.Rmd")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ############## UI RELATED ####################
  observe({
    if(!is.null(loading.dat)){
      insertTab(inputId="all_the_things",
                tabPanel("Load Duration Curves",
                         h3("Bacterial Loadings Across Flow Regimes"),
                         selectInput("site1",
                                     label = "Site Name",
                                     choices=c(unique(loading.dat$ML_Name))),
                         selectInput("pt_type",
                                     label = "Data Category",
                                     choices=c("Calendar Seasons","Recreation Seasons","Irrigation Seasons")),
                         hr(),
                         plotOutput("LDC", width="100%", height="700px")
                ), target="User Guide")
    }
  })
  
  observeEvent(input$reset_input,{
    reset("date")})
  observeEvent(input$reset_input1,{
    reset("date1")})
  
  output$checkbox <- renderUI({
    choice <-  unique(ecoli.dat$ML_Name)
    checkboxGroupInput("checkbox","Select Site(s)", choices = choice, selected = choice[1])
    
  })
  
  output$unit_type <- renderUI({
    subdat = month.dat[month.dat$ML_Name==input$site2&!is.na(month.dat$Observed_Loading),"Observed_Loading"]
    if(length(subdat)>0){
      subd=c("Concentration","Loading")
    }else{subd=c("Concentration")}
    selectInput("unit_type","Select Measurement Type", choices = subd, selected = subd[1])
  })
  
  output$unit_type1 <- renderUI({
    subdat = rec.dat[rec.dat$ML_Name==input$site3&!is.na(rec.dat$Observed_Loading),"Observed_Loading"]
    if(length(subdat)>0){
      subd=c("Concentration","Loading")
    }else{subd=c("Concentration")}
    selectInput("unit_type1","Select Measurement Type", choices = subd, selected = subd[1])
  })
  
  output$unit_type2 <- renderUI({
    subdat = irg.dat[irg.dat$ML_Name==input$site4&!is.na(irg.dat$Observed_Loading),"Observed_Loading"]
    if(length(subdat)>0){
      subd=c("Concentration","Loading")
    }else{subd=c("Concentration")}
    selectInput("unit_type2","Select Measurement Type", choices = subd, selected = subd[1])
  })
  #
  ######################## OUTPUT RELATED ############################
  
  ################ TIME SERIES #####################  
  timeseriesdat <- reactiveValues()
  
  observe({
    x = ecoli.dat[ecoli.dat$ML_Name %in% input$checkbox,]
    timeseriesdat$min = input$dateRange[1]
    timeseriesdat$max = input$dateRange[2]
    timeseriesdat$x <- x[x$Date>timeseriesdat$min&x$Date<timeseriesdat$max,]
  })
  
  output$Time_Series <- renderPlot({
    req(input$checkbox)
    req(input$dateRange[1],input$dateRange[2])
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
                               options = list(dom="ft", pageLength = 1000, lengthChange = FALSE, scrollY = "800px"))
  ################ LDC #####################
  
  output$LDC <- renderPlot({
    req(input$pt_type)
    req(input$site1)
    x <- loading.dat[loading.dat$ML_Name==input$site1,]
    flow.plot <- x[order(x$Flow_Percentile),]
    
    # Pull out observed loadings (E.coli data)
    ecoli.loads <- x[!is.na(x$E.coli_Geomean),]
    
    plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (MPN/day)", xlim=c(0, 100), ylim=c(0,max(c(ecoli.loads$Observed_Loading, ecoli.loads$Loading_Capacity))), main=paste("Load Duration Curve:",x$ML_Name[1]))
    abline(v=10, lty=2)
    abline(v=40, lty=2)
    abline(v=60, lty=2)
    abline(v=90, lty=2)
    text(5, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"High \n Flows")
    text(25, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Moist \n Conditions")
    text(50, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Mid-Range \n Flows")
    text(75, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Dry \n Conditions")
    text(95, max(ecoli.loads$Observed_Loading)-.3*max(ecoli.loads$Observed_Loading),"Low \n Flows")
    lines(flow.plot$Loading_Capacity~flow.plot$Flow_Percentile, type="l", col="firebrick3", lwd=2)
    lines(flow.plot$Loading_Capacity_MOS~flow.plot$Flow_Percentile, col="red", lwd=2)
    
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
      legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Winter", "E.coli Loading - Spring", "E.coli Loading - Summer","E.coli Loading - Fall"), bty="n", col=c("firebrick3","red","black","black","black","black"), lty=c(1,1,NA,NA,NA,NA),lwd=c(2,2,NA,NA,NA,NA),pch=c(NA,NA,21,21,21,21), pt.bg=c(NA,NA,colpal[4],colpal[3],colpal[2],colpal[1]), pt.cex=c(NA,NA,2,2,2,2),cex=1)
    }
    
    if(input$pt_type=="Recreation Seasons"){
      colpal <- colorspace::rainbow_hcl(2)
      rec <- ecoli.loads[ecoli.loads$Rec_Season=="Rec Season",]
      nonrec <- ecoli.loads[ecoli.loads$Rec_Season=="Not Rec Season",]
      
      points(rec$Observed_Loading~rec$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
      points(nonrec$Observed_Loading~nonrec$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
      legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Rec", "E.coli Loading - Non-Rec"), bty="n", col=c("firebrick3","red","black","black"), lty=c(1,1,NA,NA),lwd=c(2,2,NA,NA),pch=c(NA,NA,21,21), pt.bg=c(NA,NA,colpal), pt.cex=c(NA,NA,2,2),cex=1)
      
    }
    
    if(input$pt_type=="Irrigation Seasons"){
      colpal <- colorspace::terrain_hcl(2)
      irg <- ecoli.loads[ecoli.loads$Irg_Season=="Irrigation Season",]
      nonirg <- ecoli.loads[ecoli.loads$Irg_Season=="Not Irrigation Season",]
      
      points(irg$Observed_Loading~irg$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
      points(nonirg$Observed_Loading~nonirg$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
      legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Irrigation", "E.coli Loading - No Irrigation"), bty="n", col=c("firebrick3","red","black","black"), lty=c(1,1,NA,NA),lwd=c(2,2,NA,NA),pch=c(NA,NA,21,21), pt.bg=c(NA,NA,colpal), pt.cex=c(NA,NA,2,2),cex=1)
      
    }
  })
  
  output$dateRange1 <- renderUI({
    mondat = ecoli.dat[ecoli.dat$ML_Name==input$site2,]
    maxd = max(mondat$Date)
    mind = min(mondat$Date)
    dateRangeInput("dateRange1",
                   label="Date Range",
                   start=mind,
                   end=maxd,
                   min=mind,
                   max=maxd)
  })
  
  ################ MONTHLY #####################   
  
  output$Monthly_Geomeans <- renderPlot({
    req(input$unit_type)
    barcolors = piratepal(palette="up")
    if(input$unit_type=="Concentration"){
      # Straight bar plot - concentrations
      datrange <- ecoli.dat[ecoli.dat$ML_Name==input$site2&ecoli.dat$Date>input$dateRange1[1]&ecoli.dat$Date<input$dateRange1[2],]
      datrange$month = lubridate::month(datrange$Date, label=TRUE)
      x <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=datrange, FUN=function(x){exp(mean(log(x)))})
      x$Percent_Reduction <- ifelse(x$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,x$E.coli_Geomean), digits=0),0)
      uplim = max(x$E.coli_Geomean)*1.2
      mo_conc.p <- x$E.coli_Geomean
      barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$month,ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
      legend("topright",legend=c("Geomean Standard", "% Reduction Needed"), bty="n", fill=c("white","white"), border=c("white","white"),lty=c(1,NA),lwd=c(2,NA),cex=1)
      box(bty="l")
      abline(h=geom_crit, col="black", lwd=2)
      barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction))
      barperc <- barperc[barperc$X3>0,]
      if(dim(barperc)[1]>0){
        barperc$X4 <- paste(barperc$X3,"%",sep="")
        text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1) 
      }
      if(input$medplot){
        # Obtain boxplot stats from loading data
        y = droplevels(datrange[order(datrange$month),])
        
        # Get axes right to accommodate boxplot overlay (if checkbox checked)
        uplim1 = quantile(y$E.coli_Geomean,1)
        uplim1 = max(uplim, uplim1)
        
        # Bar plot
        barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans with Quartile Overlay", ylim=c(0, uplim1), names.arg = x$month, ylab="E.coli Concentration (MPN/100 mL)",col=barcolors[1])
        abline(h=geom_crit, col="black", lty=2, lwd=2)
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
      datrange <- loading.dat[loading.dat$ML_Name==input$site2&loading.dat$Date>input$dateRange1[1]&loading.dat$Date<input$dateRange1[2],c("MLID","ML_Name","Date","Loading_Capacity_MOS","Observed_Loading")]
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
      }}
    
  })
  
  ################ REC SEASON #####################   
  
  output$Rec_Geomeans <- renderPlot({
    # Require site name input before drawing plot
    req(input$unit_type1)
    # Color schemes (unless otherwise defined)
    legcols = colorspace::rainbow_hcl(2) # legend colors--never redefined.
    colucols = colorspace::rainbow_hcl(2) # bars--redefined if only one category exists
    ### CONCENTRATION PLOTS ###
    if(input$unit_type1=="Concentration"){
      # Obtain data from rec.dat
      x <- rec.dat[rec.dat$ML_Name==input$site3,]
      x$Rec_Season[x$Rec_Season=="Not Rec Season"] = paste0("z","NotRec") # ensures barplots are ordered rec season-->not rec season (alphabetical issue)
      x = x[order(x$Year),]
      # Set upper y limit for plot based on max geomean
      uplim = max(x$E.coli_Geomean)*1.2
      # Create columns for "Rec Season" and "Not Rec Season" to separate into different bar plots.
      recstack <- reshape2::dcast(data = x, Year~Rec_Season,value.var = "E.coli_Geomean")
      recstack1 = recstack[,!names(recstack)%in%"Year"]
      # Check to see if both rec and non rec seasons represented
      present = c("Rec Season", "zNotRec")%in%colnames(recstack)
      if(any(present==FALSE)){ # if rec or non rec are missing...
        buddies=FALSE # beside becomes false
        colucols = ifelse("Rec Season"%in%colnames(recstack),colucols[1],colucols[2]) # restrict colors to the one corresponding to the present category
      }else{
        buddies=TRUE # beside becomes true
      }
      # The actual plot...
      rec_conc <- barplot(t(recstack1), beside=buddies, names.arg = recstack$Year, main="E.coli Geomeans by Year",las=2, ylim=c(0, uplim*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
      legend("topright",legend=c("Rec Season", "Not Rec Season","Geomean Standard","% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2], NA,NA), border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,2,NA),cex=1)
      box(bty="l")
      abline(h=geom_crit, col="black", lwd=2)
      
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
      if(input$medplot1){
        # Obtain boxplot stats from geomean data
        y <- ecoli.dat[ecoli.dat$ML_Name==input$site3,c("MLID","Date","ML_Name","Rec_Season","E.coli_Geomean")]
        y$Year = lubridate::year(y$Date)
        y$Rec_Season[y$Rec_Season=="Not Rec Season"] = paste0("z","NotRec")
        
        # Update upper y axis limit if outliers are beyond max barplot height (usually are)
        uplim1 = quantile(y$E.coli_Geomean,1)
        uplim1 = max(uplim, uplim1)
        
        # Create original bar plot with different legend
        barplot(t(recstack1), beside=buddies, names.arg = recstack$Year, main="E.coli Geomeans by Year",las=2, ylim=c(0, uplim1*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
        abline(h=geom_crit, col="black", lty=2, lwd=2)
        legend("topright",legend=c("Rec Season","Not Rec Season","Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(legcols[1],legcols[2],NA,NA,"white"),border=c("black","black","white","white","white"),lty=c(NA,NA,1,2,NA),lwd=c(NA,NA,3,2,NA),cex=1)
        box(bty="l")
        
        # x-axis arguments for boxplot based on barplot placement
        boxat = recperc[,"perc_at"]
        boxplot(y$E.coli_Geomean~y$Rec_Season+y$Year,
                lty=1, xaxt="n",yaxt="n", frame=FALSE, col=ggplot2::alpha(colucols,0.1), boxwex = 0.7, at=boxat, add=TRUE)
      }
    }
    ### LOADING PLOTS ###
    if(input$unit_type1=="Loading"){
      # Define loading capacity color
      loadcol = colorspace::rainbow_hcl(3)[3]
      # Filter to data needed to produce plots
      x <- rec.dat[rec.dat$ML_Name==input$site3,]
      x = x[complete.cases(x),]
      x = x[order(x$Year),]
      # Determine if rec/non rec represented
      uni = unique(x$Rec_Season)
      uplim = max(c(x$Observed_Loading,x$Loading_Capacity_MOS))*1.2
      if(length(uni)>1){ # if both represented...
        par(mfrow=c(1,2)) # create two plot panes...
        # Separate into rec and non rec
        # Rec data
        rec_load.p <- x[x$Rec_Season=="Rec Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
        rownames(rec_load.p)= rec_load.p$Year
        rec_load.p = rec_load.p[,!names(rec_load.p)%in%("Year")]
        # Rec barplot
        barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim), main="Rec Season",ylab="E.coli Loading (MPN/day)",col=c(colucols[1],loadcol))
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Rec Season"]))
        barperc <- barperc[barperc$V3>0,]
        # Plot percent reduction needed text, if applicable.
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
        }
        
        # Non-Rec data, same process
        nrec_load.p <- x[x$Rec_Season=="Not Rec Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
        rownames(nrec_load.p)= nrec_load.p$Year
        nrec_load.p = nrec_load.p[,!names(nrec_load.p)%in%("Year")]  
        barp <- barplot(t(nrec_load.p), beside=T, names.arg=x$Year[x$Rec_Season=="Not Rec Season"], ylim=c(0, uplim), main="Not Rec Season",col=c(colucols[2],loadcol))
        legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","Loading Capacity", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Not Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Not Rec Season"]))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
        }}else{ # If only one category represented...
          par(mfrow=c(1,1)) # only make one plot
          colucol = ifelse(uni=="Rec Season",colucols[1],colucols[2]) # redefine bar color
          rec_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
          rownames(rec_load.p)= rec_load.p$Year
          rec_load.p = rec_load.p[,!names(rec_load.p)%in%("Year")]
          # Bar plot singular - same as rec 
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim), main=uni,ylab="E.coli Loading (MPN/day)",col=c(colucol,loadcol))
          legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","Loading Capacity", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
          box(bty="l")
          barps <- barp[1,]
          barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction_L))
          barperc <- barperc[barperc$V3>0,]
          if(dim(barperc)[1]>0){
            barperc$V3 <- paste(barperc$V3,"%",sep="")
            text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
          }
        }
      ## LOADING BOXPLOTS ##
      if(input$medplot1){
        # Obtain boxplot stats from loading data
        y <- loading.dat[loading.dat$ML_Name==input$site3,c("MLID","ML_Name","Date","Rec_Season","Loading_Capacity_MOS","Observed_Loading")]
        y <- y[!is.na(y$Observed_Loading),]
        datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date","Rec_Season"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
        names(datstack)[names(datstack)=="value"]<-"Loading"
        datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
        
        # Update upper y axis limit of outliers are beyond max barplot height
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)
        
        # Create OG barplot with new legend.
        if(length(uni)>1){# determine whether one or two plot panels needed...
          par(mfrow=c(1,2))
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Rec Season"], main="Rec Season",ylab="E.coli Loading (MPN/day)",col=c(colucols[1],loadcol))
          box(bty="l")
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading[datstack$Rec_Season=="Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Rec Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[1], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
          
          
          barp <- barplot(t(nrec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Not Rec Season"], main="Not Rec Season",col=c(colucols[2],loadcol))
          legend("topright",legend=c("Observed Loading - Rec","Observed Loading - Not Rec","Loading Capacity", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(colucols[1],colucols[2],loadcol,NA,"white"),border=c("black","black","black","white","white"),lty=c(NA,NA,NA,1,NA),lwd=c(NA,NA,NA,3,NA),cex=1)
          box(bty="l")
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading[datstack$Rec_Season=="Not Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Not Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Not Rec Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[2], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }else{
          par(mfrow=c(1,1))
          barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year, main=uni,ylab="E.coli Loading (MPN/day)",col=c(colucol,loadcol))
          box(bty="l")
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading~datstack$Meas_Type+lubridate::year(datstack$Date),
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucol, loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }
        
      }
      
    }
    
  })
  
  ################ IRG SEASON #####################
  
  output$Irg_Geomeans <- renderPlot({
    req(input$unit_type2)
    
    # Color schemes (unless otherwise defined)
    legcols = colorspace::terrain_hcl(2)
    colucols = colorspace::terrain_hcl(2)
    
    ### CONCENTRATION PLOTS ###
    if(input$unit_type2=="Concentration"){
      # Obtain data from irg.dat
      x <- irg.dat[irg.dat$ML_Name==input$site4,]
      x = x[order(x$Year),]
      # Define upper limit of the y-axis based on maximum ecoli geomean
      uplim = max(x$E.coli_Geomean)*1.2
      # Create columns for "Irrigation Season" and "Not Irrigation Season"
      irgstack <- reshape2::dcast(data = x, Year~Irg_Season,value.var = "E.coli_Geomean")
      irgstack1 = irgstack[,!names(irgstack)%in%"Year"]
      # Check to see if both irg and not irg represented
      present = c("Irrigation Season", "Not Irrigation Season")%in%colnames(irgstack)
      if(any(present==FALSE)){ # if rec or non rec are missing...
        buddies=FALSE # beside becomes false
        colucols = ifelse("Irrigation Season"%in%colnames(irgstack),colucols[1],colucols[2]) # restrict colors to the one corresponding to the present category
      }else{
        buddies=TRUE # beside becomes true
      }
      # The actual plot...
      irg_conc <- barplot(t(irgstack1), beside=buddies, names.arg = irgstack$Year, main="E.coli Geomeans by Year",las=2, ylim=c(0, uplim*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
      legend("topright",legend=c("Irrigation Season", "Not Irrigation Season","Geomean Standard","% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2], NA,NA), border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,2,NA),cex=1)
      box(bty="l")
      abline(h=geom_crit, col="black", lwd=2)
      
      # Label bars that exceed
      # Get percent reductions
      perc_lab <- reshape2::dcast(data = x, Year~Irg_Season,value.var = "Percent_Reduction_C")
      perc_labs = melt(perc_lab, id.vars = c("Year"), value.vars = c("Not Irrigation Season", "Irrigation Season"))
      perc_labs = perc_labs[order(perc_labs$Year),]
      
      # Get height of bars
      perc_y = melt(irgstack, id.vars = c("Year"), value.vars = c("Not Irrigation Season", "Irrigation Season"), value.name = "value1")
      perc_y = perc_y[order(perc_y$Year),]
      
      # Merge percent reductions and height at which they should be reported
      percs <- merge(perc_labs,perc_y, all=TRUE) 
      percs = percs[order(percs$Year),]
      
      # Get x pos of bars
      if(any(present==FALSE)){
        perc_at = irg_conc # if irg or not irg are missing, that means that irg_conc only has one set of axis positions
      }else{
        perc_at = c(irg_conc[1,],irg_conc[2,]) # if both present, there are two rows of numbers
        perc_at = perc_at[order(perc_at)]
      }
      irgperc <- data.frame(perc_at,percs) # meld data with x axis position
      irgperc1 <- irgperc[irgperc$value>0&!is.na(irgperc$value),]
      
      # Plot text of percent reduction on plot
      if(dim(irgperc1)[1]>0){
        irgperc1$percn <- paste(irgperc1$value,"%",sep="")
        text(irgperc1$perc_at,irgperc1$value1+0.1*mean(irgperc1$value1),labels=irgperc1$percn,cex=1) 
      }
      ## CONC BOXPLOTS ##
      if(input$medplot2){
        # Obtain boxplot stats from geomean data
        y <- ecoli.dat[ecoli.dat$ML_Name==input$site4,c("MLID","Date","ML_Name","Irg_Season","E.coli_Geomean")]
        y$Year = lubridate::year(y$Date)
        
        # Update upper y axis limit if outliers are beyond max barplot height
        uplim1 = quantile(y$E.coli_Geomean,1)
        uplim1 = max(uplim, uplim1)
        
        # Create original barplot with different legend
        barplot(t(irgstack1), beside=buddies, names.arg = irgstack$Year, main="E.coli Geomeans by Year",las=2, ylim=c(0, uplim1*1.1), ylab="E.coli Concentration (MPN/100 mL)",col=colucols)
        abline(h=geom_crit, col="black", lty=2, lwd=2)
        legend("topright",legend=c("Irrigation Season","Not Irrigation Season","Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(legcols[1],legcols[2],NA,NA,"white"),border=c("black","black","white","white","white"),lty=c(NA,NA,1,2,NA),lwd=c(NA,NA,3,2,NA),cex=1)
        box(bty="l")
        
        # x-axis arguments for boxplot based on barplot placement
        boxat = irgperc[,"perc_at"]
        boxplot(y$E.coli_Geomean~y$Irg_Season+y$Year,
                lty=1, xaxt="n",yaxt="n", frame=FALSE, col=ggplot2::alpha(colucols,0.1), boxwex = 0.7, at=boxat, add=TRUE)
      }
    }
    ### LOADING PLOTS ###
    if(input$unit_type2=="Loading"){
      # Define loading capacity color
      loadcol = colorspace::terrain_hcl(12)[8]
      
      # Filter to data needed to produce plots
      x <- irg.dat[irg.dat$ML_Name==input$site4,]
      x = x[complete.cases(x),]
      x = x[order(x$Year),]
      # Determine if rec/non rec represented
      uni = unique(x$Irg_Season)
      uplim = max(c(x$Observed_Loading,x$Loading_Capacity_MOS))*1.2
      if(length(uni)>1){
        par(mfrow=c(1,2))
        irg_load.p <- x[x$Irg_Season=="Irrigation Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
        rownames(irg_load.p)= irg_load.p$Year
        irg_load.p = irg_load.p[,!names(irg_load.p)%in%("Year")]
        barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim), main="Irrigation Season",ylab="E.coli Loading (MPN/day)",col=c(colucols[1],loadcol))
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Irg_Season=="Irrigation Season"], x$Percent_Reduction_L[x$Irg_Season=="Irrigation Season"]))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
        }
        
        nirg_load.p <- x[x$Irg_Season=="Not Irrigation Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
        rownames(nirg_load.p)= nirg_load.p$Year
        nirg_load.p = nirg_load.p[,!names(nirg_load.p)%in%("Year")]  
        barp <- barplot(t(nirg_load.p), beside=T, names.arg=x$Year[x$Irg_Season=="Not Irrigation Season"], ylim=c(0, uplim), main="Not Irrigation Season",col=c(colucols[2],loadcol))
        legend("topright",legend=c("Observed Loading - Irg","Observed Loading - Not Irg","Loading Capacity", "% Reduction Needed"), bty="n", fill=c(colucols[1],colucols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Irg_Season=="Not Irrigation Season"], x$Percent_Reduction_L[x$Irg_Season=="Not Irrigation Season"]))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
        }
      }else{
        par(mfrow=c(1,1))
        colucol = ifelse(uni=="Irrigation Season",legcols[1],legcols[2])
        irg_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
        rownames(irg_load.p)= irg_load.p$Year
        irg_load.p = irg_load.p[,!names(irg_load.p)%in%("Year")]
        barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim), main=uni,names.arg=x$Year, ylab="E.coli Loading (MPN/day)",col=c(colucol,loadcol))
        legend("topright",legend=c("Observed Loading - Irg","Observed Loading - Not Irg","Loading Capacity", "% Reduction Needed"), bty="n", fill=c(legcols[1],legcols[2],loadcol,"white"), border=c("black","black","black","white"),cex=1)
        box(bty="l")
        barps <- barp[1,]
        barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction_L))
        barperc <- barperc[barperc$V3>0,]
        if(dim(barperc)[1]>0){
          barperc$V3 <- paste(barperc$V3,"%",sep="")
          text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
        } 
      }
      
      if(input$medplot2){
        # Obtain boxplot stats from loading data
        y <- loading.dat[loading.dat$ML_Name==input$site4,c("MLID","ML_Name","Date","Irg_Season","Loading_Capacity_MOS","Observed_Loading")]
        y <- y[!is.na(y$Observed_Loading),]
        datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date","Irg_Season"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
        names(datstack)[names(datstack)=="value"]<-"Loading"
        datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
        
        # Get axes right to accommodate boxplot overlay (if checkbox checked)
        uplim1 = quantile(datstack$Loading,1)
        uplim1 = max(uplim, uplim1)
        
        # Bar plot
        if(length(uni)>1){
          par(mfrow=c(1,2))
          barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Irg_Season=="Irrigation Season"], main="Irrigation Season",ylab="E.coli Loading (MPN/day)",col=c(colucols[1],loadcol))
          box(bty="l")
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading[datstack$Irg_Season=="Irrigation Season"]~datstack$Meas_Type[datstack$Irg_Season=="Irrigation Season"]+lubridate::year(datstack$Date)[datstack$Irg_Season=="Irrigation Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[1], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
          
          
          barp <- barplot(t(nirg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Irg_Season=="Not Irrigation Season"], main="Not Irrigation Season",col=c(colucols[2],loadcol))
          legend("topright",legend=c("Observed Loading - Irg","Observed Loading - Not Irg","Loading Capacity", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,NA,1),fill=c(colucols[1],colucols[2],loadcol,NA,"white"),border=c("black","black","black","white","white"),lty=c(NA,NA,NA,1,NA),lwd=c(NA,NA,NA,3,NA),cex=1)
          box(bty="l")
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading[datstack$Irg_Season=="Not Irrigation Season"]~datstack$Meas_Type[datstack$Irg_Season=="Not Irrigation Season"]+lubridate::year(datstack$Date)[datstack$Irg_Season=="Not Irrigation Season"],
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucols[2], loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
        }else{
          par(mfrow=c(1,1))
          barp <- barplot(t(irg_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year, main=uni,ylab="E.coli Loading (MPN/day)",col=c(colucol,loadcol))
          box(bty="l")
          
          # x-axis arguments for boxplot based on barplot placement
          ax <- c(barp[1,],barp[2,])
          ax_spots = ax[order(ax)]
          
          boxplot(datstack$Loading~datstack$Meas_Type+lubridate::year(datstack$Date),
                  lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c(colucol, loadcol),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
          
        }
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

