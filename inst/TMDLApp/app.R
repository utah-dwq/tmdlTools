################## SHINY APP ##########################################

wb_path <- "C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\Fremont_data_2019-02-21.xlsx"
wb.dat <- openxlsx::loadWorkbook(wb_path)
ecoli.dat <- openxlsx::readWorkbook(wb.dat,sheet="Daily_Geomean_Data",startRow=1)
ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
flow.dat <- openxlsx::readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
loading.dat <- openxlsx::readWorkbook(wb.dat, sheet="LDC_Data")
loading.dat$Date <- as.Date(loading.dat$Date, origin="1899-12-30")
month.dat <- openxlsx::readWorkbook(wb.dat,sheet="Monthly_Data",startRow=1)
rec.dat <- openxlsx::readWorkbook(wb.dat,sheet="Rec_Season_Data",startRow=1)
irg.dat <- openxlsx::readWorkbook(wb.dat,sheet="Irg_Season_Data",startRow=1)
specs <- openxlsx::readWorkbook(wb.dat,sheet="Inputs",startRow=1)

max_crit = specs$Value[specs$Parameter=="Max Criterion"]
geom_crit = specs$Value[specs$Parameter=="Geometric Mean Criterion"]
cf = specs$Value[specs$Parameter=="Correction Factor"]
mos = specs$Value[specs$Parameter=="Margin of Safety"]


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Time Series",
             shinyjs::useShinyjs(),
             plotOutput("Time_Series"),
             hr(),
             h5(strong("Use the site check boxes to select multiple sites to view and edit the date range menu to change the time axis in the plot above. Changing the criterion alters the percent exceedance reported at the top of the plot.")),
             br(),
             fluidRow(
               column(4,
                      selectInput("plottype", label = "Select Plot Type", choices = c("Point","Line"), selected = "Point"),
                      uiOutput("checkbox")),
               column(4,
                      div(
                        id = "date",
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
                      textInput("crit1",label = "Max Criterion",value=max_crit),
                      textInput("crit2",label = "Geomean Criterion",value=geom_crit)
                       ),
             column(4,
                    downloadButton("dwn1",label = "Download Plot")
             ))
             ), 
    tabPanel("Monthly",
             selectInput("site2",
                         label = "Site Name",
                         choices=c(unique(month.dat$ML_Name))),
             uiOutput("unit_type"),
             checkboxInput("medplot", label = "View Medians and Quartiles"),
             h5("Quartiles: The lower whisker of the boxplot represents the lowest value, excluding outliers, while the upper whisker represents the highest value, excluding outliers. The top and bottom edges of the box indicate the 75th and 25th percentiles, respectively. Boxes without whiskers indicate only 2 data points exist, and median lines without a box indicate only 1 data point exists."),
             hr(),
             plotOutput("Monthly_Geomeans", height="700px")),
    tabPanel("Rec/Non-Rec Season",
             selectInput("site3",
                         label = "Site Name",
                         choices=c(unique(rec.dat$ML_Name))),
             uiOutput("unit_type1"),
             checkboxInput("medplot1", label = "View Medians and Quartiles"),
             hr(),
             plotOutput("Rec_Geomeans", height="700px")),
    tabPanel("Irrigation/Non-Irrigation Season",
             selectInput("site4",
                         label = "Site Name",
                         choices=c(unique(irg.dat$ML_Name))),
             uiOutput("unit_type2"),
             checkboxInput("medplot2", label = "View Medians and Quartiles"),
             hr(),
             plotOutput("Irg_Geomeans", height="700px")),
    tabPanel("Load Duration Curves",
             selectInput("site1",
                         label = "Site Name",
                         choices=c(unique(loading.dat$ML_Name))),
             selectInput("pt_type",
                         label = "Data Category",
                         choices=c("Calendar Seasons","Recreation Seasons","Irrigation Seasons")),
             hr(),
             plotOutput("LDC", width="100%", height="700px")
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$checkbox <- renderUI({
    choice <-  unique(ecoli.dat$ML_Name)
    checkboxGroupInput("checkbox","Select Site(s)", choices = choice, selected = choice[1])
    
  })
  
  observeEvent(input$reset_input,{
    reset("date")
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
      subd=c("Loading","Concentration")
    }else{subd=c("Concentration")}
    selectInput("unit_type1","Select Measurement Type", choices = subd, selected = subd[1])
    
  })
  
  output$unit_type2 <- renderUI({
    subdat = irg.dat[irg.dat$ML_Name==input$site4&!is.na(irg.dat$Observed_Loading),"Observed_Loading"]
    if(length(subdat)>0){
      subd=c("Loading","Concentration")
    }else{subd=c("Concentration")}
    selectInput("unit_type2","Select Measurement Type", choices = subd, selected = subd[1])
    
  })

  output$Time_Series <- renderPlot({
    x = ecoli.dat[ecoli.dat$ML_Name %in% input$checkbox,]
    # Get number of sites
    uni.sites <- unique(x$ML_Name)
    colrs <- yarrr::piratepal("basel")
    # Get dates for axis
    min = input$dateRange[1]
    max = input$dateRange[2]
    x <- x[x$Date>min&x$Date<max,]
    # Create an empty plot
    plot(1, type="n", xlab="", ylab="MPN/100 mL", xaxt="n", xlim=c(min, max), ylim=c(0, 2420))
    axis.Date(1, at=seq(min, max, by="6 months"), format="%m-%Y", las=2, cex=0.8)
    abline(h=input$crit2,col="orange", lwd=2)
    abline(h=input$crit1, col="red", lwd=2)
    text(min+10,as.numeric(input$crit1)-100, "Max Crit")
    text(min+150,as.numeric(input$crit2)-100, "Geometric Mean Crit")
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

   output$LDC <- renderPlot({
     req(input$pt_type)
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
       spre <- ecoli.loads[ecoli.loads$CalSeason=="Spring",]
       sume <- ecoli.loads[ecoli.loads$CalSeason=="Summer",]
       fale <- ecoli.loads[ecoli.loads$CalSeason=="Fall",]
       wine <- ecoli.loads[ecoli.loads$CalSeason=="Winter",]
       
       points(spre$Observed_Loading~spre$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
       points(sume$Observed_Loading~sume$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
       points(fale$Observed_Loading~fale$Flow_Percentile, pch=21, col="black", bg=colpal[3], cex=2)
       points(wine$Observed_Loading~wine$Flow_Percentile, pch=21, col="black", bg=colpal[4], cex=2)
       legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Spring", "E.coli Loading - Summer","E.coli Loading - Fall", "E.coli Loading - Winter"), bty="n", col=c("firebrick3","red","black","black","black","black"), lty=c(1,1,NA,NA,NA,NA),lwd=c(2,2,NA,NA,NA,NA),pch=c(NA,NA,21,21,21,21), pt.bg=c(NA,NA,colpal), pt.cex=c(NA,NA,2,2,2,2),cex=1)
     }
     
     if(input$pt_type=="Recreation Seasons"){
       colpal <- colorspace::heat_hcl(12)
       colpal = c(colpal[1],colpal[12])
       rec <- ecoli.loads[ecoli.loads$Rec_Season=="Rec Season",]
       nonrec <- ecoli.loads[ecoli.loads$Rec_Season=="Not Rec Season",]
       
       points(rec$Observed_Loading~rec$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
       points(nonrec$Observed_Loading~nonrec$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
       legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Rec", "E.coli Loading - Non-Rec"), bty="n", col=c("firebrick3","red","black","black"), lty=c(1,1,NA,NA),lwd=c(2,2,NA,NA),pch=c(NA,NA,21,21), pt.bg=c(NA,NA,colpal), pt.cex=c(NA,NA,2,2),cex=1)
       
     }
     
     if(input$pt_type=="Irrigation Seasons"){
       colpal <- colorspace::terrain_hcl(12)
       colpal = c(colpal[1],colpal[8])
       irg <- ecoli.loads[ecoli.loads$Irg_Season=="Irrigation Season",]
       nonirg <- ecoli.loads[ecoli.loads$Irg_Season=="Not Irrigation Season",]
       
       points(irg$Observed_Loading~irg$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=2)
       points(nonirg$Observed_Loading~nonirg$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=2)
       legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Irrigation", "E.coli Loading - No Irrigation"), bty="n", col=c("firebrick3","red","black","black"), lty=c(1,1,NA,NA),lwd=c(2,2,NA,NA),pch=c(NA,NA,21,21), pt.bg=c(NA,NA,colpal), pt.cex=c(NA,NA,2,2),cex=1)
       
     }
     })

     output$Monthly_Geomeans <- renderPlot({
       req(input$unit_type)
       if(input$unit_type=="Concentration"){
         # Obtain boxplot stats from loading data
         y <- ecoli.dat[ecoli.dat$ML_Name==input$site2,c("MLID","ML_Name","Date","E.coli_Geomean")]
         y$Month = lubridate::month(y$Date, label=TRUE, abbr=TRUE)
         y = y[order(y$Month),]

         # Straight bar plot - concentrations
         x <- month.dat[month.dat$ML_Name==input$site2,]
         uplim = max(x$E.coli_Geomean)*1.2
         mo_conc.p <- x$E.coli_Geomean
         barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$month,ylab="E.coli Concentration (MPN/100 mL)",col=colorspace::heat_hcl(12,h=c(0,-100),l=c(75,40),c=c(40,80), power=1))
         abline(h=geom_crit, col="black", lwd=2)
         barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction_C))
         barperc <- barperc[barperc$X3>0,]
         if(dim(barperc)[1]>0){
           barperc$X4 <- paste(barperc$X3,"%",sep="")
           text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1) 
         }
         if(input$medplot){
           # Get axes right to accommodate boxplot overlay (if checkbox checked)
           uplim1 = quantile(y$E.coli_Geomean,1)
           uplim1 = max(uplim, uplim1)
           
           # Bar plot
           barp <- barplot(mo_conc.p, main = "Monthly E.coli Concentration Geomeans with Quartile Overlay", ylim=c(0, uplim1), names.arg = x$month, ylab="E.coli Concentration (MPN/100 mL)",col=colorspace::heat_hcl(12,h=c(0,-100),l=c(75,40),c=c(40,80), power=1))
           abline(h=geom_crit, col="black", lty=2, lwd=2)
           legend("topright",legend=c("Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,1),fill=c(NA,NA,"white"),border=c("white","white","white"),lty=c(1,2,NA),lwd=c(3,2,NA),cex=1)
           box(bty="l")
           
           # x-axis arguments for boxplot based on barplot placement
           
           boxplot(y$E.coli_Geomean~y$Month,
                   lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c("firebrick3"),0.1), boxwex = 0.7, at=barp[,1], add=TRUE)
         }
       }
    if(input$unit_type=="Loading"){
      # Obtain boxplot stats from loading data
      y <- loading.dat[loading.dat$ML_Name==input$site2,c("MLID","ML_Name","Date","Loading_Capacity_MOS","Observed_Loading")]
      y <- y[!is.na(y$Observed_Loading),]
      datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
      names(datstack)[names(datstack)=="value"]<-"Loading"
      datstack$Month = lubridate::month(datstack$Date, label=TRUE, abbr=TRUE)
      datstack = datstack[order(datstack$Month),]
      datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
      
      # Straight bar plots
      x <- month.dat[month.dat$ML_Name==input$site2,]
      x = x[complete.cases(x),]
      rownames(x) <- x$month
      uplim = max(c(x$Observed_Loading,x$Loading_Capacity_MOS))*1.2
      mo_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
      barp <- barplot(t(mo_load.p), beside=T, main = "Monthly E.coli Loading Geomeans", ylim=c(0, uplim), ylab="E.coli Loading (MPN/day)",col=c("firebrick3","dodgerblue3"))
      legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=1)
      box(bty="l")
      barps <- barp[1,]
      barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction_L))
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
        barp <- barplot(t(mo_load.p), beside=T, main = "Monthly E.coli Loading Geomeans with Quartile Overlay", ylim=c(0, uplim1), ylab="E.coli Loading (MPN/day)",col=c("firebrick3","dodgerblue3"))
        legend("topright",legend=c("Observed Loading","Loading Capacity", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,1),fill=c("firebrick3","dodgerblue3",NA,"white"),border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,3,NA),cex=1)
        box(bty="l")
        
        # x-axis arguments for boxplot based on barplot placement
        ax <- c(barp[1,],barp[2,])
        ax_spots = ax[order(ax)]
        
        boxplot(datstack$Loading~datstack$Meas_Type+lubridate::month(datstack$Date),
                lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c("firebrick3", "dodgerblue3"),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
      }}

     })
     
     output$Rec_Geomeans <- renderPlot({
       req(input$unit_type1)
       if(input$unit_type1=="Concentration"){
         # Obtain boxplot stats from loading data
         y <- ecoli.dat[ecoli.dat$ML_Name==input$site2,c("MLID","ML_Name","Rec_Season","E.coli_Geomean")]
         
         # Straight bar plot - concentrations
         x <- rec.dat[rec.dat$ML_Name==input$site2,]
         uplim = max(x$E.coli_Geomean)*1.2
         rec_conc.p <- x$E.coli_Geomean
         barp <- barplot(rec_conc.p, main = "Rec Season E.coli Concentration Geomeans", ylim=c(0, uplim), names.arg = x$Rec_Season,ylab="E.coli Concentration (MPN/100 mL)",col=c("dodgerblue3","firebrick3"))
         abline(h=geom_crit, col="black", lwd=2)
         barperc <- data.frame(cbind(barp,x$E.coli_Geomean, x$Percent_Reduction_C))
         barperc <- barperc[barperc$X3>0,]
         if(dim(barperc)[1]>0){
           barperc$X4 <- paste(barperc$X3,"%",sep="")
           text(barperc$X1,barperc$X2+0.1*mean(barperc$X2),labels=barperc$X4,cex=1) 
         }
         if(input$medplot1){
           # Get axes right to accommodate boxplot overlay (if checkbox checked)
           uplim1 = quantile(y$E.coli_Geomean,1)
           uplim1 = max(uplim, uplim1)
           
           # Bar plot
           barp <- barplot(rec_conc.p, main = "Rec Season E.coli Concentration Geomeans with Quartile Overlay", ylim=c(0, uplim1), names.arg = x$Rec_Season, ylab="E.coli Concentration (MPN/100 mL)",col=c("dodgerblue3","firebrick3"))
           abline(h=geom_crit, col="black", lty=2, lwd=2)
           legend("topright",legend=c("Median", "Geomean Standard","Outliers"), bty="n", pch=c(NA,NA,1),fill=c(NA,NA,"white"),border=c("white","white","white"),lty=c(1,2,NA),lwd=c(3,2,NA),cex=1)
           box(bty="l")
           
           # x-axis arguments for boxplot based on barplot placement
           
           boxplot(y$E.coli_Geomean~y$Rec_Season,
                   lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c("dodgerblue3","firebrick3"),0.1), boxwex = 0.7, at=barp[,1], add=TRUE)
         }
       }
       if(input$unit_type1=="Loading"){
         par(mfrow=c(1,2))
         # Obtain boxplot stats from loading data
         y <- loading.dat[loading.dat$ML_Name==input$site2,c("MLID","ML_Name","Date","Rec_Season","Loading_Capacity_MOS","Observed_Loading")]
         y <- y[!is.na(y$Observed_Loading),]
         datstack <- reshape2::melt(data = y, id.vars = c("MLID", "ML_Name", "Date","Rec_Season"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
         names(datstack)[names(datstack)=="value"]<-"Loading"
         datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
         
         # Straight bar plots
         x <- rec.dat[rec.dat$ML_Name==input$site2,]
         x = x[complete.cases(x),]
         x = x[order(x$Year),]
         uplim = max(c(x$Observed_Loading,x$Loading_Capacity_MOS))*1.2
         rec_load.p <- x[x$Rec_Season=="Rec Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
         rownames(rec_load.p)= rec_load.p$Year
         rec_load.p = rec_load.p[,!names(rec_load.p)%in%("Year")]
         barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim), ylab="E.coli Loading (MPN/day)",col=c("firebrick3","dodgerblue3"))
         legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=1)
         box(bty="l")
         barps <- barp[1,]
         barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Rec Season"]))
         barperc <- barperc[barperc$V3>0,]
         if(dim(barperc)[1]>0){
           barperc$V3 <- paste(barperc$V3,"%",sep="")
           text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
         }
         
         nrec_load.p <- x[x$Rec_Season=="Not Rec Season",names(x)%in%c("Observed_Loading","Loading_Capacity_MOS","Year")]
         rownames(nrec_load.p)= nrec_load.p$Year
         nrec_load.p = nrec_load.p[,!names(nrec_load.p)%in%("Year")]  
         barp <- barplot(t(nrec_load.p), beside=T, names.arg=x$Year[x$Rec_Season=="Not Rec Season"], ylim=c(0, uplim), col=c("firebrick3","dodgerblue3"))
         box(bty="l")
         barps <- barp[1,]
         barperc <- data.frame(cbind(barps,x$Observed_Loading[x$Rec_Season=="Not Rec Season"], x$Percent_Reduction_L[x$Rec_Season=="Not Rec Season"]))
         barperc <- barperc[barperc$V3>0,]
         if(dim(barperc)[1]>0){
           barperc$V3 <- paste(barperc$V3,"%",sep="")
           text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1) 
         }
         
         if(input$medplot1){
           # Get axes right to accommodate boxplot overlay (if checkbox checked)
           uplim1 = quantile(datstack$Loading,1)
           uplim1 = max(uplim, uplim1)
           
           # Bar plot
           par(mfrow=c(1,2))
           barp <- barplot(t(rec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Rec Season"], ylab="E.coli Loading (MPN/day)",col=c("firebrick3","dodgerblue3"))
           legend("topright",legend=c("Observed Loading","Loading Capacity", "Median","Outliers"), bty="n", pch=c(NA,NA,NA,1),fill=c("firebrick3","dodgerblue3",NA,"white"),border=c("black","black","white","white"),lty=c(NA,NA,1,NA),lwd=c(NA,NA,3,NA),cex=1)
           box(bty="l")
           # x-axis arguments for boxplot based on barplot placement
           ax <- c(barp[1,],barp[2,])
           ax_spots = ax[order(ax)]
           
           boxplot(datstack$Loading[datstack$Rec_Season=="Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Rec Season"],
                   lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c("firebrick3", "dodgerblue3"),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
           
           
           barp <- barplot(t(nrec_load.p), beside=T, ylim=c(0, uplim1), names.arg=x$Year[x$Rec_Season=="Not Rec Season"], col=c("firebrick3","dodgerblue3"))
           box(bty="l")
           
           # x-axis arguments for boxplot based on barplot placement
           ax <- c(barp[1,],barp[2,])
           ax_spots = ax[order(ax)]
           
           boxplot(datstack$Loading[datstack$Rec_Season=="Not Rec Season"]~datstack$Meas_Type[datstack$Rec_Season=="Not Rec Season"]+lubridate::year(datstack$Date)[datstack$Rec_Season=="Not Rec Season"],
                   lty=1, xaxt="n", frame=FALSE, col=ggplot2::alpha(c("firebrick3", "dodgerblue3"),0.1), boxwex = 0.7, at=ax_spots, add=TRUE)
           
           
         }
         
       }
       
     }, width = 900, height = 600)
     
    output$Irg_Geomeans <- renderPlot({
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


############# OLD CODE #################

# output$Monthly_Med_Quar <- renderPlot({
#   x <- loading.dat[loading.dat$ML_Name==input$site2,!names(loading.dat)%in%c("Loading_Capacity", "E.coli_Geomean","Flow_Percentile","Flow.cfs", "Season","Exceeds")]
#   y <- x[!is.na(x$Observed_Loading),]
#   datstack <- melt(data = y, id.vars = c("MLID", "ML_Name", "Date"), value.vars=c("Loading_Capacity_MOS","Observed_Loading"), variable.name = "Meas_Type")
#   names(datstack)[names(datstack)=="value"]<-"Loading"
#   datstack$Month = lubridate::month(datstack$Date, label=TRUE, abbr=TRUE)
#   datstack = datstack[order(datstack$Month),]
#   datstack$Meas_Type = factor(datstack$Meas_Type, levels = levels(datstack$Meas_Type)[c(2,1)])
#   uni_months <- unique(as.character(datstack$Month))
#   nummon <- length(uni_months)
#   numbox <- length(uni_months)*2
#   totlen <- 1:(numbox+nummon-1)
#   boxez = totlen[totlen%%3>0]
#   linez = totlen[totlen%%3==0]
#   
#   boxplot(datstack$Loading~datstack$Meas_Type+lubridate::month(datstack$Date), main = "Median and Quartiles by Month and Loading Metric",
#           lty=1, xaxt="n", ylab = "E.coli Loading (MPN/day)", col=c("firebrick3", "dodgerblue3"), 
#           outline=FALSE, boxwex = 0.8, at=boxez)
#   axis(1, at=boxez[c(TRUE,FALSE)]+0.5, labels=c(uni_months))
#   # for(i in 1:length(linez)){
#   #   abline(v=linez[i], lty=2)
#   # }
# })

# tabPanel("View Data",
#          mainPanel(
#            h2("Visualizing data in .xlsx file"),
#            h4("E.coli Data"),
#            DT::dataTableOutput("ecolidat"),
#            hr(),
#            h4("Flow Data"),
#            DT::dataTableOutput("flowdat"),
#            hr(),
#            h4("E.coli + Flow = Loading Data"),
#            DT::dataTableOutput("loaddat")
#          )
#          
# ),

# output$ecolidat <- renderDataTable({datatable(head(ecoli.dat), options = list(dom="t", paging = FALSE))})
# output$flowdat <- renderDataTable({datatable(head(flow.dat), options = list(dom="t", paging = FALSE))})
# output$loaddat <- renderDataTable({datatable(loading.dat, options = list(dom="t", paging = FALSE))})
# 

# tabPanel("Select Site Data",
#          titlePanel("E.coli TMDL Data Analysis Tool"),
#          fileInput("file1", "Select a TMDL workbook file...", multiple=FALSE),
#          textOutput("file_stats")
#          #selectInput("sheet1",label="Select sheet to view...",choices=)
#          #uiOutput("worksheets")
#          #tableOutput("see_dat")
#          ),

# # Function to get file path of uploaded file
# datfile <- reactive({
#   if (!is.null(input$file1)) {
#     wb.dat <- loadWorkbook(input$file1$datapath)
#     return(wb.dat)
#   } else {
#     return(NULL)
#   }
# })

# if(!is.null(datfile)){
#   ecoli.dat <- readWorkbook(datfile,sheet="Ecoli_data",startRow=1)
#   ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
#   flow.dat <- readWorkbook(datfile, sheet="Flow_data", startRow=1)
#   flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
#   loading.dat <- readWorkbook(datfile, sheet="LDC_Data")
#   loading.dat$Date <- as.Date(loading.dat$Date, origin="1899-12-30")
#   
#   unq_mlids = unique(ecoli.dat$MLID)
#   unq_mlns = unique(ecoli.dat$ML_Name)
#   
# }
# 
# output$file_stats <- renderText({
#   h2("File path chosen:")
#   p(input$file1$datapath)
#   br()
#   h2("Unique MLIDS:")
#   p()
# })
