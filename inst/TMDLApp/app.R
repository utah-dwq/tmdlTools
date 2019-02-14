################## SHINY APP ##########################################

#wb_path_new <- "NFVR_data_EH_2019-02-07.xlsx"
wb.dat <- loadWorkbook(wb_path)
ecoli.dat <- readWorkbook(wb.dat,sheet="Daily_Geomean_Data",startRow=1)
ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
loading.dat <- readWorkbook(wb.dat, sheet="LDC_Data")
loading.dat$Date <- as.Date(loading.dat$Date, origin="1899-12-30")
month.dat <- readWorkbook(wb.dat,sheet="Monthly_Data",startRow=1)
rec.dat <- readWorkbook(wb.dat,sheet="Rec_Season_Data",startRow=1)
specs <- readWorkbook(wb.dat,sheet="Inputs",startRow=1)

max_crit = specs$Value[specs$Parameter=="Max Criterion"]
geom_crit = specs$Value[specs$Parameter=="Geometric Mean Criterion"]
cf = specs$Value[specs$Parameter=="Correction Factor"]
mos = specs$Value[specs$Parameter=="Margin of Safety"]


# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("View Data",
             mainPanel(
               h2("Visualizing data in .xlsx file"),
               h4("E.coli Data"),
               DT::dataTableOutput("ecolidat"),
               hr(),
               h4("Flow Data"),
               DT::dataTableOutput("flowdat"),
               hr(),
               h4("E.coli + Flow = Loading Data"),
               DT::dataTableOutput("loaddat")
             )
            
    ),
    tabPanel("Time Series",
             plotOutput("Time_Series"),
             hr(),
             h5(strong("Use the Monitoring Location Name drop down menu to toggle between sites and the date range menu to change the time axis in the plot above. Changing the criterion alters the percent exceedance reported at the top of the plot.")),
             br(),
             fluidRow(
               column(4,
                      selectInput("plottype", label = "Select Plot Type", choices = c("Point","Line"), selected = "Point"),
                      useShinyjs(),
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
                      textInput("crit1",label = "Max Criterion",value=max_crit),
                      textInput("crit2",label = "Geomean Criterion",value=geom_crit)
                       ),
             column(4,
                    downloadButton("dwn1",label = "Download Graph")
             ))
             ), 
    tabPanel("Load Duration Curves",
             selectInput("site1",
                         label = "Monitoring Location Name",
                         choices=c(unique(loading.dat$ML_Name))),
             hr(),
             plotOutput("LDC", width="100%", height="700px")
             ), 
    tabPanel("Monthly Geomeans", 
             plotOutput("Monthly_Geomeans")),
    tabPanel("Rec Season Geomeans",
             plotOutput("Rec_Geomeans")))
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
  
  output$ecolidat <- renderDataTable({datatable(head(ecoli.dat), options = list(dom="t", paging = FALSE))})
  output$flowdat <- renderDataTable({datatable(head(flow.dat), options = list(dom="t", paging = FALSE))})
  output$loaddat <- renderDataTable({datatable(loading.dat, options = list(dom="t", paging = FALSE))})
  
  output$Time_Series <- renderPlot({
    x = ecoli.dat[ecoli.dat$ML_Name %in% input$checkbox,]
    # Get number of sites
    uni.sites <- unique(x$ML_Name)
    colrs <- piratepal("basel")
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
      points(y$E.coli_Geomean~y$Date, pch=21, col="black", bg=colrs[i])
      site[i] = paste0(as.character(uni.sites[i])," (",perc.exc,"% Exceed)")
      colr[i] = colrs[i]
    }
    l=legend("topleft",c(site),col="black",pt.bg=c(colrs), pch=21, bty="n", cex=1) 
    
   })

   output$LDC <- renderPlot({
     
     x <- loading.dat[loading.dat$ML_Name==input$site1,]
     flow.plot <- x[order(x$Flow_Percentile),]
     
     # Pull out observed loadings (E.coli data)
     ecoli.loads <- x[!is.na(x$E.coli_Geomean),]
     colpal <- colorspace::sequential_hcl(4)
     spre <- ecoli.loads[ecoli.loads$Season=="Spring",]
     sume <- ecoli.loads[ecoli.loads$Season=="Summer",]
     fale <- ecoli.loads[ecoli.loads$Season=="Fall",]
     wine <- ecoli.loads[ecoli.loads$Season=="Winter",]

     plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (MPN/day)", xlim=c(0, 100), ylim=c(0,max(ecoli.loads$Observed_Loading)), main=paste("Load Duration Curve:",x$ML_Name[1]))
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
     points(spre$Observed_Loading~spre$Flow_Percentile, pch=21, col="black", bg=colpal[1], cex=1.5)
     points(sume$Observed_Loading~sume$Flow_Percentile, pch=21, col="black", bg=colpal[2], cex=1.5)
     points(fale$Observed_Loading~fale$Flow_Percentile, pch=21, col="black", bg=colpal[3], cex=1.5)
     points(wine$Observed_Loading~wine$Flow_Percentile, pch=21, col="black", bg=colpal[4], cex=1.5)
     legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "E.coli Loading - Spring", "E.coli Loading - Summer","E.coli Loading - Fall", "E.coli Loading - Winter"), bty="n", col=c("firebrick3","red","black","black","black","black"), lty=c(1,1,NA,NA,NA,NA),lwd=c(2,2,NA,NA,NA,NA),pch=c(NA,NA,21,21,21,21), pt.bg=c(NA,NA,colpal), cex=1)
     
     })

     output$Monthly_Geomeans <- renderPlot({
       x <- month.dat[month.dat$ML_Name==input$site1,]
       rownames(x) <- x$month
       mo_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
       barp <- barplot(t(mo_load.p), beside=T, main = paste("Monthly E.coli Loading Geomean:",x$ML_Name[1]), ylim=c(0, max(c(mo_load.p$Observed_Loading, mo_load.p$Loading_Capacity_MOS))+0.1*max(c(mo_load.p$Observed_Loading, mo_load.p$Loading_Capacity_MOS))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
       legend("topleft",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
       box(bty="l")
       barps <- barp[1,]
       barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
       barperc <- barperc[barperc$V3>0,]
       barperc$V3 <- paste(barperc$V3,"%",sep="")
       text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
     }, width = 900, height = 600)
     
     output$Rec_Geomeans <- renderPlot({
       x <- rec.dat[rec.dat$ML_Name==input$site1,]
       rownames(x) <- x$year
       rec.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
       barp <- barplot(t(rec.p), beside=T, main = paste("Rec Season E.coli Loading Geomean by Year:",x$ML_Name[1]), ylim=c(0, max(c(rec.p$Observed_Loading, rec.p$Loading_Capacity_MOS))+.1*max(c(rec.p$Observed_Loading, rec.p$Loading_Capacity_MOS))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
       legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
       box(bty="l")
       barps <- barp[1,]
       barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
       barperc <- barperc[barperc$V3>0,]
       barperc$V3 <- paste(barperc$V3,"%",sep="")
       text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
     }, width = 900, height = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)


############# OLD CODE #################

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
