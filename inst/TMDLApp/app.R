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
                      useShinyjs(),
                      
                      selectInput("site1",
                                  label = "Monitoring Location Name",
                                  choices=c(unique(ecoli.dat$ML_Name))
                      ),
                      div(
                        id = "date",
                        dateRangeInput("dateRange",
                                     label="Date Range",
                                     start=min(ecoli.dat$Date),
                                     end=max(ecoli.dat$Date),
                                     min=min(ecoli.dat$Date),
                                     max=max(ecoli.dat$Date)
                      )),
                      actionButton("reset_input","Reset Date Range")),
               column(4,
                      textInput("crit1",label = "Max Criterion",value=668),
                      textInput("crit2",label = "Geomean Criterion",value=206)
                       ),
             column(4,
                    downloadButton("dwn1",label = "Download Graph")
             ))
             ), 
    tabPanel("Load Duration Curves",
             selectInput("site2",
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
  
  observeEvent(input$reset_input,{
    reset("date")
  })
  
  output$ecolidat <- renderDataTable({datatable(head(ecoli.dat), options = list(dom="t", paging = FALSE))})
  output$flowdat <- renderDataTable({datatable(head(flow.dat), options = list(dom="t", paging = FALSE))})
  output$loaddat <- renderDataTable({datatable(loading.dat, options = list(dom="t", paging = FALSE))})
  
  output$Time_Series <- renderPlot({
     x <- ecoli.dat[ecoli.dat$ML_Name==input$site1,]
     min = input$dateRange[1]
     max = input$dateRange[2]
     x <- x[x$Date>min&x$Date<max,]
     max.e <- max(x$E.coli)
     min.e <- min(x$E.coli)
     crit = as.numeric(input$crit1)
     perc.exc = round(length(x$E.coli[x$E.coli>crit])/length(x$E.coli)*100, digits=0)
     plot(x$E.coli~x$Date,xlab="",ylab="MPN/100 mL", xaxt="n", pch=19, main=paste("E.coli","in",x$ML_Name[1],":",perc.exc,"% exceed Max Crit Std."))
     lines(x$E.coli~x$Date,xlab="Date",ylab="MPN/100 mL", lwd=1.5, lty=2)
     axis.Date(1, at=seq(input$dateRange[1], input$dateRange[2], by="6 months"), format="%m-%Y", las=2, cex=0.8)
     abline(h=input$crit2,col="orange", lwd=2)
     abline(h=input$crit1, col="red", lwd=2)
     l=legend("topleft",c("Max","Geomean"),col=c("red","orange"), lty=1, lwd=2, bty="n", cex=1) 
     #text(x=l$text$x[1], y=l$text$y[1]-l$rect$h[1]/2,paste("Max E.coli:",max.e,"MPN/100 mL"), adj=c(0,1), cex=0.8)
     
   })

   output$LDC <- renderPlot({
     x <- loading.dat[loading.dat$ML_Name==input$site2,]
     # Order flow data
     flow.plot <- x[order(x$flow.percentile),]
     # Pull out observed loadings (E.coli data)
     ecoli.loads <- x[!is.na(x$E.coli),]
     plot(1, type="n", xlab="Flow Exceedance Percentile", ylab="E.coli Load (MPN/day)", xlim=c(0, 100), ylim=c(0,max(ecoli.loads$observed.loading)), main=paste("Load Duration Curve:",x$ML_Name[1]))
     abline(v=10, lty=2)
     abline(v=40, lty=2)
     abline(v=60, lty=2)
     abline(v=90, lty=2)
     text(5, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"High \n Flows")
     text(25, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Moist \n Conditions")
     text(50, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Mid-Range \n Flows")
     text(75, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Dry \n Conditions")
     text(95, max(ecoli.loads$observed.loading)-.2*max(ecoli.loads$observed.loading),"Low \n Flows")
     lines(flow.plot$loading.capacity~flow.plot$flow.percentile, type="l", col="blue", lwd=2)
     lines(flow.plot$loading.capacity.mos~flow.plot$flow.percentile, col="green", lwd=2)
     points(ecoli.loads$observed.loading~ecoli.loads$flow.percentile, pch=21, col="black", bg="purple", cex=1.5)
     legend("topright",legend=c("Loading Capacity","Loading Capacity + 10% MOS", "Observed E.coli Loading"), bty="n", col=c("blue","green","black"), lty=c(1,1,NA),lwd=c(2,2,NA),pch=c(NA,NA,21), pt.bg=c(NA,NA,"purple"), cex=1)
   })

     output$Monthly_Geomeans <- renderPlot({
       x <- month.dat[month.dat$ML_Name==input$site2,]
       rownames(x) <- x$month
       mo_load.p <- x[,!names(x)%in%c("month","perc.red","MLID","ML_Name")]
       barp <- barplot(t(mo_load.p), beside=T, main = paste("Monthly E.coli Loading Geomean:",x$ML_Name[1]), ylim=c(0, max(c(mo_load.p$observed.loading, mo_load.p$loading.capacity))+0.1*max(c(mo_load.p$observed.loading, mo_load.p$loading.capacity))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
       legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=1)
       box(bty="l")
       barps <- barp[1,]
       barperc <- data.frame(cbind(barps,x$observed.loading, x$perc.red))
       barperc <- barperc[barperc$V3>0,]
       barperc$V3 <- paste(barperc$V3,"%",sep="")
       text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
     }, width = 900, height = 600)
     
     output$Rec_Geomeans <- renderPlot({
       x <- rec.dat[rec.dat$ML_Name==input$site2,]
       rownames(x) <- x$year
       rec.p <- x[,!names(x)%in%c("year","MLID","ML_Name","perc.red")]
       barp <- barplot(t(rec.p), beside=T, main = paste("Rec Season E.coli Loading Geomean by Year:",x$ML_Name[1]), ylim=c(0, max(c(rec.p$observed.loading+.1*rec.p$observed.loading, rec.p$loading.capacity))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
       legend("topleft",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=1)
       box(bty="l")
       barps <- barp[1,]
       barperc <- data.frame(cbind(barps,x$observed.loading, x$perc.red))
       barperc <- barperc[barperc$V3>0,]
       barperc$V3 <- paste(barperc$V3,"%",sep="")
       text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=1)
       
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
