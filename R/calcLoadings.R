#' Prep data for TMDL analysis and plotting
#' 
#' This function takes E.coli concentration and flow data to calculating loadings on a daily, monthly, and rec-season basis. Produces outputs that may be fed into plotting functions within the tmdlTools package.
#' @param wb_path A file path to the .xlsx file containing E.coli and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param geom_crit Numeric. The geometric mean criterion for the E.coli dataset, taken from R317-2-14.
#' @param max_crit Numeric. The maximum criterion for the E.coli dataset, taken from R317-2-14.
#' @param mos Numeric proportion. The percent margin of safety (as a proportion) to use when calculating percent exceedance/reduction needed.
#' @param plot_it Logical. If TRUE, plots time series, LDC, monthly and rec geomeans in scatter and bar plots.
#' @param run_shiny Logical. If TRUE, launches shiny app browser where user can toggle between sites within the E.coli dataset.
#' @param overwrite Logical. If TRUE, function updates input .xlsx file. If FALSE, function writes a new workbook with the name of the original file plus today's date.xlsx.
#' @export calcLoadings
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom plyr ddply
#' @import shiny

# library(openxlsx)
# library(lubridate)
# library(plyr)
# library(dplyr)
# library(shiny)
# geom_crit <- 126
# max_crit <- 409
# cf <- 1000/100*28.3168*3600*24
# wb_path <- "C:\\Users\\ehinman\\Documents\\NFVR_data.xlsx"
# wb_path <- "Spring_Creek.xlsx"

### TESTING ####
#calcLoadings(wb_path="Spring_Creek.xlsx", geom_crit = 206, max_crit = 668, mos = 0.1, plot_it = TRUE, run_shiny = FALSE, overwrite = FALSE)

calcLoadings <- function(wb_path,geom_crit,max_crit, mos = .1, cf=1000/100*28.3168*3600*24, plot_it=FALSE, run_shiny=TRUE, overwrite=FALSE){
  
  ## Calculation functions needed for plotting and assessment ## 
  gmean=function(x){exp(mean(log(x)))} # geometric mean
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){flow.percentile = (1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)
  
  ### Load the dataset from the workbook and convert to dates ###
  wb.dat <- loadWorkbook(wb_path)
  ecoli.dat <- readWorkbook(wb.dat,sheet="Ecoli_data",startRow=1)
  ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
  flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
  flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
  
  ### Convert any "<" to min and max detection limits
  ecoli.dat$E.coli=gsub("<1",1,ecoli.dat$E.coli)
  ecoli.dat$E.coli=as.numeric(gsub(">2419.6",2420,ecoli.dat$E.coli))
  ecoli.dat = ecoli.dat[!is.na(ecoli.dat$E.coli),]
  
  # Trim white space
  ecoli.dat1$ML_Name = trimws(ecoli.dat$ML_Name)
  
  # Take geometric mean over same site/day samples
  ecoli.day.gmean <- aggregate(E.coli~Date+ML_Name+MLID, data=ecoli.dat1, FUN=function(x){exp(mean(log(x)))})
  
  # Determine season for LDC
  
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
    
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))
  }
  
  ecoli.day.gmean$Season <- getSeason(ecoli.day.gmean$Date)
  
  # Write daily geometric mean data to new datasheet 
  if(!any(wb.dat$sheet_names=="Daily_Geomean_Data")){
    addWorksheet(wb.dat, "Daily_Geomean_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Daily_Geomean_Data", ecoli.day.gmean, rowNames = FALSE)}
  
  ###### LOAD DURATION CALCULATIONS, MONTHLY LOADS/REC SEASON AND PERC REDUCTION #######
  
  ## Create loading dataset
  ecoli.flow.dat <- merge(flow.dat,ecoli.day.gmean, all.x=TRUE)
  
  # LDC calc function
  ldc_func <- function(x){
    # Calculate loadings
    x$loading.capacity <- x$Flow.cfs*geom_crit*cf
    x$loading.capacity.mos <- x$loading.capacity*(1-mos)
    x$observed.loading <- x$Flow.cfs*x$E.coli*cf
    x$exceedance <- ifelse(x$observed.loading>x$loading.capacity.mos,"yes","no")
    x$flow.percentile = flow_perc(x$Flow.cfs)
    out = x
    return(out)
    }
  
  # Apply LDC calc to data
  ldc.dat <- ddply(.data=ecoli.flow.dat, .(MLID), .fun=ldc_func)
  
  # Write load duration curve data to new datasheet 
  if(!any(wb.dat$sheet_names=="LDC_Data")){
    addWorksheet(wb.dat, "LDC_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "LDC_Data", ldc.dat, rowNames = FALSE)}
  
  # Continue forward with loadings only for records with E.coli concentrations
  ecoli.ldc <- ldc.dat[!is.na(ldc.dat$E.coli),]
  
  ## Loading by month ##
  ecoli.ldc$month <- month(ecoli.ldc$Date, label=TRUE)
  ol_mo <- aggregate(observed.loading~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
  lc_mo <- aggregate(loading.capacity~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
  mo_load <- merge(ol_mo,lc_mo, all=TRUE)
  mo_load <- mo_load[order(mo_load$month),]
  mo_load$perc.red <- ifelse(mo_load$observed.loading>mo_load$loading.capacity,round(perc.red(mo_load$loading.capacity,mo_load$observed.loading), digits=0),0)
  #rownames(mo_load)<- mo_load$month
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Monthly_Data")){
    addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Monthly_Data", mo_load, rowNames = FALSE)}
 
 
  ## Filter to Rec Season ##
  ecoli.ldc.rec <- ecoli.ldc[month(ecoli.ldc$Date)>4&month(ecoli.ldc$Date)<10,]
  ecoli.ldc.rec$year <- year(ecoli.ldc.rec$Date)
  
  # Aggregate by geometric mean
  ol_rec <- aggregate(observed.loading~year+MLID+ML_Name, dat=ecoli.ldc.rec, FUN=gmean)
  lcmos_rec <- aggregate(loading.capacity.mos~year+MLID+ML_Name, dat=ecoli.ldc.rec, FUN=gmean)
  rec <- merge(ol_rec,lcmos_rec, all=TRUE)
  rec$perc.red <- ifelse(rec$observed.loading>rec$loading.capacity,round(perc.red(rec$loading.capacity,rec$observed.loading), digits=0),0)
  
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
    addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Rec_Season_Data", rec, rowNames = FALSE)}
  
  ######################## PLOTTING FUNCTIONS #########################################
  
  # Time Series Plotting function - used at bottom if plot_it = TRUE
  # Function to plot MPN over time by site -- add in time period option.
  time_series_plot <- function(x){
    windows()
    # Get dates for axis
    max.dat <- max(x$Date)
    min.dat <- min(x$Date)
    max.e <- max(x$E.coli)
    min.e <- min(x$E.coli)
    perc.exc = round(length(x$E.coli[x$E.coli>max_crit])/length(x$E.coli)*100, digits=0)
    plot(x$E.coli~x$Date,xlab="",ylab="MPN/100 mL", xaxt="n", pch=19, main=paste("E.coli","in",x$ML_Name[1],":",perc.exc,"% exceed Max Crit Std."))
    lines(x$E.coli~x$Date,xlab="Date",ylab="MPN/100 mL", lwd=1.5, lty=2)
    axis.Date(1, at=seq(min.dat, max.dat, by="6 months"), format="%m-%Y", las=2, cex=0.8)
    abline(h=geom_crit,col="orange", lwd=2)
    abline(h=max_crit, col="red", lwd=2)
    l=legend("topleft",c("Max","Geomean"),col=c("red","orange"), lty=1, lwd=2, bty="n", cex=0.6) 
    #text(x=l$text$x[1], y=l$text$y[1]-l$rect$h[1]/2,paste("Max E.coli:",max.e,"MPN/100 mL"), adj=c(0,1), cex=0.8)
    
  }
  
  # LDC plot function - used below if plot_it=TRUE
  ldc_plot <- function(x){
    # Order flow data
    flow.plot <- x[order(x$flow.percentile),]
    # Pull out observed loadings (E.coli data)
    ecoli.loads <- x[!is.na(x$E.coli),]
    windows()
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
    legend("topright",legend=c("Loading Capacity","Loading Capacity + MOS", "Observed E.coli Loading"), bty="n", col=c("blue","green","black"), lty=c(1,1,NA),lwd=c(2,2,NA),pch=c(NA,NA,21), pt.bg=c(NA,NA,"purple"), cex=1)
  }
  
  
  # MONTHLY Plotting function - used below if plot_it=TRUE
  month_loading_plot <- function(x){
    rownames(x) <- x$month
    mo_load.p <- x[,!names(x)%in%c("month","perc.red","MLID","ML_Name")]
    windows()
    barp <- barplot(t(mo_load.p), beside=T, main = paste("Monthly E.coli Loading Geomean:",x$ML_Name[1]), ylim=c(0, max(c(mo_load.p$observed.loading, mo_load.p$loading.capacity))+0.1*max(c(mo_load.p$observed.loading, mo_load.p$loading.capacity))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
    legend("topleft",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
    box(bty="l")
    barps <- barp[1,]
    barperc <- data.frame(cbind(barps,x$observed.loading, x$perc.red))
    barperc <- barperc[barperc$V3>0,]
    barperc$V3 <- paste(barperc$V3,"%",sep="")
    text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
  }
  
  # REC SEASON Plotting function - used below if plot_it=TRUE
  rec_season_plot <- function(x){
    rownames(x) <- x$year
    windows()
    rec.p <- x[,!names(x)%in%c("year","MLID","ML_Name","perc.red")]
    barp <- barplot(t(rec.p), beside=T, main = paste("Rec Season E.coli Loading Geomean by Year:",x$ML_Name[1]), ylim=c(0, max(c(rec.p$observed.loading, rec.p$loading.capacity))+.1*max(c(rec.p$observed.loading, rec.p$loading.capacity))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
    legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
    box(bty="l")
    barps <- barp[1,]
    barperc <- data.frame(cbind(barps,x$observed.loading, x$perc.red))
    barperc <- barperc[barperc$V3>0,]
    barperc$V3 <- paste(barperc$V3,"%",sep="")
    text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
    
  }
  
  ############################## PLOTTING USING FUNCTIONS ABOVE ###################
  if(plot_it){
    by(ecoli.day.gmean,ecoli.day.gmean[,c("MLID","ML_Name")],time_series_plot) # time series plots
    by(ldc.dat,ldc.dat[,"MLID"],ldc_plot) # load duration curves
    by(mo_load,mo_load[,"MLID"],month_loading_plot) # monthly loadings
    by(rec,rec[,"MLID"],rec_season_plot) # rec season loadings by year
  }
  ############################ SAVE WORKBOOK FILE WITH NEW SHEETS #########################
  if(overwrite){
    saveWorkbook(wb.dat, wb_path, overwrite = TRUE)
    wb_path_new = wb_path
  }else{saveWorkbook(wb.dat, paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx"), overwrite = TRUE)
    wb_path_new = paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx")}
  
  ########################### RUN SHINY APP ############################################
  if(run_shiny){
    wb_path_new <<- wb_path_new
    runApp("app.R")
  }
}


####### OLD CODE ######
### Note 1:10 dilutions ###
# dils <- grepl("1:10",ecoli.dat1$ML_Name)
# ecoli.dat1$Notes <- ifelse(dils,"1:10 Dilution",NA)
# ecoli.dat1$ML_Name = gsub("1:10 dilution","",ecoli.dat1$ML_Name)
# ecoli.dat1$ML_Name = gsub("\\()","",ecoli.dat1$ML_Name)

### Remove any dups ###
# ecoli.dat1 <- ecoli.dat[!grepl("DUP",ecoli.dat$ML_Name),]
