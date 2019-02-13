#' Prep data for TMDL analysis and plotting
#' 
#' This function takes E.coli concentration and flow data to calculating loadings on a daily, monthly, and rec-season basis. Produces outputs that may be fed into plotting functions within the tmdlTools package.
#' @param wb_path A file path to the .xlsx file containing E.coli and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param specs Logical. If TRUE, uses geom_crit, max_crit, and mos from workbook. If FALSE, function requires inputs of geom_crit, max_crit, and mos.
#' @param geom_crit Numeric. The geometric mean criterion for the E.coli dataset, taken from R317-2-14.
#' @param max_crit Numeric. The maximum criterion for the E.coli dataset, taken from R317-2-14.
#' @param cf Numeric. A correction factor to convert E.coli concentrations and flow data to loadings (amount per day). Default converts MPN/100 mL and cfs to MPN/day.
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

library(openxlsx)
library(lubridate)
library(plyr)
library(dplyr)
library(shiny)

### TESTING ####
# calcLoadings(wb_path="C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\Spring_Creek.xlsx", specs=TRUE, geom_crit = 206, max_crit = 668, mos = 0.1, plot_it = FALSE, run_shiny = FALSE, overwrite = FALSE)
# 
calcLoadings <- function(wb_path, specs = TRUE, geom_crit,max_crit, mos = .1, cf=1000/100*28.3168*3600*24, plot_it=FALSE, run_shiny=TRUE, overwrite=FALSE){
  
  ## Calculation functions needed for plotting and assessment ## 
  gmean=function(x){exp(mean(log(x)))} # geometric mean
  gsd=function(x){exp(sd(log(x)))} # standard deviation of the geometric mean - shows spread in the data
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){(1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)
  
  ### Load the dataset from the workbook and convert to dates ###
  wb.dat <- loadWorkbook(wb_path)
  ecoli.dat <- readWorkbook(wb.dat,sheet="Ecoli_data",startRow=1)
  ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
  flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
  flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30")
  
  ### Obtain criteria from specs.dat sheet or function inputs ###
  # If xlsx workbook contains worksheet "Inputs", then pull criteria and other inputs from that... 
  if(specs){
    specs.dat <- readWorkbook(wb.dat, sheet="Inputs",startRow=1)
    geom_crit = specs.dat[specs.dat$Parameter=="Geometric Mean Criterion","Value"]
    max_crit = specs.dat[specs.dat$Parameter=="Max Criterion","Value"]
    cf = specs.dat[specs.dat$Parameter=="Correction Factor","Value"]
    mos = specs.dat[specs.dat$Parameter=="Margin of Safety","Value"]
  }else{ # Otherwise, use user-defined inputs to function.
    geom_crit = geom_crit
    max_crit = max_crit
    cf = cf
    mos = mos
  }
  
  ### Convert any "<" to min and max detection limits
  ecoli.dat$E.coli=gsub("<1",1,ecoli.dat$E.coli)
  ecoli.dat$E.coli=as.numeric(gsub(">2419.6",2420,ecoli.dat$E.coli))
  ecoli.dat = ecoli.dat[!is.na(ecoli.dat$E.coli),]
  
  # Trim white space
  ecoli.dat$ML_Name = trimws(ecoli.dat$ML_Name)
  
  # Take geometric mean over same site/day samples
  ecoli.day.gmean <- aggregate(E.coli~Date+ML_Name+MLID, data=ecoli.dat, FUN=function(x){exp(mean(log(x)))})
  names(ecoli.day.gmean)[names(ecoli.day.gmean)=="E.coli"] <- "E.coli_Geomean"
  
  # Determine season for LDC - taken from https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))}
  
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
    x$Loading_Capacity <- x$Flow*geom_crit*cf
    x$Loading_Capacity_MOS <- x$Loading_Capacity*(1-mos)
    x$Observed_Loading <- x$Flow*x$E.coli_Geomean*cf
    x$Exceeds <- ifelse(x$Observed_Loading>x$Loading_Capacity_MOS,"yes","no")
    x$Flow_Percentile = flow_perc(x$Flow)
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
  ecoli.ldc <- ldc.dat[!is.na(ldc.dat$E.coli_Geomean),]
  
  ## Loading by month ##
  ecoli.ldc$month <- month(ecoli.ldc$Date, label=TRUE)
  ol_mo <- aggregate(Observed_Loading~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
  lc_mo <- aggregate(Loading_Capacity_MOS~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
  mo_load <- merge(ol_mo,lc_mo, all=TRUE)
  mo_load <- mo_load[order(mo_load$month),]
  mo_load$Percent_Reduction <- ifelse(mo_load$Observed_Loading>mo_load$Loading_Capacity_MOS,round(perc.red(mo_load$Loading_Capacity_MOS,mo_load$Observed_Loading), digits=0),0)
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Monthly_Data")){
    addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Monthly_Data", mo_load, rowNames = FALSE)}
 
 
  ## Filter to Rec Season ##
  ecoli.ldc.rec <- ecoli.ldc[month(ecoli.ldc$Date)>4&month(ecoli.ldc$Date)<10,]
  ecoli.ldc.rec$year <- year(ecoli.ldc.rec$Date)
  
  # Aggregate by geometric mean
  ol_rec <- aggregate(Observed_Loading~year+MLID+ML_Name, dat=ecoli.ldc.rec, FUN=gmean)
  lcmos_rec <- aggregate(Loading_Capacity_MOS~year+MLID+ML_Name, dat=ecoli.ldc.rec, FUN=gmean)
  rec <- merge(ol_rec,lcmos_rec, all=TRUE)
  rec$Percent_Reduction <- ifelse(rec$Observed_Loading>rec$Loading_Capacity_MOS,round(perc.red(rec$Loading_Capacity_MOS,rec$Observed_Loading), digits=0),0)
  
  
  # Write monthly data to new datasheet 
  if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
    addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Rec_Season_Data", rec, rowNames = FALSE)}
  
  ############################ SAVE WORKBOOK FILE WITH NEW SHEETS #########################
  if(overwrite){
    saveWorkbook(wb.dat, wb_path, overwrite = TRUE)
    wb_path_new = wb_path
  }else{saveWorkbook(wb.dat, paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx"), overwrite = TRUE)
    wb_path_new = paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx")}

  
  ############################## PLOTTING USING FUNCTIONS ABOVE ###################
  if(plot_it){
    by(ecoli.day.gmean,ecoli.day.gmean[,c("MLID","ML_Name")],plotTimeSeries, max_crit=max_crit, yeslines=FALSE) # time series plots
    by(ldc.dat,ldc.dat[,"MLID"],plotLDC) # load duration curves
    by(mo_load,mo_load[,"MLID"],plotMonthlyLoads) # monthly loadings
    by(rec,rec[,"MLID"],plotRecSeasonLoads) # rec season loadings by year
  }

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
