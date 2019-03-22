#' Prep data for TMDL analysis and plotting
#' 
#' This function takes E.coli concentration and flow data (if applicable) to calculate geomean concentrations and loadings on a daily, monthly, rec-season, and irrigation season basis. Produces outputs that may be fed into plotting functions within the tmdlTools package.
#' @param wb_path A file path to the .xlsx file containing E.coli and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param specs Logical. If TRUE, uses geom_crit, max_crit, and mos from workbook. If FALSE, function requires inputs of geom_crit, max_crit, and mos.
#' @param rec_ssn A character string defining the recreation season over which to calculate geometric means. Defaults to May 1 to October 31.
#' @param irg_ssn A character string defining the irrigation season over which to calculate geometric means. Defaults to May 15 to October 15.
#' @param geom_crit Numeric. The geometric mean criterion for the E.coli dataset, taken from R317-2-14.
#' @param max_crit Numeric. The maximum criterion for the E.coli dataset, taken from R317-2-14.
#' @param cf Numeric. A correction factor to convert E.coli concentrations and flow data to loadings (amount per day). Default converts MPN/100 mL and cfs to MPN/day.
#' @param mos Numeric proportion. The percent margin of safety (as a proportion) to use when calculating percent exceedance/reduction needed.
#' @param overwrite Logical. If TRUE, function updates input .xlsx file. If FALSE, function writes a new workbook with the name of the original file plus today's date.xlsx.
#' @export tmdlCalcs
#' @importFrom dplyr percent_rank
#' @importFrom openxlsx loadWorkbook
#' @importFrom openxlsx readWorkbook
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#' @importFrom openxlsx saveWorkbook
#' @import lubridate
#' @import plyr
#' @import shiny

# library(openxlsx)
# library(lubridate)
# library(plyr)
# library(dplyr)
# library(shiny)

# ### TESTING ####
# tmdlCalcs(wb_path=wb_path, specs=TRUE, overwrite = FALSE)
# # # 
# wb_path = "C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\Fremont_data.xlsx"
# specs = TRUE
# # rec_ssn = c("05-01","10-31")
# # irg_ssn = c("05-15","10-15")
# overwrite=FALSE

tmdlCalcs <- function(wb_path, 
                         specs = TRUE,
                         geom_crit,
                         max_crit, 
                         mos = .1, 
                         cf=1000/100*28.3168*3600*24, 
                         rec_ssn = c("05-01","10-31"),
                         irg_ssn = c("05-15","10-15"),
                         overwrite=FALSE){
  
  ## Calculation functions needed for plotting and assessment ## 
  gmean=function(x){exp(mean(log(x)))} # geometric mean
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){(1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)
  
  ### Load the dataset from the workbook and convert to dates ###
  wb.dat <- loadWorkbook(wb_path)
  ecoli.dat <- readWorkbook(wb.dat,sheet="Ecoli_data",startRow=1)
  ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
  if("Flow_data"%in%wb.dat$sheet_names){
    flow.dat <- readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
    flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30") 
    flo.dat = TRUE
  }else{flo.dat=FALSE}

  
  ### Obtain criteria from specs.dat sheet or function inputs ###
  # If xlsx workbook contains worksheet "Inputs", then pull criteria and other inputs from that... 
  if(specs){
    specs.dat <- readWorkbook(wb.dat, sheet="Inputs",startRow=1)
    geom_crit = specs.dat[specs.dat$Parameter=="Geometric Mean Criterion","Value"]
    max_crit = specs.dat[specs.dat$Parameter=="Max Criterion","Value"]
    cf = specs.dat[specs.dat$Parameter=="Correction Factor","Value"]
    mos = specs.dat[specs.dat$Parameter=="Margin of Safety","Value"]
    rec_ssn = as.Date(c(specs.dat[specs.dat$Parameter=="Rec Season Start","Value"],specs.dat[specs.dat$Parameter=="Rec Season End","Value"]), origin = "1899-12-30")
    irg_ssn = as.Date(c(specs.dat[specs.dat$Parameter=="Irrigation Season Start","Value"],specs.dat[specs.dat$Parameter=="Irrigation Season End","Value"]), origin = "1899-12-30")
  }else{ # Otherwise, use user-defined inputs to function.
    print("Input tab not detected. tmdlCalcs will use user-specified or default criteria, correction factor, MOS, and season lengths.")
    geom_crit = geom_crit
    max_crit = max_crit
    cf = cf
    mos = mos
    rec_ssn = rec_ssn
    irg_ssn = irg_ssn
  }
  
  ### Convert any "<" to min and max detection limits
  ecoli.dat$E.coli=gsub("<1",1,ecoli.dat$E.coli)
  ecoli.dat$E.coli=as.numeric(gsub(">2419.6",2420,ecoli.dat$E.coli))
  ecoli.dat = ecoli.dat[!is.na(ecoli.dat$E.coli),]
  
  # Trim white space
  ecoli.dat$ML_Name = trimws(ecoli.dat$ML_Name)
  
  # Take geometric mean over same site/day samples
  if(!length(unique(ecoli.dat$ML_Name))== length(unique(ecoli.dat$MLID))){
    warning("Monitoring location names are not specific to MLID's. Function will calculate separate daily geomeans for each ML_Name/MLID combination")
  }
  ecoli.day.gmean <- aggregate(E.coli~Date+ML_Name+MLID, data=ecoli.dat, FUN=function(x){exp(mean(log(x)))})
  names(ecoli.day.gmean)[names(ecoli.day.gmean)=="E.coli"] <- "E.coli_Geomean"
  
  # Determine calendar season for LDC - taken from https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
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
  
  ecoli.day.gmean$CalSeason <- getSeason(ecoli.day.gmean$Date)
  
  # Determine if each point falls within the rec season
  rec_ssn1 = yday(as.Date(rec_ssn, "%m-%d"))
  ecoli.day.gmean$Rec_Season = ifelse(yday(ecoli.day.gmean$Date)>=rec_ssn1[1]&yday(ecoli.day.gmean$Date)<=rec_ssn1[2],"Rec Season","Not Rec Season")
  
  # Determine if each point falls within the irrigation season
  irg_ssn1 = yday(as.Date(irg_ssn, "%m-%d"))
  ecoli.day.gmean$Irg_Season = ifelse(yday(ecoli.day.gmean$Date)>=irg_ssn1[1]&yday(ecoli.day.gmean$Date)<=irg_ssn1[2],"Irrigation Season","Not Irrigation Season")
  
  # Write daily geometric mean data to new datasheet 
    if(!any(wb.dat$sheet_names=="Daily_Geomean_Data")){
    addWorksheet(wb.dat, "Daily_Geomean_Data", gridLines = TRUE)
    writeData(wb.dat, sheet = "Daily_Geomean_Data", ecoli.day.gmean, rowNames = FALSE)}
  
  ###### LOAD DURATION CALCULATIONS, MONTHLY LOADS/REC SEASON/IRG SEASON AND PERC REDUCTION #######
  if(flo.dat){
    ## Create loading dataset
    ecoli.flow.dat <- merge(flow.dat,ecoli.day.gmean, all.x=TRUE)
    ecoli.flow.dat$Loading_Capacity <- ecoli.flow.dat$Flow*geom_crit*cf
    ecoli.flow.dat$Loading_Capacity_MOS <- ecoli.flow.dat$Loading_Capacity*(1-mos)
    ecoli.flow.dat$Observed_Loading <- ecoli.flow.dat$Flow*ecoli.flow.dat$E.coli_Geomean*cf
    ecoli.flow.dat$Exceeds <- ifelse(ecoli.flow.dat$Observed_Loading>ecoli.flow.dat$Loading_Capacity_MOS,"yes","no")
    
    # Flow Percentile calc function
    ldc_func <- function(x){
      x$Flow_Percentile = flow_perc(x$Flow)
      out = x
      return(out)
    }
    
    # Apply percentile calc to data
    ldc.dat <- ddply(.data=ecoli.flow.dat, .(MLID, ML_Name), .fun=ldc_func)
    
    # Write load duration curve data to new datasheet 
    if(!any(wb.dat$sheet_names=="LDC_Data")){
      addWorksheet(wb.dat, "LDC_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "LDC_Data", ldc.dat, rowNames = FALSE)
    }  

    # Continue forward with loadings only for records with E.coli concentrations
    ecoli.ldc <- ldc.dat[!is.na(ldc.dat$E.coli_Geomean),]
    
    ## Loading by month ##
    ecoli.ldc$month <- month(ecoli.ldc$Date, label=TRUE)
    ol_mo <- aggregate(Observed_Loading~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
    lc_mo <- aggregate(Loading_Capacity_MOS~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
    mo_load <- merge(ol_mo,lc_mo, all=TRUE)
    mo_load <- mo_load[order(mo_load$month),]
    mo_load$Percent_Reduction_L <- ifelse(mo_load$Observed_Loading>mo_load$Loading_Capacity_MOS,round(perc.red(mo_load$Loading_Capacity_MOS,mo_load$Observed_Loading), digits=0),0)
    
    ## Loading by rec season ##
    ecoli.ldc$Year = year(ecoli.ldc$Date)
    ol_rec <- aggregate(Observed_Loading~Year+MLID+ML_Name+Rec_Season, dat=ecoli.ldc, FUN=gmean)
    lcmos_rec <- aggregate(Loading_Capacity_MOS~Year+MLID+ML_Name+Rec_Season, dat=ecoli.ldc, FUN=gmean)
    rec_load <- merge(ol_rec,lcmos_rec, all=TRUE)
    rec_load$Percent_Reduction_L <- ifelse(rec_load$Observed_Loading>rec_load$Loading_Capacity_MOS,round(perc.red(rec_load$Loading_Capacity_MOS,rec_load$Observed_Loading), digits=0),0)
    
    ## Loading by irrigation season ##
    ol_irg <- aggregate(Observed_Loading~Year+MLID+ML_Name+Irg_Season, dat=ecoli.ldc, FUN=gmean)
    lcmos_irg <- aggregate(Loading_Capacity_MOS~Year+MLID+ML_Name+Irg_Season, dat=ecoli.ldc, FUN=gmean)
    irg_load <- merge(ol_irg,lcmos_irg, all=TRUE)
    irg_load$Percent_Reduction_L <- ifelse(irg_load$Observed_Loading>irg_load$Loading_Capacity_MOS,round(perc.red(irg_load$Loading_Capacity_MOS,irg_load$Observed_Loading), digits=0),0)
    
  }else{mo_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))
        rec_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))
        irg_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))}

  ## Concentration by month ##
    ecoli.day.gmean$month <- lubridate::month(ecoli.day.gmean$Date, label=TRUE)
    concen_mo <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=ecoli.day.gmean, FUN=gmean)
    concen_mo$Percent_Reduction_C <- ifelse(concen_mo$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_mo$E.coli_Geomean), digits=0),0)
    
  ## Concentration by rec season ##
    ecoli.day.gmean$Year <- lubridate::year(ecoli.day.gmean$Date)
    concen_rec <- aggregate(E.coli_Geomean~Rec_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=gmean)
    concen_rec$Percent_Reduction_C <- ifelse(concen_rec$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_rec$E.coli_Geomean), digits=0),0)

  ## Concentration by irrigation season ##
    concen_irg <- aggregate(E.coli_Geomean~Irg_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=gmean)
    concen_irg$Percent_Reduction_C <- ifelse(concen_irg$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_irg$E.coli_Geomean), digits=0),0)

  # Merge monthly data and write to new datasheet 
    month.dat = merge(concen_mo,mo_load, all=TRUE)
    
    if(!any(wb.dat$sheet_names=="Monthly_Data")){
      addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Monthly_Data", month.dat, rowNames = FALSE)}
 
  # Merge rec season data and write to new datasheet
    rec.dat = merge(rec_load,concen_rec, all=TRUE)
    
    if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
      addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Rec_Season_Data", rec.dat, rowNames = FALSE)}
  
  # Merge irrigation season data and write to new datasheet
    irg.dat = merge(irg_load, concen_irg, all=TRUE)
  
    if(!any(wb.dat$sheet_names=="Irg_Season_Data")){
      addWorksheet(wb.dat, "Irg_Season_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Irg_Season_Data", irg.dat, rowNames = FALSE)}
  
  ############################ SAVE WORKBOOK FILE WITH NEW SHEETS #########################
  if(overwrite){
    saveWorkbook(wb.dat, wb_path, overwrite = TRUE)
  }else{saveWorkbook(wb.dat, paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx"), overwrite = TRUE)}

  
  # ############################## PLOTTING USING FUNCTIONS ABOVE ###################
  # if(plot_it){
  #   by(ecoli.day.gmean,ecoli.day.gmean[,c("MLID","ML_Name")],plotTimeSeries, max_crit=max_crit, yeslines=TRUE, wndws=FALSE) # time series plots
  #   by(ldc.dat,ldc.dat[,"MLID"],plotLDC, wndws=FALSE) # load duration curves
  #   by(mo_load,mo_load[,"MLID"],plotMonthlyLoads, wndws=FALSE) # monthly loadings
  #   by(rec,rec[,"MLID"],plotRecSeasonLoads, wndws=FALSE) # rec season loadings by year
  # }

}



####### OLD CODE ######
### Note 1:10 dilutions ###
# dils <- grepl("1:10",ecoli.dat1$ML_Name)
# ecoli.dat1$Notes <- ifelse(dils,"1:10 Dilution",NA)
# ecoli.dat1$ML_Name = gsub("1:10 dilution","",ecoli.dat1$ML_Name)
# ecoli.dat1$ML_Name = gsub("\\()","",ecoli.dat1$ML_Name)

### Remove any dups ###
# ecoli.dat1 <- ecoli.dat[!grepl("DUP",ecoli.dat$ML_Name),]
