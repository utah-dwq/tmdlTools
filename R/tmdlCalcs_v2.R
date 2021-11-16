#' Prep data for TMDL analysis and plotting
#' 
#' This function takes Parameter concentration and flow data (if applicable) to calculate mean concentrations and loadings on a daily, monthly, rec-season, and irrigation season basis. Produces outputs that may be fed into plotting functions within the tmdlTools package.
#' @param wb_path A file path to the .xlsx file containing Parameter and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param inputs Logical. Indicates whether the .xlsx file has an Inputs tab containing TMDL conversions and seasonal cutoffs. If not, these inputs must be specified as arguments in the function.
#' @param crit Numeric. The numeric criterion to be applied to the dataset.
#' @param cf Numeric. The correction factor to be applied to the loading calculation.
#' @param mos Numeric. A proportion between 0 and 1 to be used as the margin of safety applied to the TMDL calculations. In other words, this proportion describes the % reduction applied to the straight TMDL loading value based on the standard.
#' @param rec_ssn Numeric. A two-object vector of year days signifying the start and end to the rec season.
#' @param irg_ssn Numeric. A two-object vector of year days signifying the start and end to the irrigation season.
#' @param aggFun String. A character object describing the function used to aggregate to daily/monthly/rec season/irrigation season values. Most typically will be one of the following: gmean, mean, max, min.
#' @param units String. If not supplied in Inputs, units describes the loading units following application of the correction factor. This character string shows up in plot axes and in the export workbook.
#' @param exportfromfunc Logical. Indicates whether workbook should be exported from tmdlCalcs function, or skipped if using Shiny interface. Default is FALSE to accommodate Shiny use.
#' @return The output includes a new Excel workbook with the name of the original file plus today's date.xlsx, as well as the following dataframes, composed within a list: ecoli concentrations, flow data, ldc data, monthly means, rec/non rec means, and irg/non irg means.
#' @export tmdlCalcs
#' @import lubridate
#' @import plyr
#' @import dplyr
#' @import shiny
#' @import openxlsx
#' @import data.table

# library(openxlsx)
# library(lubridate)
# library(plyr)
# library(dplyr)
# library(shiny)

# ### TESTING ####
# tmdlCalcs(wb_path=wb_path, specs=TRUE, overwrite = FALSE)
# # # 
# wb_path = "C:\\Users\\ehinman\\Documents\\GitHub\\ecoli_tmdl\\Fremont_data.xlsx"
# overwrite=FALSE

tmdlCalcs <- function(wb_path, inputs = TRUE, crit, cf, mos, rec_ssn, irg_ssn, aggFun, units, exportfromfunc = FALSE){
  
  calcs <- list()
  
  ## Calculation functions needed for plotting and assessment ## 
  #gmean=function(x){exp(mean(log(x)))} # geometric mean
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){(1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)
  
  ### Load the dataset from the workbook ###
  wb.dat <- openxlsx::loadWorkbook(wb_path)
  
  ### Determine if data are from AWQMS or WQP
  # flowtest = openxlsx::readWorkbook(wb.dat, sheet = "Flow_data", startRow = 1)
  # if(!grepl(".",names(flowtest)[1])){}
  
  ### Obtain criteria from specs.dat sheet or function inputs ###
  if(inputs){
    specs.dat <- openxlsx::readWorkbook(wb.dat, sheet="Inputs",startRow=1)
    
    # Add to list
    calcs$Inputs <- specs.dat
    
    # Generate list of params for loading calcs
    crit = specs.dat[specs.dat$Parameter=="Numeric Criterion","Value"]
    cf = specs.dat[specs.dat$Parameter=="Correction Factor","Value"]
    mos = specs.dat[specs.dat$Parameter=="Margin of Safety","Value"]
    rec_ssn = c(specs.dat[specs.dat$Parameter=="Rec Season Start","Value"],specs.dat[specs.dat$Parameter=="Rec Season End","Value"])
    irg_ssn = c(specs.dat[specs.dat$Parameter=="Irrigation Season Start","Value"],specs.dat[specs.dat$Parameter=="Irrigation Season End","Value"])
    aggFun = specs.dat[1,"Aggregating.Function"]
    units = specs.dat[1, "TMDL.units"]
  }
  
  if(aggFun=="gmean"){
    aggFun = function(x){exp(mean(log(x)))}
  }
  
  
  ### Obtain site order from site_order sheet ###
  if("Site_order"%in%wb.dat$sheet_names){
    site.order = openxlsx::readWorkbook(wb.dat, sheet="Site_order", startRow = 1)
    if(dim(site.order)[1]>0){
      calcs$Site_order = site.order
    }
  }
  
  # Load Param concentrations
  param.dat <- openxlsx::readWorkbook(wb.dat,sheet="Param_data",startRow=1)
   
  ### Convert all columns of interest to AWQMS column names ###
  if("MonitoringLocationIdentifier"%in%names(param.dat)){
    data.table::setnames(param.dat, old = c("ActivityStartDate","CharacteristicName","ResultMeasureValue","ResultMeasure/MeasureUnitCode","DetectionQuantitationLimitMeasure.MeasureValue","MonitoringLocationIdentifier"), new = c("Activity.Start.Date","Characteristic.Name","Result.Value","Result.Unit","Detection.Limit.Value1","Monitoring.Location.ID"), skip_absent = TRUE)
  }
  param.dat$Activity.Start.Date <- as.Date(param.dat$Activity.Start.Date, origin="1899-12-30")
  
  # name_trans = data.frame("AWQMS" = c("Activity.Start.Date","Characteristic.Name","Result.Value","Result.Unit","Detection.Limit.Value1","Monitoring.Location.ID"),
  #                         "WQP" = c("ActivityStartDate","CharacteristicName","ResultMeasureValue","ResultMeasure.MeasureUnitCode","DetectionQuantitationLimitMeasure.MeasureValue","MonitoringLocationIdentifier"))
  # 
  ### CHECK THAT UNITS ARE CONSISTENT
  if(length(unique(param.dat$Result.Unit))>1){
    stop("Multiple units found in parameter data. Convert units before proceeding.")
  }
  
  # Add to list
  calcs$Param_data <- param.dat
  
  if("Flow_data"%in%wb.dat$sheet_names){
    flow.dat <- openxlsx::readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
    if("MonitoringLocationIdentifier"%in%names(flow.dat)){
      data.table::setnames(flow.dat, old = c("ActivityStartDate","CharacteristicName","ResultMeasureValue","ResultMeasure/MeasureUnitCode","DetectionQuantitationLimitMeasure.MeasureValue","MonitoringLocationIdentifier"), new = c("Activity.Start.Date","Characteristic.Name","Result.Value","Result.Unit","Detection.Limit.Value1","Monitoring.Location.ID"), skip_absent = TRUE)
    }
    flow.dat$Activity.Start.Date <- as.Date(flow.dat$Activity.Start.Date, origin="1899-12-30")
    if(any(flow.dat$Result.Value==0)){warning("Some measured flow values are zero. This will affect calculations in which the geomean is used.")}
    if(length(unique(flow.dat$Result.Unit))>1){
      stop("Multiple units found in flow data. Convert units before proceeding.")
    }
    flo.dat = TRUE
  }else{flo.dat=FALSE}

  ### Convert any "<" to min and max detection limits
  param.dat$Result.Value=gsub("<1",1,param.dat$Result.Value)
  param.dat$Result.Value=gsub(">2419.6",2420,param.dat$Result.Value)
  param.dat$Result.Value = as.numeric(param.dat$Result.Value)
  param.dat = param.dat[!is.na(param.dat$Result.Value),]
  names(param.dat)[names(param.dat)=="Characteristic.Name"] = "Parameter.Name"
  names(param.dat)[names(param.dat)=="Result.Value"] = "Parameter.Value"
  names(param.dat)[names(param.dat)=="Result.Unit"] = "Parameter.Unit"
  
  # Fill in concentrations present at detection limits
  param.dat$Parameter.Value = ifelse(is.na(param.dat$Parameter.Value),param.dat$Detection.Limit.Value1,param.dat$Parameter.Value)
  
  # Trim white space
  # param.dat$Monitoring.Location.ID = trimws(param.dat$Monitoring.Location.ID)
  
  # Use aggregating function over same site/day samples

  param.day.mean <- aggregate(Parameter.Value~Activity.Start.Date+Monitoring.Location.ID+Parameter.Name+Parameter.Unit, data=param.dat, FUN=aggFun)

  names(param.day.mean)[names(param.day.mean)=="Parameter.Value"] <- "Parameter.Value_Mean"
  
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
  
  param.day.mean$CalSeason <- getSeason(param.day.mean$Activity.Start.Date)
  
  # Determine if each point falls within the rec season
  #rec_ssn1 = yday(as.Date(rec_ssn, "%m-%d"))
  param.day.mean$Rec_Season = ifelse(yday(param.day.mean$Activity.Start.Date)>=rec_ssn[1]&yday(param.day.mean$Activity.Start.Date)<=rec_ssn[2],"Rec Season","Not Rec Season")
  
  # Determine if each point falls within the irrigation season
  #irg_ssn1 = yday(as.Date(irg_ssn, "%m-%d"))
  param.day.mean$Irg_Season = ifelse(yday(param.day.mean$Activity.Start.Date)>=irg_ssn[1]&yday(param.day.mean$Activity.Start.Date)<=irg_ssn[2],"Irrigation Season","Not Irrigation Season")
  
  # Add to list
  calcs$Daily_Mean_Data <- param.day.mean
  
  ###### LOAD DURATION CALCULATIONS, MONTHLY LOADS/REC SEASON/IRG SEASON AND PERC REDUCTION #######
  if(flo.dat){
    
    # Add to list
    calcs$Flow_data <- flow.dat
    
    flow.dat = flow.dat[,c("Activity.Start.Date","Monitoring.Location.ID","Characteristic.Name","Result.Value","Result.Unit")]
    names(flow.dat)[names(flow.dat)=="Result.Value"] = "Flow.Value"
    flow.dat$Flow.Value = as.numeric(flow.dat$Flow.Value)
    names(flow.dat)[names(flow.dat)=="Result.Unit"] = "Flow.Unit"
    
    flow.dat.mean <- aggregate(Flow.Value~Activity.Start.Date+Monitoring.Location.ID+Characteristic.Name+Flow.Unit, data=flow.dat, FUN=mean)
    
    ## Create loading dataset
    param.flow.dat <- merge(flow.dat.mean,param.day.mean, all.x=TRUE)
    #param.flow.dat = param.flow.dat[!is.na(param.flow.dat$Observed_Loading)]
    param.flow.dat$TMDL <- (param.flow.dat$Flow.Value*crit*cf)*(1-mos)
    # units = sub("\\/.*", "", param.day.mean$Parameter.Unit[1])
    param.flow.dat$Units = units
    #param.flow.dat$Loading_Capacity_MOS <- param.flow.dat$Loading_Capacity*(1-mos)
    param.flow.dat$Observed_Loading <- param.flow.dat$Flow.Value*param.flow.dat$Parameter.Value_Mean*cf
    param.flow.dat$Exceeds <- ifelse(param.flow.dat$Observed_Loading>param.flow.dat$TMDL,"yes","no")
    
    # Flow Percentile calc function
    ldc_func <- function(x){
      x$Flow_Percentile = flow_perc(x$Flow.Value)
      out = x
      return(out)
    }
    
    # Apply percentile calc to data
    ldc.dat <- plyr::ddply(.data=param.flow.dat, .(Monitoring.Location.ID), .fun=ldc_func)
    
    # Add to list
    calcs$LDC_Data <- ldc.dat

    # Continue forward with loadings only for records with Result.Value concentrations
    param.ldc <- ldc.dat[!is.na(ldc.dat$Parameter.Value_Mean),]
    
    ## Loading by month ##
    param.ldc$month <- lubridate::month(param.ldc$Activity.Start.Date, label=TRUE)
    ol_mo <- aggregate(Observed_Loading~month+Monitoring.Location.ID, dat=param.ldc, FUN=aggFun)
    tmdl_mo <- aggregate(TMDL~month+Monitoring.Location.ID, dat=param.ldc, FUN=aggFun)
    n_mo <- aggregate(TMDL~month+Monitoring.Location.ID, dat = param.ldc, FUN=length)
    names(n_mo)[names(n_mo)=="TMDL"]<- "Ncount_mo_L"
    mo_load <- merge(ol_mo,tmdl_mo, all=TRUE)
    mo_load <- merge(mo_load, n_mo, all=TRUE)
    mo_load <- mo_load[order(mo_load$month),]
    mo_load$Percent_Reduction_L <- ifelse(mo_load$Observed_Loading>mo_load$TMDL,round(perc.red(mo_load$TMDL,mo_load$Observed_Loading), digits=0),0)
    
    ## Loading by rec season ##
    param.ldc$Year = year(param.ldc$Activity.Start.Date)
    ol_rec <- aggregate(Observed_Loading~Year+Monitoring.Location.ID+Rec_Season, dat=param.ldc, FUN=aggFun)
    tmdl_rec <- aggregate(TMDL~Year+Monitoring.Location.ID+Rec_Season, dat=param.ldc, FUN=aggFun)
    n_rec <- aggregate(TMDL~Year+Monitoring.Location.ID+Rec_Season, dat = param.ldc, FUN=length)
    names(n_rec)[names(n_rec)=="TMDL"]<- "Ncount_rec_L"
    rec_load <- merge(ol_rec,tmdl_rec, all=TRUE)
    rec_load <- merge(rec_load, n_rec, all= TRUE)
    rec_load$Percent_Reduction_L <- ifelse(rec_load$Observed_Loading>rec_load$TMDL,round(perc.red(rec_load$TMDL,rec_load$Observed_Loading), digits=0),0)
    
    ## Loading by irrigation season ##
    ol_irg <- aggregate(Observed_Loading~Year+Monitoring.Location.ID+Irg_Season, dat=param.ldc, FUN=aggFun)
    tmdl_irg <- aggregate(TMDL~Year+Monitoring.Location.ID+Irg_Season, dat=param.ldc, FUN=aggFun)
    n_irg <- aggregate(TMDL~Year+Monitoring.Location.ID+Irg_Season, dat = param.ldc, FUN=length)
    names(n_irg)[names(n_irg)=="TMDL"]<- "Ncount_irg_L"
    irg_load <- merge(ol_irg,tmdl_irg, all=TRUE)
    irg_load <- merge(irg_load, n_irg)
    irg_load$Percent_Reduction_L <- ifelse(irg_load$Observed_Loading>irg_load$TMDL,round(perc.red(irg_load$TMDL,irg_load$Observed_Loading), digits=0),0)
    
  }else{mo_load = data.frame(Monitoring.Location.ID=unique(param.day.mean$Monitoring.Location.ID))
        rec_load = data.frame(Monitoring.Location.ID=unique(param.day.mean$Monitoring.Location.ID))
        irg_load = data.frame(Monitoring.Location.ID=unique(param.day.mean$Monitoring.Location.ID))}

  ## Concentration by month ##
    param.day.mean$month <- lubridate::month(param.day.mean$Activity.Start.Date, label=TRUE)
    concen_mo <- aggregate(Parameter.Value_Mean~month+Monitoring.Location.ID, dat=param.day.mean, FUN=aggFun)
    concen_mo_n <- aggregate(Parameter.Value_Mean~month+Monitoring.Location.ID, dat=param.day.mean, FUN=length)
    names(concen_mo_n)[names(concen_mo_n)=="Parameter.Value_Mean"] <- "Ncount_mo_C"
    concen_mo = merge(concen_mo, concen_mo_n, all=TRUE)
    concen_mo$Percent_Reduction_C <- ifelse(concen_mo$Parameter.Value_Mean>crit,round(perc.red(crit,concen_mo$Parameter.Value_Mean), digits=0),0)
    
  ## Concentration by rec season ##
    param.day.mean$Year <- lubridate::year(param.day.mean$Activity.Start.Date)
    concen_rec <- aggregate(Parameter.Value_Mean~Rec_Season+Monitoring.Location.ID+Year, dat=param.day.mean, FUN=aggFun)
    concen_rec_n <- aggregate(Parameter.Value_Mean~Rec_Season+Monitoring.Location.ID+Year, dat=param.day.mean, FUN=length)
    names(concen_rec_n)[names(concen_rec_n)=="Parameter.Value_Mean"] <- "Ncount_rec_C"
    concen_rec = merge(concen_rec, concen_rec_n, all=TRUE)
    concen_rec$Percent_Reduction_C <- ifelse(concen_rec$Parameter.Value_Mean>crit,round(perc.red(crit,concen_rec$Parameter.Value_Mean), digits=0),0)

  ## Concentration by irrigation season ##
    concen_irg <- aggregate(Parameter.Value_Mean~Irg_Season+Monitoring.Location.ID+Year, dat=param.day.mean, FUN=aggFun)
    concen_irg_n <- aggregate(Parameter.Value_Mean~Irg_Season+Monitoring.Location.ID+Year, dat=param.day.mean, FUN=length)
    names(concen_irg_n)[names(concen_irg_n)=="Parameter.Value_Mean"] <- "Ncount_irg_C"
    concen_irg = merge(concen_irg, concen_irg_n, all=TRUE)
    concen_irg$Percent_Reduction_C <- ifelse(concen_irg$Parameter.Value_Mean>crit,round(perc.red(crit,concen_irg$Parameter.Value_Mean), digits=0),0)

  # Merge monthly data and write to new datasheet 
    month.dat = merge(concen_mo,mo_load, all=TRUE)
    
    # Add to list
    calcs$Monthly_Data <- month.dat
    
  # Merge rec season data and write to new datasheet
    rec.dat = merge(rec_load,concen_rec, all=TRUE)
    
    # Add to list
    calcs$Rec_Season_Data <- rec.dat
    
  # Merge irrigation season data and write to new datasheet
    irg.dat = merge(irg_load, concen_irg, all=TRUE)
    
    # Add to list
    calcs$Irg_Season_Data <- irg.dat

  ############################ SAVE WORKBOOK FILE WITH NEW SHEETS #########################
  if(exportfromfunc){
    # Create workbooks for each data frame
    # Daily means
    if(!any(wb.dat$sheet_names=="Daily_Mean_Data")){
      addWorksheet(wb.dat, "Daily_Mean_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Daily_Mean_Data", param.day.mean, rowNames = FALSE)}
    
    # Monthly means
    if(!any(wb.dat$sheet_names=="Monthly_Data")){
      addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Monthly_Data", month.dat, rowNames = FALSE)}
    
    # Rec Season means
    if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
      addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Rec_Season_Data", rec.dat, rowNames = FALSE)}
    
    # Irrigation Season means
    if(!any(wb.dat$sheet_names=="Irg_Season_Data")){
      addWorksheet(wb.dat, "Irg_Season_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Irg_Season_Data", irg.dat, rowNames = FALSE)}
    if(flo.dat){
      if(!any(wb.dat$sheet_names=="LDC_Data")){
        addWorksheet(wb.dat, "LDC_Data", gridLines = TRUE)
        writeData(wb.dat, sheet = "LDC_Data", ldc.dat, rowNames = FALSE)}
    }
      
    saveWorkbook(wb.dat, paste0(unlist(strsplit(wb_path,".xlsx")),"_",Sys.Date(),".xlsx"), overwrite = TRUE)}
  
    return(calcs)
  # ############################## PLOTTING USING FUNCTIONS ABOVE ###################
  # if(plot_it){
  #   by(param.day.gmean,ecoli.day.gmean[,c("MLID","ML_Name")],plotTimeSeries, max_crit=max_crit, yeslines=TRUE, wndws=FALSE) # time series plots
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
