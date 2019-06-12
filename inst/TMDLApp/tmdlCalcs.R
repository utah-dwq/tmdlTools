#' Prep data for TMDL analysis and plotting
#' 
#' This function takes E.coli concentration and flow data (if applicable) to calculate geomean concentrations and loadings on a daily, monthly, rec-season, and irrigation season basis. Produces outputs that may be fed into plotting functions within the tmdlTools package.
#' @param wb_path A file path to the .xlsx file containing E.coli and flow data, linked by MLID/ML_Name/Date, contained in separate worksheets.
#' @param exportfromfunc Logical. Indicates whether workbook should be exported from tmdlCalcs function, or skipped if using Shiny interface. Default is FALSE to accommodate Shiny use.
#' @return The output includes a new Excel workbook with the name of the original file plus today's date.xlsx, as well as the following dataframes, composed within a list: ecoli concentrations, flow data, ldc data, monthly geomeans, rec/non rec geomeans, and irg/non irg geomeans.
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
# overwrite=FALSE

tmdlCalcs <- function(wb_path, exportfromfunc = FALSE){
  
  calcs <- list()
  
  ## Calculation functions needed for plotting and assessment ## 
  gmean=function(x){exp(mean(log(x)))} # geometric mean
  perc.red <- function(x,y){100-x/y*100} # percent reduction equation where x = capacity and y = observed
  flow_perc <- function(x){(1-percent_rank(x))*100} # gives each flow measurement a percent rank (what percentage of flows are higher than value?)
  
  ### Load the dataset from the workbook ###
  wb.dat <- openxlsx::loadWorkbook(wb_path)
  
  ### Obtain criteria from specs.dat sheet or function inputs ###
  if(!"Inputs"%in%wb.dat$sheet_names){print("Workbook is missing 'Inputs' tab. Please refer to template for required tab contents/format.")}
  specs.dat <- openxlsx::readWorkbook(wb.dat, sheet="Inputs",startRow=1)
  
  # Add to list
  calcs$Inputs <- specs.dat
  
  geom_crit = specs.dat[specs.dat$Parameter=="Geometric Mean Criterion","Value"]
  max_crit = specs.dat[specs.dat$Parameter=="Max Criterion","Value"]
  cf = specs.dat[specs.dat$Parameter=="Correction Factor","Value"]
  mos = specs.dat[specs.dat$Parameter=="Margin of Safety","Value"]
  rec_ssn = c(specs.dat[specs.dat$Parameter=="Rec Season Start","Value"],specs.dat[specs.dat$Parameter=="Rec Season End","Value"])
  irg_ssn = c(specs.dat[specs.dat$Parameter=="Irrigation Season Start","Value"],specs.dat[specs.dat$Parameter=="Irrigation Season End","Value"])
  #rec_ssn = as.Date(c(specs.dat[specs.dat$Parameter=="Rec Season Start","Value"],specs.dat[specs.dat$Parameter=="Rec Season End","Value"]), origin = "1899-12-30")
  #irg_ssn = as.Date(c(specs.dat[specs.dat$Parameter=="Irrigation Season Start","Value"],specs.dat[specs.dat$Parameter=="Irrigation Season End","Value"]), origin = "1899-12-30")
  
  ### Obtain site order from site_order sheet ###
  if("Site_order"%in%wb.dat$sheet_names){
    site.order = readWorkbook(wb.dat, sheet="Site_order", startRow = 1)
    calcs$Site_order = site.order
  }
  
  # Load E.coli concentrations
  ecoli.dat <- openxlsx::readWorkbook(wb.dat,sheet="Ecoli_data",startRow=1)
  ecoli.dat$Date <- as.Date(ecoli.dat$Date, origin="1899-12-30")
  
  # Add to list
  calcs$Ecoli_data <- ecoli.dat
  
  if("Flow_data"%in%wb.dat$sheet_names){
    flow.dat <- openxlsx::readWorkbook(wb.dat, sheet="Flow_data", startRow=1)
    flow.dat$Date <- as.Date(flow.dat$Date, origin="1899-12-30") 
    flo.dat = TRUE
  }else{flo.dat=FALSE}

  ### Convert any "<" to min and max detection limits
  ecoli.dat$E.coli=gsub("<1",1,ecoli.dat$E.coli)
  ecoli.dat$E.coli=gsub(">2419.6",2420,ecoli.dat$E.coli)
  ecoli.dat$E.coli = as.numeric(ecoli.dat$E.coli)
  ecoli.dat = ecoli.dat[!is.na(ecoli.dat$E.coli),]
  
  # Trim white space
  ecoli.dat$ML_Name = trimws(ecoli.dat$ML_Name)
  
  # Take geometric mean over same site/day samples
  if(!length(unique(ecoli.dat$ML_Name))== length(unique(ecoli.dat$MLID))){
    warning("Monitoring location names are not specific to MLID's. Function will calculate separate daily geomeans for each ML_Name/MLID combination")
  }
  #ecoli.day.gmean = ecoli.dat%>%group_by(Date, ML_Name, MLID)%>%summarise(E.coli_Geomean = gmean(E.coli))
  ecoli.day.gmean <- aggregate(E.coli~Date+ML_Name+MLID, data=ecoli.dat, FUN=function(x){exp(mean(log(x)))})
  # test = merge(ecoli.day.gmean, ecoli.day.gmean1, all = TRUE)
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
  #rec_ssn1 = yday(as.Date(rec_ssn, "%m-%d"))
  ecoli.day.gmean$Rec_Season = ifelse(yday(ecoli.day.gmean$Date)>=rec_ssn[1]&yday(ecoli.day.gmean$Date)<=rec_ssn[2],"Rec Season","Not Rec Season")
  
  # Determine if each point falls within the irrigation season
  #irg_ssn1 = yday(as.Date(irg_ssn, "%m-%d"))
  ecoli.day.gmean$Irg_Season = ifelse(yday(ecoli.day.gmean$Date)>=irg_ssn[1]&yday(ecoli.day.gmean$Date)<=irg_ssn[2],"Irrigation Season","Not Irrigation Season")
  
  # Add to list
  calcs$Daily_Geomean_Data <- ecoli.day.gmean
  
  ###### LOAD DURATION CALCULATIONS, MONTHLY LOADS/REC SEASON/IRG SEASON AND PERC REDUCTION #######
  if(flo.dat){
    
    # Add to list
    calcs$Flow_data <- flow.dat
    
    ## Create loading dataset
    ecoli.flow.dat <- merge(flow.dat,ecoli.day.gmean, all.x=TRUE)
    ecoli.flow.dat$TMDL <- ((ecoli.flow.dat$Flow*geom_crit*cf)*(1-mos))/1000000000
    ecoli.flow.dat$Units = "GigaMPN/day"
    #ecoli.flow.dat$Loading_Capacity_MOS <- ecoli.flow.dat$Loading_Capacity*(1-mos)
    ecoli.flow.dat$Observed_Loading <- (ecoli.flow.dat$Flow*ecoli.flow.dat$E.coli_Geomean*cf)/1000000000
    ecoli.flow.dat$Exceeds <- ifelse(ecoli.flow.dat$Observed_Loading>ecoli.flow.dat$TMDL,"yes","no")
    
    # Flow Percentile calc function
    ldc_func <- function(x){
      x$Flow_Percentile = flow_perc(x$Flow)
      out = x
      return(out)
    }
    
    # Apply percentile calc to data
    ldc.dat <- plyr::ddply(.data=ecoli.flow.dat, .(MLID, ML_Name), .fun=ldc_func)
    
    # Add to list
    calcs$LDC_Data <- ldc.dat

    # Continue forward with loadings only for records with E.coli concentrations
    ecoli.ldc <- ldc.dat[!is.na(ldc.dat$E.coli_Geomean),]
    
    ## Loading by month ##
    ecoli.ldc$month <- month(ecoli.ldc$Date, label=TRUE)
    ol_mo <- aggregate(Observed_Loading~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
    tmdl_mo <- aggregate(TMDL~month+MLID+ML_Name, dat=ecoli.ldc, FUN=gmean)
    n_mo <- aggregate(TMDL~month+MLID+ML_Name, dat = ecoli.ldc, FUN=length)
    names(n_mo)[names(n_mo)=="TMDL"]<- "Ncount_mo_L"
    mo_load <- merge(ol_mo,tmdl_mo, all=TRUE)
    mo_load <- merge(mo_load, n_mo, all=TRUE)
    mo_load <- mo_load[order(mo_load$month),]
    mo_load$Percent_Reduction_L <- ifelse(mo_load$Observed_Loading>mo_load$TMDL,round(perc.red(mo_load$TMDL,mo_load$Observed_Loading), digits=0),0)
    
    ## Loading by rec season ##
    ecoli.ldc$Year = year(ecoli.ldc$Date)
    ol_rec <- aggregate(Observed_Loading~Year+MLID+ML_Name+Rec_Season, dat=ecoli.ldc, FUN=gmean)
    tmdl_rec <- aggregate(TMDL~Year+MLID+ML_Name+Rec_Season, dat=ecoli.ldc, FUN=gmean)
    n_rec <- aggregate(TMDL~Year+MLID+ML_Name+Rec_Season, dat = ecoli.ldc, FUN=length)
    names(n_rec)[names(n_rec)=="TMDL"]<- "Ncount_rec_L"
    rec_load <- merge(ol_rec,tmdl_rec, all=TRUE)
    rec_load <- merge(rec_load, n_rec, all= TRUE)
    rec_load$Percent_Reduction_L <- ifelse(rec_load$Observed_Loading>rec_load$TMDL,round(perc.red(rec_load$TMDL,rec_load$Observed_Loading), digits=0),0)
    
    ## Loading by irrigation season ##
    ol_irg <- aggregate(Observed_Loading~Year+MLID+ML_Name+Irg_Season, dat=ecoli.ldc, FUN=gmean)
    tmdl_irg <- aggregate(TMDL~Year+MLID+ML_Name+Irg_Season, dat=ecoli.ldc, FUN=gmean)
    n_irg <- aggregate(TMDL~Year+MLID+ML_Name+Irg_Season, dat = ecoli.ldc, FUN=length)
    names(n_irg)[names(n_irg)=="TMDL"]<- "Ncount_irg_L"
    irg_load <- merge(ol_irg,tmdl_irg, all=TRUE)
    irg_load <- merge(irg_load, n_irg)
    irg_load$Percent_Reduction_L <- ifelse(irg_load$Observed_Loading>irg_load$TMDL,round(perc.red(irg_load$TMDL,irg_load$Observed_Loading), digits=0),0)
    
  }else{mo_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))
        rec_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))
        irg_load = data.frame(MLID=unique(ecoli.day.gmean$MLID))}

  ## Concentration by month ##
    ecoli.day.gmean$month <- lubridate::month(ecoli.day.gmean$Date, label=TRUE)
    concen_mo <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=ecoli.day.gmean, FUN=gmean)
    concen_mo_n <- aggregate(E.coli_Geomean~month+MLID+ML_Name, dat=ecoli.day.gmean, FUN=length)
    names(concen_mo_n)[names(concen_mo_n)=="E.coli_Geomean"] <- "Ncount_mo_C"
    concen_mo = merge(concen_mo, concen_mo_n, all=TRUE)
    concen_mo$Percent_Reduction_C <- ifelse(concen_mo$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_mo$E.coli_Geomean), digits=0),0)
    
  ## Concentration by rec season ##
    ecoli.day.gmean$Year <- lubridate::year(ecoli.day.gmean$Date)
    concen_rec <- aggregate(E.coli_Geomean~Rec_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=gmean)
    concen_rec_n <- aggregate(E.coli_Geomean~Rec_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=length)
    names(concen_rec_n)[names(concen_rec_n)=="E.coli_Geomean"] <- "Ncount_rec_C"
    concen_rec = merge(concen_rec, concen_rec_n, all=TRUE)
    concen_rec$Percent_Reduction_C <- ifelse(concen_rec$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_rec$E.coli_Geomean), digits=0),0)

  ## Concentration by irrigation season ##
    concen_irg <- aggregate(E.coli_Geomean~Irg_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=gmean)
    concen_irg_n <- aggregate(E.coli_Geomean~Irg_Season+MLID+ML_Name+Year, dat=ecoli.day.gmean, FUN=length)
    names(concen_irg_n)[names(concen_irg_n)=="E.coli_Geomean"] <- "Ncount_irg_C"
    concen_irg = merge(concen_irg, concen_irg_n, all=TRUE)
    concen_irg$Percent_Reduction_C <- ifelse(concen_irg$E.coli_Geomean>geom_crit,round(perc.red(geom_crit,concen_irg$E.coli_Geomean), digits=0),0)

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
    # Daily geomeans
    if(!any(wb.dat$sheet_names=="Daily_Geomean_Data")){
      addWorksheet(wb.dat, "Daily_Geomean_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Daily_Geomean_Data", ecoli.day.gmean, rowNames = FALSE)}
    
    # Monthly geomeans
    if(!any(wb.dat$sheet_names=="Monthly_Data")){
      addWorksheet(wb.dat, "Monthly_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Monthly_Data", month.dat, rowNames = FALSE)}
    
    # Rec Season geomeans
    if(!any(wb.dat$sheet_names=="Rec_Season_Data")){
      addWorksheet(wb.dat, "Rec_Season_Data", gridLines = TRUE)
      writeData(wb.dat, sheet = "Rec_Season_Data", rec.dat, rowNames = FALSE)}
    
    # Irrigation Season geomeans
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
