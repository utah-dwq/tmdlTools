#' Plot Observed Loading and Loading Capacity over the Rec Season by Year
#'
#' This function creates a barplot of observed and loading capacities by rec season for a given site.
#' @param x A data frame containing a field labeled "Observed_Loading", "Loading_Capacity", "Loading_Capacity_MOS", "Season", and "Flow_Percentile". Generally used on a site level.
#' @param wndws Logical. If TRUE, will create plots in individual windows. Default is false for Shiny.
#' @export plotRecSeasonLoads


plotRecSeasonLoads <- function(x, wndws = FALSE){
  rownames(x) <- x$year
  if(wndws){
    windows() 
  }
  rec.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
  barp <- barplot(t(rec.p), beside=T, main = paste("Rec Season E.coli Loading Geomean by Year:",x$ML_Name[1]), ylim=c(0, max(c(rec.p$Observed_Loading, rec.p$Loading_Capacity_MOS))+.1*max(c(rec.p$Observed_Loading, rec.p$Loading_Capacity_MOS))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
  legend("topright",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
  box(bty="l")
  barps <- barp[1,]
  barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
  barperc <- barperc[barperc$V3>0,]
  barperc$V3 <- paste(barperc$V3,"%",sep="")
  text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
  
}