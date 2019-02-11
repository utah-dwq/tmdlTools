#' Plot Observed Loading and Loading Capacity by Month
#'
#' This function creates a boxplot of observed and loading capacities by month for a given site.
#' @param x A data frame containing a field labeled "Observed_Loading", "Loading_Capacity", "Loading_Capacity_MOS", "Season", and "Flow_Percentile". Generally used on a site level.
#' @export plotMonthlyLoads


plotMonthlyLoads <- function(x){
  rownames(x) <- x$month
  mo_load.p <- x[,names(x)%in%c("Observed_Loading","Loading_Capacity_MOS")]
  windows()
  barp <- barplot(t(mo_load.p), beside=T, main = paste("Monthly E.coli Loading Geomean:",x$ML_Name[1]), ylim=c(0, max(c(mo_load.p$Observed_Loading, mo_load.p$Loading_Capacity_MOS))+0.1*max(c(mo_load.p$Observed_Loading, mo_load.p$Loading_Capacity_MOS))), ylab="E.coli loading MPN/day",col=c("firebrick3","dodgerblue3"))
  legend("topleft",legend=c("Observed Loading","Loading Capacity", "Percent Reduction Needed"), bty="n", fill=c("firebrick3","dodgerblue3","white"), border=c("black","black","white"),cex=0.8)
  box(bty="l")
  barps <- barp[1,]
  barperc <- data.frame(cbind(barps,x$Observed_Loading, x$Percent_Reduction))
  barperc <- barperc[barperc$V3>0,]
  barperc$V3 <- paste(barperc$V3,"%",sep="")
  text(barperc$barps,barperc$V2+0.1*mean(barperc$V2),labels=barperc$V3,cex=0.8)
}

