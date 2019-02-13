#' Plot Observed Loading and Loading Capacity over the Rec Season by Year
#'
#' This function creates a barplot of observed and loading capacities by rec season for a given site.
#' @param x A data frame containing a field labeled "Observed_Loading", "Loading_Capacity", "Loading_Capacity_MOS", "Season", and "Flow_Percentile". Generally used on a site level.
#' @export plotRecSeasonLoads


plotRecSeasonLoads <- function(x){
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