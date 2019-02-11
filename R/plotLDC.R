#' Plot Load Duration Curve
#'
#' This function creates a plot of observed and loading capacities as a function of flow percentile for a given site.
#' @param x A data frame containing a field labeled "Observed_Loading", "Loading_Capacity", "Loading_Capacity_MOS", "Season", and "Flow_Percentile". Generally used on a site level.
#' @export plotLDC
#' @import colorspace



# LDC plot function - used below if plot_it=TRUE
plotLDC <- function(x){
  # Order flow data
  flow.plot <- x[order(x$Flow_Percentile),]
  # Pull out observed loadings (E.coli data)
  ecoli.loads <- x[!is.na(x$E.coli_Geomean),]
  colpal <- sequential_hcl(4)
  spre <- ecoli.loads[ecoli.loads$Season=="Spring",]
  sume <- ecoli.loads[ecoli.loads$Season=="Summer",]
  fale <- ecoli.loads[ecoli.loads$Season=="Fall",]
  wine <- ecoli.loads[ecoli.loads$Season=="Winter",]
  windows()
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
}
